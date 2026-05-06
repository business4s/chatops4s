package chatops4s.slack

import chatops4s.slack.api.{ChannelId, Timestamp, conversations}
import io.circe.Json
import sttp.monad.MonadError
import sttp.monad.syntax.*

import java.time.{Duration, Instant}

trait IdempotencyCheck[F[_]] {
  def findExisting(client: SlackClient[F], channel: String, threadTs: Option[Timestamp], key: IdempotencyKey): F[Option[MessageId]]
  def recordSent(key: IdempotencyKey, messageId: MessageId): F[Unit]
  def requiredBotScopes: Set[String] = Set.empty
}

// Selects the API used by `slackScan` to map a channel name to an ID. The bot must be invited to
// read history anyway, so BotChannelsOnly (`users.conversations`) is sufficient and avoids
// paginating the entire workspace; AllChannels (`conversations.list`) is the fallback for setups
// that pass names for channels the bot has not yet joined.
enum ChannelLookup {
  case BotChannelsOnly
  case AllChannels
}

case class SlackScanSettings(
    scanLimit: Int = 100,
    ttl: Duration = Duration.ofHours(1),
    maxEntries: Int = 1000,
    channelLookup: ChannelLookup = ChannelLookup.BotChannelsOnly,
)

object IdempotencyCheck {

  private val EventType = "chatops4s_idempotency"

  def noCheck[F[_]](using monad: MonadError[F]): IdempotencyCheck[F] =
    new IdempotencyCheck[F] {
      def findExisting(client: SlackClient[F], channel: String, threadTs: Option[Timestamp], key: IdempotencyKey): F[Option[MessageId]] =
        monad.unit(None)
      def recordSent(key: IdempotencyKey, messageId: MessageId): F[Unit]                                                                =
        monad.unit(())
    }

  def inMemory[F[_]](
      ttl: Duration = Duration.ofHours(1),
      maxEntries: Int = 1000,
  )(using monad: MonadError[F]): F[IdempotencyCheck[F]] =
    inMemoryWithClock(ttl, maxEntries, () => Instant.now())

  private[slack] def inMemoryWithClock[F[_]](
      ttl: Duration,
      maxEntries: Int,
      clock: () => Instant,
  )(using monad: MonadError[F]): F[IdempotencyCheck[F]] =
    TtlCache.of[F, IdempotencyKey](ttl, maxEntries, clock).map(new InMemoryIdempotencyCheck[F](_))

  // slackScan is the durable default: it works across restarts and replicas because Slack message
  // metadata is the source of truth. To avoid repeatedly hitting Tier 2/3 rate limits, an in-process
  // L1 cache short-circuits subsequent checks for keys this process already saw (either via
  // `recordSent` or via a positive scan result). Negative results are *never* cached -- another
  // replica may have sent the message after our scan. Channel-name -> ChannelId resolution is also
  // cached for the lifetime of the process (renames preserve the ChannelId).
  def slackScan[F[_]](
      settings: SlackScanSettings = SlackScanSettings(),
  )(using monad: MonadError[F]): F[IdempotencyCheck[F]] =
    slackScanWithClock(settings, () => Instant.now())

  private[slack] def slackScanWithClock[F[_]](
      settings: SlackScanSettings,
      clock: () => Instant,
  )(using monad: MonadError[F]): F[IdempotencyCheck[F]] =
    for {
      keyCache       <- TtlCache.of[F, (ChannelId, String)](settings.ttl, settings.maxEntries, clock)
      channelIdCache <- Ref.of[F, Map[String, ChannelId]](Map.empty)
    } yield new SlackScanIdempotencyCheck[F](settings, keyCache, channelIdCache)

  private[slack] def buildMetadataJson(key: IdempotencyKey): Json =
    Json.obj(
      "event_type"    -> Json.fromString(EventType),
      "event_payload" -> Json.obj(
        "key" -> Json.fromString(key.value),
      ),
    )

  private[slack] def extractKeyFromMetadata(metadata: Json): Option[String] = {
    val cursor = metadata.hcursor
    for {
      eventType <- cursor.downField("event_type").as[String].toOption
      if eventType == EventType
      key       <- cursor.downField("event_payload").downField("key").as[String].toOption
    } yield key
  }

  private case class CacheEntry(messageId: MessageId, insertedAt: Instant)

  // Bounded TTL map: writes sweep expired entries and cap total size to maxEntries.
  private class TtlCache[F[_], K](
      ref: Ref[F, Map[K, CacheEntry]],
      ttl: Duration,
      maxEntries: Int,
      clock: () => Instant,
  )(using monad: MonadError[F]) {

    def get(key: K): F[Option[MessageId]] =
      ref.get.map { entries =>
        val now = clock()
        entries.get(key).collect { case e if !isExpired(e, now) => e.messageId }
      }

    def put(key: K, messageId: MessageId): F[Unit] = {
      val now = clock()
      ref.update { entries =>
        val withNew = entries + (key -> CacheEntry(messageId, now))
        val swept   = withNew.filter { case (_, e) => !isExpired(e, now) }
        if (swept.size > maxEntries) swept.toList.sortBy(_._2.insertedAt).drop(swept.size - maxEntries).toMap
        else swept
      }
    }

    private def isExpired(entry: CacheEntry, now: Instant): Boolean =
      Duration.between(entry.insertedAt, now).compareTo(ttl) > 0
  }

  private object TtlCache {
    def of[F[_], K](ttl: Duration, maxEntries: Int, clock: () => Instant)(using monad: MonadError[F]): F[TtlCache[F, K]] =
      Ref.of[F, Map[K, CacheEntry]](Map.empty).map(new TtlCache[F, K](_, ttl, maxEntries, clock))
  }

  private class InMemoryIdempotencyCheck[F[_]](cache: TtlCache[F, IdempotencyKey]) extends IdempotencyCheck[F] {

    def findExisting(client: SlackClient[F], channel: String, threadTs: Option[Timestamp], key: IdempotencyKey): F[Option[MessageId]] =
      cache.get(key)

    def recordSent(key: IdempotencyKey, messageId: MessageId): F[Unit] =
      cache.put(key, messageId)
  }

  private class SlackScanIdempotencyCheck[F[_]](
      settings: SlackScanSettings,
      keyCache: TtlCache[F, (ChannelId, String)],
      channelIdCache: Ref[F, Map[String, ChannelId]],
  )(using monad: MonadError[F])
      extends IdempotencyCheck[F] {

    override val requiredBotScopes: Set[String] = {
      val historyScopes = Set("channels:history", "groups:history", "mpim:history", "im:history")
      val lookupScopes  = settings.channelLookup match {
        case ChannelLookup.BotChannelsOnly => Set.empty[String]
        case ChannelLookup.AllChannels     => Set("channels:read", "groups:read")
      }
      historyScopes ++ lookupScopes
    }

    def findExisting(client: SlackClient[F], channel: String, threadTs: Option[Timestamp], key: IdempotencyKey): F[Option[MessageId]] =
      resolveChannelId(client, channel).flatMap {
        case None            => monad.unit(None)
        case Some(channelId) =>
          keyCache.get((channelId, key.value)).flatMap {
            case Some(found) => monad.unit(Some(found))
            case None        =>
              scanSlack(client, channelId, threadTs, key).flatMap {
                case Some(found) => keyCache.put((channelId, key.value), found).map(_ => Some(found))
                case None        => monad.unit(None)
              }
          }
      }

    def recordSent(key: IdempotencyKey, messageId: MessageId): F[Unit] =
      keyCache.put((messageId.channel, key.value), messageId)

    private def scanSlack(
        client: SlackClient[F],
        channelId: ChannelId,
        threadTs: Option[Timestamp],
        key: IdempotencyKey,
    ): F[Option[MessageId]] = {
      val messagesF = threadTs match {
        case Some(ts) => client.fetchThreadReplies(channelId, ts, settings.scanLimit)
        case None     => client.fetchRecentMessages(channelId, settings.scanLimit)
      }
      messagesF.map { messages =>
        messages.collectFirst {
          case msg if msg.ts.isDefined && msg.metadata.exists(m => extractKeyFromMetadata(m).contains(key.value)) =>
            MessageId(channelId, msg.ts.get)
        }
      }
    }

    private def resolveChannelId(client: SlackClient[F], channel: String): F[Option[ChannelId]] =
      if (ChannelId.looksLikeId(channel)) monad.unit(Some(ChannelId(channel)))
      else {
        val needle = channel.stripPrefix("#")
        channelIdCache.get.flatMap {
          _.get(needle) match {
            case Some(id) => monad.unit(Some(id))
            case None     => client.listChannels(settings.channelLookup).flatMap(consumePages(_, needle))
          }
        }
      }

    // Slack ignores `exclude_archived` for some workspaces and still returns archived channels, so
    // we filter client-side. Every non-archived channel we observe is cached -- the API call was
    // already paid for, and Tier 2/3 list endpoints are far more expensive than the memory used.
    private def consumePages(page: ConversationsPage[F], needle: String): F[Option[ChannelId]] = {
      val active = page.channels.filterNot(_.is_archived.contains(true))
      cachePage(active).flatMap { _ =>
        active.find(_.name.contains(needle)).map(_.id) match {
          case Some(id) => monad.unit(Some(id))
          case None     => page.next.fold(monad.unit(Option.empty[ChannelId]))(_.flatMap(consumePages(_, needle)))
        }
      }
    }

    private def cachePage(channels: List[conversations.ConversationInfo]): F[Unit] = {
      val pairs = channels.flatMap(c => c.name.map(_ -> c.id))
      if (pairs.isEmpty) monad.unit(())
      else channelIdCache.update(_ ++ pairs)
    }
  }
}
