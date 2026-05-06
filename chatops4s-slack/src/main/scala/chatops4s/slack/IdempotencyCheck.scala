package chatops4s.slack

import chatops4s.slack.api.{ChannelId, Timestamp, conversations}
import io.circe.Json
import sttp.monad.MonadError
import sttp.monad.syntax.*

import java.time.{Duration, Instant}

trait IdempotencyCheck[F[_]] {
  // The gateway hands its connected client to each call. Implementations that don't need it
  // (in-memory caches, no-ops) can ignore the parameter.
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
    Ref.of[F, Map[IdempotencyKey, CacheEntry]](Map.empty).map { ref =>
      new InMemoryIdempotencyCheck[F](ref, ttl, maxEntries, clock)
    }

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
      keyCache       <- Ref.of[F, Map[(ChannelId, String), CacheEntry]](Map.empty)
      channelIdCache <- Ref.of[F, Map[String, ChannelId]](Map.empty)
    } yield new SlackScanIdempotencyCheck[F](settings, keyCache, channelIdCache, clock)

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

  private class InMemoryIdempotencyCheck[F[_]](
      ref: Ref[F, Map[IdempotencyKey, CacheEntry]],
      ttl: Duration,
      maxEntries: Int,
      clock: () => Instant,
  )(using monad: MonadError[F])
      extends IdempotencyCheck[F] {

    def findExisting(client: SlackClient[F], channel: String, threadTs: Option[Timestamp], key: IdempotencyKey): F[Option[MessageId]] =
      ref.get.map { entries =>
        entries.get(key).collect {
          case entry if !isExpired(entry) => entry.messageId
        }
      }

    def recordSent(key: IdempotencyKey, messageId: MessageId): F[Unit] = {
      val now = clock()
      ref.update { entries =>
        val withNew = entries + (key -> CacheEntry(messageId, now))
        val swept   = withNew.filter { case (_, entry) => !isExpired(entry, now) }
        if (swept.size > maxEntries) {
          swept.toList.sortBy(_._2.insertedAt).drop(swept.size - maxEntries).toMap
        } else swept
      }
    }

    private def isExpired(entry: CacheEntry): Boolean =
      isExpired(entry, clock())

    private def isExpired(entry: CacheEntry, now: Instant): Boolean =
      Duration.between(entry.insertedAt, now).compareTo(ttl) > 0
  }

  private class SlackScanIdempotencyCheck[F[_]](
      settings: SlackScanSettings,
      keyCache: Ref[F, Map[(ChannelId, String), CacheEntry]],
      channelIdCache: Ref[F, Map[String, ChannelId]],
      clock: () => Instant,
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
          checkLocal(channelId, key).flatMap {
            case Some(found) => monad.unit(Some(found))
            case None        =>
              scanSlack(client, channelId, threadTs, key).flatMap {
                case Some(found) => storeLocal(channelId, key.value, found).map(_ => Some(found))
                case None        => monad.unit(None)
              }
          }
      }

    def recordSent(key: IdempotencyKey, messageId: MessageId): F[Unit] =
      storeLocal(messageId.channel, key.value, messageId)

    private def checkLocal(channelId: ChannelId, key: IdempotencyKey): F[Option[MessageId]] =
      keyCache.get.map { entries =>
        val now = clock()
        entries.get((channelId, key.value)).collect {
          case e if !isExpired(e, now) => e.messageId
        }
      }

    private def storeLocal(channelId: ChannelId, key: String, messageId: MessageId): F[Unit] = {
      val now = clock()
      keyCache.update { entries =>
        val withNew = entries + ((channelId, key) -> CacheEntry(messageId, now))
        val swept   = withNew.filter { case (_, e) => !isExpired(e, now) }
        if (swept.size > settings.maxEntries)
          swept.toList.sortBy(_._2.insertedAt).drop(swept.size - settings.maxEntries).toMap
        else swept
      }
    }

    private def isExpired(entry: CacheEntry, now: Instant): Boolean =
      Duration.between(entry.insertedAt, now).compareTo(settings.ttl) > 0

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
      if (looksLikeChannelId(channel)) monad.unit(Some(ChannelId(channel)))
      else {
        val needle = channel.stripPrefix("#")
        channelIdCache.get.flatMap { cached =>
          cached.get(needle).fold(client.listChannels(settings.channelLookup).flatMap(consumePages(_, needle)))(id => monad.unit(Some(id)))
        }
      }

    // Slack ignores `exclude_archived` for some workspaces and still returns archived channels, so
    // we filter client-side. Every non-archived channel we observe is cached -- the cost was already
    // paid for the page.
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

    private def looksLikeChannelId(s: String): Boolean =
      s.nonEmpty && (s.head == 'C' || s.head == 'G' || s.head == 'D') && s.forall(c => c.isUpper || c.isDigit)
  }
}
