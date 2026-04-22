package chatops4s.slack

import chatops4s.slack.api.{ChannelId, ConversationId, Email, UserId}
import chatops4s.slack.api.socket.{SelectedOption, ViewStateValue, ViewSubmissionPayload}
import chatops4s.slack.api.blocks.*
import io.circe.{Encoder, Json}
import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror
import java.time.{Instant, LocalDate, LocalTime}

opaque type Url = String
object Url {
  def apply(value: String): Url        = value
  extension (u: Url) def value: String = u
}

case class FormFieldDef(
    id: String,
    label: String,
    optional: Boolean,
)

trait FieldCodec[T] {
  def optional: Boolean = false
  def buildElement(actionId: String, initialValue: Option[T]): BlockElement
  def parse(value: ViewStateValue): Either[String, T]
  def isEmpty(value: ViewStateValue): Boolean
}

object FieldCodec {

  given FieldCodec[String] with {
    def buildElement(actionId: String, initialValue: Option[String]): BlockElement =
      PlainTextInputElement(action_id = actionId, initial_value = initialValue)
    def parse(value: ViewStateValue): Either[String, String]                       =
      value.value.toRight("missing value")
    def isEmpty(value: ViewStateValue): Boolean                                    =
      value.value.isEmpty
  }

  given FieldCodec[Int] with {
    def buildElement(actionId: String, initialValue: Option[Int]): BlockElement =
      NumberInputElement(is_decimal_allowed = false, action_id = actionId, initial_value = initialValue.map(_.toString))
    def parse(value: ViewStateValue): Either[String, Int]                       =
      value.value.toRight("missing value").flatMap(s => s.toIntOption.toRight(s"'$s' is not a valid integer"))
    def isEmpty(value: ViewStateValue): Boolean                                 =
      value.value.isEmpty
  }

  given FieldCodec[Long] with {
    def buildElement(actionId: String, initialValue: Option[Long]): BlockElement =
      NumberInputElement(is_decimal_allowed = false, action_id = actionId, initial_value = initialValue.map(_.toString))
    def parse(value: ViewStateValue): Either[String, Long]                       =
      value.value.toRight("missing value").flatMap(s => s.toLongOption.toRight(s"'$s' is not a valid long"))
    def isEmpty(value: ViewStateValue): Boolean                                  =
      value.value.isEmpty
  }

  given FieldCodec[Double] with {
    def buildElement(actionId: String, initialValue: Option[Double]): BlockElement =
      NumberInputElement(is_decimal_allowed = true, action_id = actionId, initial_value = initialValue.map(_.toString))
    def parse(value: ViewStateValue): Either[String, Double]                       =
      value.value.toRight("missing value").flatMap(s => s.toDoubleOption.toRight(s"'$s' is not a valid double"))
    def isEmpty(value: ViewStateValue): Boolean                                    =
      value.value.isEmpty
  }

  given FieldCodec[Float] with {
    def buildElement(actionId: String, initialValue: Option[Float]): BlockElement =
      NumberInputElement(is_decimal_allowed = true, action_id = actionId, initial_value = initialValue.map(_.toString))
    def parse(value: ViewStateValue): Either[String, Float]                       =
      value.value.toRight("missing value").flatMap(s => s.toFloatOption.toRight(s"'$s' is not a valid float"))
    def isEmpty(value: ViewStateValue): Boolean                                   =
      value.value.isEmpty
  }

  given FieldCodec[BigDecimal] with {
    def buildElement(actionId: String, initialValue: Option[BigDecimal]): BlockElement =
      NumberInputElement(is_decimal_allowed = true, action_id = actionId, initial_value = initialValue.map(_.toString))
    def parse(value: ViewStateValue): Either[String, BigDecimal]                       =
      value.value
        .toRight("missing value")
        .flatMap(s =>
          try Right(BigDecimal(s))
          catch { case _: NumberFormatException => Left(s"'$s' is not a valid decimal") },
        )
    def isEmpty(value: ViewStateValue): Boolean                                        =
      value.value.isEmpty
  }

  given FieldCodec[Boolean] with {
    override def optional: Boolean                                                  = true
    private val yesOption                                                           = BlockOption(text = PlainTextObject(text = "Yes"), value = "true")
    def buildElement(actionId: String, initialValue: Option[Boolean]): BlockElement =
      CheckboxesElement(
        action_id = actionId,
        options = List(yesOption),
        initial_options = initialValue.filter(identity).map(_ => List(yesOption)),
      )
    def parse(value: ViewStateValue): Either[String, Boolean]                       =
      Right(value.selected_options.exists(_.nonEmpty))
    def isEmpty(value: ViewStateValue): Boolean                                     =
      value.selected_options.isEmpty
  }

  given FieldCodec[Email] with {
    def buildElement(actionId: String, initialValue: Option[Email]): BlockElement =
      EmailInputElement(action_id = actionId, initial_value = initialValue.map(_.value))
    def parse(value: ViewStateValue): Either[String, Email]                       =
      value.value.toRight("missing value").map(Email(_))
    def isEmpty(value: ViewStateValue): Boolean                                   =
      value.value.isEmpty
  }

  given FieldCodec[Url] with {
    def buildElement(actionId: String, initialValue: Option[Url]): BlockElement =
      UrlInputElement(action_id = actionId, initial_value = initialValue.map(_.value))
    def parse(value: ViewStateValue): Either[String, Url]                       =
      value.value.toRight("missing value").map(Url(_))
    def isEmpty(value: ViewStateValue): Boolean                                 =
      value.value.isEmpty
  }

  given FieldCodec[LocalDate] with {
    def buildElement(actionId: String, initialValue: Option[LocalDate]): BlockElement =
      DatePickerElement(action_id = actionId, initial_date = initialValue.map(_.toString))
    def parse(value: ViewStateValue): Either[String, LocalDate]                       =
      value.selected_date
        .toRight("missing date")
        .flatMap(s =>
          try Right(LocalDate.parse(s))
          catch { case _: Exception => Left(s"'$s' is not a valid date") },
        )
    def isEmpty(value: ViewStateValue): Boolean                                       =
      value.selected_date.isEmpty
  }

  given FieldCodec[LocalTime] with {
    def buildElement(actionId: String, initialValue: Option[LocalTime]): BlockElement =
      TimePickerElement(action_id = actionId, initial_time = initialValue.map(_.toString))
    def parse(value: ViewStateValue): Either[String, LocalTime]                       =
      value.selected_time
        .toRight("missing time")
        .flatMap(s =>
          try Right(LocalTime.parse(s))
          catch { case _: Exception => Left(s"'$s' is not a valid time") },
        )
    def isEmpty(value: ViewStateValue): Boolean                                       =
      value.selected_time.isEmpty
  }

  given FieldCodec[Instant] with {
    def buildElement(actionId: String, initialValue: Option[Instant]): BlockElement =
      DatetimePickerElement(action_id = actionId, initial_date_time = initialValue.map(_.getEpochSecond.toInt))
    def parse(value: ViewStateValue): Either[String, Instant]                       =
      value.selected_date_time.toRight("missing datetime").map(epoch => Instant.ofEpochSecond(epoch.toLong))
    def isEmpty(value: ViewStateValue): Boolean                                     =
      value.selected_date_time.isEmpty
  }

  given FieldCodec[UserId] with {
    def buildElement(actionId: String, initialValue: Option[UserId]): BlockElement =
      UsersSelectElement(action_id = actionId, initial_user = initialValue.map(_.value))
    def parse(value: ViewStateValue): Either[String, UserId]                       =
      value.selected_user.toRight("missing user")
    def isEmpty(value: ViewStateValue): Boolean                                    =
      value.selected_user.isEmpty
  }

  given multiUserIdCodec: FieldCodec[List[UserId]] with {
    def buildElement(actionId: String, initialValue: Option[List[UserId]]): BlockElement =
      MultiUsersSelectElement(action_id = actionId, initial_users = initialValue.filter(_.nonEmpty).map(_.map(_.value)))
    def parse(value: ViewStateValue): Either[String, List[UserId]]                       =
      Right(value.selected_users.getOrElse(Nil))
    def isEmpty(value: ViewStateValue): Boolean                                          =
      value.selected_users.forall(_.isEmpty)
  }

  given FieldCodec[ChannelId] with {
    def buildElement(actionId: String, initialValue: Option[ChannelId]): BlockElement =
      ChannelsSelectElement(action_id = actionId, initial_channel = initialValue.map(_.value))
    def parse(value: ViewStateValue): Either[String, ChannelId]                       =
      value.selected_channel.toRight("missing channel").map(ChannelId(_))
    def isEmpty(value: ViewStateValue): Boolean                                       =
      value.selected_channel.isEmpty
  }

  given multiChannelIdCodec: FieldCodec[List[ChannelId]] with {
    def buildElement(actionId: String, initialValue: Option[List[ChannelId]]): BlockElement =
      MultiChannelsSelectElement(action_id = actionId, initial_channels = initialValue.filter(_.nonEmpty).map(_.map(_.value)))
    def parse(value: ViewStateValue): Either[String, List[ChannelId]]                       =
      Right(value.selected_channels.getOrElse(Nil).map(ChannelId(_)))
    def isEmpty(value: ViewStateValue): Boolean                                             =
      value.selected_channels.forall(_.isEmpty)
  }

  given FieldCodec[ConversationId] with {
    def buildElement(actionId: String, initialValue: Option[ConversationId]): BlockElement =
      ConversationsSelectElement(action_id = actionId, initial_conversation = initialValue.map(_.value))
    def parse(value: ViewStateValue): Either[String, ConversationId]                       =
      value.selected_conversation.toRight("missing conversation").map(ConversationId(_))
    def isEmpty(value: ViewStateValue): Boolean                                            =
      value.selected_conversation.isEmpty
  }

  given multiConversationIdCodec: FieldCodec[List[ConversationId]] with {
    def buildElement(actionId: String, initialValue: Option[List[ConversationId]]): BlockElement =
      MultiConversationsSelectElement(action_id = actionId, initial_conversations = initialValue.filter(_.nonEmpty).map(_.map(_.value)))
    def parse(value: ViewStateValue): Either[String, List[ConversationId]]                       =
      Right(value.selected_conversations.getOrElse(Nil).map(ConversationId(_)))
    def isEmpty(value: ViewStateValue): Boolean                                                  =
      value.selected_conversations.forall(_.isEmpty)
  }

  given FieldCodec[RichTextBlock] with {
    def buildElement(actionId: String, initialValue: Option[RichTextBlock]): BlockElement =
      RichTextInputElement(action_id = actionId, initial_value = initialValue.map(b => Encoder[Block].apply(b)))
    def parse(value: ViewStateValue): Either[String, RichTextBlock]                       =
      value.rich_text_value.toRight("missing rich text value")
    def isEmpty(value: ViewStateValue): Boolean                                           =
      value.rich_text_value.isEmpty
  }

  given fileListCodec: FieldCodec[List[Json]] with {
    def buildElement(actionId: String, initialValue: Option[List[Json]]): BlockElement =
      FileInputElement(action_id = actionId)
    def parse(value: ViewStateValue): Either[String, List[Json]]                       =
      Right(value.files.getOrElse(Nil))
    def isEmpty(value: ViewStateValue): Boolean                                        =
      value.files.forall(_.isEmpty)
  }

  given [T](using inner: FieldCodec[T]): FieldCodec[Option[T]] with {
    override def optional: Boolean                                                    = true
    def buildElement(actionId: String, initialValue: Option[Option[T]]): BlockElement =
      inner.buildElement(actionId, initialValue.flatten)
    def parse(value: ViewStateValue): Either[String, Option[T]]                       =
      if (inner.isEmpty(value)) Right(None) else inner.parse(value).map(Some(_))
    def isEmpty(value: ViewStateValue): Boolean                                       =
      inner.isEmpty(value)
  }

  def staticSelect[T](mappings: List[(BlockOption, T)]): FieldCodec[T] =
    new FieldCodec[T] {
      private val valueMap                                                      = mappings.map((opt, t) => opt.value -> t).toMap
      def buildElement(actionId: String, initialValue: Option[T]): BlockElement =
        StaticSelectElement(
          action_id = actionId,
          options = Some(mappings.map(_._1)),
          initial_option = initialValue.flatMap(v => mappings.find(_._2 == v).map(_._1)),
        )
      def parse(value: ViewStateValue): Either[String, T]                       =
        value.selected_option.toRight("missing selection").flatMap(opt => valueMap.get(opt.value).toRight(s"unknown option: ${opt.value}"))
      def isEmpty(value: ViewStateValue): Boolean                               =
        value.selected_option.isEmpty
    }

  def multiStaticSelect[T](mappings: List[(BlockOption, T)]): FieldCodec[List[T]] =
    new FieldCodec[List[T]] {
      private val valueMap                                                            = mappings.map((opt, t) => opt.value -> t).toMap
      def buildElement(actionId: String, initialValue: Option[List[T]]): BlockElement =
        MultiStaticSelectElement(
          action_id = actionId,
          options = Some(mappings.map(_._1)),
          initial_options = initialValue.map(vs => vs.flatMap(v => mappings.find(_._2 == v).map(_._1))).filter(_.nonEmpty),
        )
      def parse(value: ViewStateValue): Either[String, List[T]]                       =
        value.selected_options.getOrElse(Nil).traverse(opt => valueMap.get(opt.value).toRight(s"unknown option: ${opt.value}"))
      def isEmpty(value: ViewStateValue): Boolean                                     =
        value.selected_options.forall(_.isEmpty)
    }

  def externalSelect(minQueryLength: Option[Int] = None): FieldCodec[String] =
    new FieldCodec[String] {
      def buildElement(actionId: String, initialValue: Option[String]): BlockElement =
        ExternalSelectElement(action_id = actionId, min_query_length = minQueryLength)
      def parse(value: ViewStateValue): Either[String, String]                       =
        value.selected_option.toRight("missing selection").map(_.value)
      def isEmpty(value: ViewStateValue): Boolean                                    =
        value.selected_option.isEmpty
    }

  def multiExternalSelect(minQueryLength: Option[Int] = None): FieldCodec[List[String]] =
    new FieldCodec[List[String]] {
      def buildElement(actionId: String, initialValue: Option[List[String]]): BlockElement =
        MultiExternalSelectElement(action_id = actionId, min_query_length = minQueryLength)
      def parse(value: ViewStateValue): Either[String, List[String]]                       =
        Right(value.selected_options.getOrElse(Nil).map(_.value))
      def isEmpty(value: ViewStateValue): Boolean                                          =
        value.selected_options.forall(_.isEmpty)
    }

  def radioButtons[T](mappings: List[(BlockOption, T)]): FieldCodec[T] =
    new FieldCodec[T] {
      private val valueMap                                                      = mappings.map((opt, t) => opt.value -> t).toMap
      def buildElement(actionId: String, initialValue: Option[T]): BlockElement =
        RadioButtonGroupElement(
          action_id = actionId,
          options = mappings.map(_._1),
          initial_option = initialValue.flatMap(v => mappings.find(_._2 == v).map(_._1)),
        )
      def parse(value: ViewStateValue): Either[String, T]                       =
        value.selected_option.toRight("missing selection").flatMap(opt => valueMap.get(opt.value).toRight(s"unknown option: ${opt.value}"))
      def isEmpty(value: ViewStateValue): Boolean                               =
        value.selected_option.isEmpty
    }

  def checkboxes[T](mappings: List[(BlockOption, T)]): FieldCodec[List[T]] =
    new FieldCodec[List[T]] {
      private val valueMap                                                            = mappings.map((opt, t) => opt.value -> t).toMap
      def buildElement(actionId: String, initialValue: Option[List[T]]): BlockElement =
        CheckboxesElement(
          action_id = actionId,
          options = mappings.map(_._1),
          initial_options = initialValue.map(vs => vs.flatMap(v => mappings.find(_._2 == v).map(_._1))).filter(_.nonEmpty),
        )
      def parse(value: ViewStateValue): Either[String, List[T]]                       =
        value.selected_options.getOrElse(Nil).traverse(opt => valueMap.get(opt.value).toRight(s"unknown option: ${opt.value}"))
      def isEmpty(value: ViewStateValue): Boolean                                     =
        value.selected_options.forall(_.isEmpty)
    }

  extension (list: List[SelectedOption]) {
    private[FieldCodec] def traverse[B](f: SelectedOption => Either[String, B]): Either[String, List[B]] =
      list.foldRight(Right(Nil): Either[String, List[B]]) { (elem, acc) =>
        for { a <- acc; e <- f(elem) } yield e :: a
      }
  }
}

trait FormDef[T] {
  def fields: List[FormFieldDef]
  def parse(values: Map[String, Map[String, ViewStateValue]]): Either[String, T]
  def buildBlocks(initialValues: Map[String, Any]): List[Block]
}

object FormDef {

  @scala.annotation.nowarn("msg=New anonymous class definition will be duplicated at each inline site")
  inline def derived[T](using m: Mirror.ProductOf[T]): FormDef[T] = {
    val fieldsAndCodecs = buildFieldsAndCodecs[m.MirroredElemTypes, m.MirroredElemLabels]

    new FormDef[T] {
      def fields: List[FormFieldDef] = fieldsAndCodecs.map(_._1)

      def parse(values: Map[String, Map[String, ViewStateValue]]): Either[String, T] = {
        val results = fieldsAndCodecs.map { case (fieldDef, parseFn, _) =>
          val fieldValues  = values.getOrElse(fieldDef.id, Map.empty)
          val elementValue = fieldValues.values.headOption.getOrElse(ViewStateValue())
          parseFn(elementValue)
        }
        val errors  = results.collect { case Left(e) => e }
        if (errors.nonEmpty) Left(errors.mkString(", "))
        else {
          val tuple = Tuple.fromArray(results.map(_.toOption.get).toArray)
          Right(m.fromTuple(tuple.asInstanceOf[m.MirroredElemTypes]))
        }
      }

      def buildBlocks(initialValues: Map[String, Any]): List[Block] =
        fieldsAndCodecs.map { case (fieldDef, _, buildElementFn) =>
          val iv      = initialValues.get(fieldDef.id)
          val element = buildElementFn(fieldDef.id, iv)
          InputBlock(
            block_id = Some(fieldDef.id),
            label = PlainTextObject(text = fieldDef.label),
            element = element,
            optional = if (fieldDef.optional) Some(true) else None,
          )
        }
    }
  }

  private inline def buildFieldsAndCodecs[Types <: Tuple, Labels <: Tuple]
      : List[(FormFieldDef, ViewStateValue => Either[String, Any], (String, Option[Any]) => BlockElement)] =
    inline (erasedValue[Types], erasedValue[Labels]) match {
      case (_: EmptyTuple, _)           => Nil
      case (_: (t *: ts), _: (l *: ls)) =>
        val codec                                                 = summonInline[FieldCodec[t]]
        val label                                                 = constValue[l].asInstanceOf[String]
        val humanLabel                                            = label.replaceAll("([A-Z])", " $1").trim.capitalize
        val fieldDef                                              = FormFieldDef(
          id = label,
          label = humanLabel,
          optional = codec.optional,
        )
        val parser: ViewStateValue => Either[String, Any]         = value => codec.parse(value).left.map(e => s"$label: $e")
        val buildElementFn: (String, Option[Any]) => BlockElement = (actionId, iv) => codec.buildElement(actionId, iv.asInstanceOf[Option[t]])
        (fieldDef, parser, buildElementFn) :: buildFieldsAndCodecs[ts, ls]
    }
}

trait MetadataCodec[M] {
  def encode(value: M): String
  def decode(raw: String): M
}

object MetadataCodec {
  given string: MetadataCodec[String] with {
    def encode(value: String): String = value
    def decode(raw: String): String   = raw
  }

  given circe[M](using enc: io.circe.Encoder[M], dec: io.circe.Decoder[M]): MetadataCodec[M] with {
    def encode(value: M): String = enc(value).noSpaces
    def decode(raw: String): M   = io.circe.parser.decode[M](raw)(using dec) match {
      case Right(m)  => m
      case Left(err) => throw new RuntimeException(s"Failed to decode form metadata: ${err.getMessage}")
    }
  }
}

case class FormId[T, M](value: String)

case class FormSubmission[T, M](payload: ViewSubmissionPayload, values: T, metadata: M) {
  def userId: UserId = payload.user.id
}
