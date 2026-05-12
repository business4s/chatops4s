package chatops4s.slack.api

import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json, JsonObject}
import scala.reflect.ClassTag

object blocks {

  // --- Enums ---

  enum ButtonStyle   {
    case Primary, Danger
  }
  object ButtonStyle {
    private val mapping        = Map("primary" -> Primary, "danger" -> Danger)
    private val reverse        = mapping.map(_.swap)
    given Encoder[ButtonStyle] = Encoder[String].contramap(reverse)
    given Decoder[ButtonStyle] = Decoder[String].emap(s => mapping.get(s).toRight(s"Unknown button style: $s"))
  }

  enum TriggerAction   {
    case OnEnterPressed, OnCharacterEntered
  }
  object TriggerAction {
    private val mapping          = Map("on_enter_pressed" -> OnEnterPressed, "on_character_entered" -> OnCharacterEntered)
    private val reverse          = mapping.map(_.swap)
    given Encoder[TriggerAction] = Encoder[String].contramap(reverse)
    given Decoder[TriggerAction] = Decoder[String].emap(s => mapping.get(s).toRight(s"Unknown trigger action: $s"))
  }

  enum ViewType   {
    case Modal, Home
  }
  object ViewType {
    private val mapping     = Map("modal" -> Modal, "home" -> Home)
    private val reverse     = mapping.map(_.swap)
    given Encoder[ViewType] = Encoder[String].contramap(reverse)
    given Decoder[ViewType] = Decoder[String].emap(s => mapping.get(s).toRight(s"Unknown view type: $s"))
  }

  enum TableCellAlign   {
    case Left, Center, Right
  }
  object TableCellAlign {
    private val mapping           = Map("left" -> Left, "center" -> Center, "right" -> Right)
    private val reverse           = mapping.map(_.swap)
    given Encoder[TableCellAlign] = Encoder[String].contramap(reverse)
    given Decoder[TableCellAlign] = Decoder[String].emap(s => mapping.get(s).toRight(s"Unknown cell alignment: $s"))
  }

  // --- TypeEntry helper ---

  private class TypeEntry[A](
      val typeName: String,
      val decoder: Decoder[A],
      val encoder: Encoder.AsObject[A],
      val ct: ClassTag[A],
  )
  private object TypeEntry {
    def apply[A: Decoder: Encoder.AsObject: ClassTag](typeName: String): TypeEntry[A] =
      new TypeEntry(typeName, summon, summon, summon)
  }

  // --- Composition objects ---
  // Defined first as blocks and elements reference them.

  // https://docs.slack.dev/reference/block-kit/composition-objects/text-object
  sealed trait ContextBlockElement

  sealed trait TextObject extends ContextBlockElement {
    def text: String
  }

  // https://docs.slack.dev/reference/block-kit/composition-objects/text-object
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/composition/PlainTextObject.java
  case class PlainTextObject(
      text: String,
      emoji: Option[Boolean] = None,
  ) extends TextObject derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/composition-objects/text-object
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/composition/MarkdownTextObject.java
  case class MarkdownTextObject(
      text: String,
      verbatim: Option[Boolean] = None,
  ) extends TextObject derives Codec.AsObject

  given Encoder.AsObject[TextObject] = Encoder.AsObject.instance {
    case p: PlainTextObject    =>
      ("type" -> Json.fromString("plain_text")) +: Encoder.AsObject[PlainTextObject].encodeObject(p)
    case m: MarkdownTextObject =>
      ("type" -> Json.fromString("mrkdwn")) +: Encoder.AsObject[MarkdownTextObject].encodeObject(m)
  }

  given Decoder[TextObject] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap {
      case "plain_text" => cursor.as[PlainTextObject]
      case "mrkdwn"     => cursor.as[MarkdownTextObject]
      case other        => Left(DecodingFailure(s"Unknown text type: $other", cursor.history))
    }
  }

  // https://docs.slack.dev/reference/block-kit/composition-objects/option-object
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/composition/OptionObject.java
  case class BlockOption(
      text: TextObject,
      value: String,
      description: Option[TextObject] = None,
      url: Option[String] = None,
  ) derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/composition-objects/confirmation-dialog-object
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/composition/ConfirmationDialogObject.java
  case class ConfirmationDialogObject(
      title: TextObject,
      text: TextObject,
      confirm: TextObject,
      deny: TextObject,
      style: Option[ButtonStyle] = None,
  ) derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/composition-objects/dispatch-action-configuration-object
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/composition/DispatchActionConfig.java
  case class DispatchActionConfig(
      trigger_actions_on: Option[List[TriggerAction]] = None,
  ) derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/composition-objects/option-group-object
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/composition/OptionGroupObject.java
  case class OptionGroupObject(
      label: TextObject,
      options: List[BlockOption],
  ) derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/composition-objects/slack-file-object
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/composition/SlackFileObject.java
  case class SlackFileObject(
      id: Option[String] = None,
      url: Option[String] = None,
  ) derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/composition-objects/conversation-filter-object
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/ConversationsSelectElement.java
  case class ConversationsFilter(
      include: Option[List[String]] = None,
      exclude_external_shared_channels: Option[Boolean] = None,
      exclude_bot_users: Option[Boolean] = None,
  ) derives Codec.AsObject

  // --- Elements ---

  sealed trait BlockElement

  // https://docs.slack.dev/reference/block-kit/block-elements/button-element
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/ButtonElement.java
  case class ButtonElement(
      text: TextObject,
      action_id: String,
      value: Option[NonEmptyString] = None,
      url: Option[String] = None,
      style: Option[ButtonStyle] = None,
      confirm: Option[ConfirmationDialogObject] = None,
      accessibility_label: Option[String] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/plain-text-input-element
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/PlainTextInputElement.java
  case class PlainTextInputElement(
      action_id: String,
      initial_value: Option[String] = None,
      multiline: Option[Boolean] = None,
      min_length: Option[Int] = None,
      max_length: Option[Int] = None,
      dispatch_action_config: Option[DispatchActionConfig] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/number-input-element
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/NumberInputElement.java
  case class NumberInputElement(
      is_decimal_allowed: Boolean,
      action_id: String,
      initial_value: Option[String] = None,
      min_value: Option[String] = None,
      max_value: Option[String] = None,
      dispatch_action_config: Option[DispatchActionConfig] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/checkboxes-element
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/CheckboxesElement.java
  case class CheckboxesElement(
      options: List[BlockOption],
      action_id: String,
      initial_options: Option[List[BlockOption]] = None,
      confirm: Option[ConfirmationDialogObject] = None,
      focus_on_load: Option[Boolean] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/radio-button-group-element
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/RadioButtonsElement.java
  case class RadioButtonGroupElement(
      options: List[BlockOption],
      action_id: String,
      initial_option: Option[BlockOption] = None,
      confirm: Option[ConfirmationDialogObject] = None,
      focus_on_load: Option[Boolean] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/select-menu-element
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/StaticSelectElement.java
  case class StaticSelectElement(
      action_id: String,
      options: Option[List[BlockOption]] = None,
      option_groups: Option[List[OptionGroupObject]] = None,
      initial_option: Option[BlockOption] = None,
      confirm: Option[ConfirmationDialogObject] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/multi-select-menu-element
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/MultiStaticSelectElement.java
  case class MultiStaticSelectElement(
      action_id: String,
      options: Option[List[BlockOption]] = None,
      option_groups: Option[List[OptionGroupObject]] = None,
      initial_options: Option[List[BlockOption]] = None,
      confirm: Option[ConfirmationDialogObject] = None,
      max_selected_items: Option[Int] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/overflow-menu-element
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/OverflowMenuElement.java
  case class OverflowMenuElement(
      options: List[BlockOption],
      action_id: String,
      confirm: Option[ConfirmationDialogObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/date-picker-element
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/DatePickerElement.java
  case class DatePickerElement(
      action_id: String,
      initial_date: Option[String] = None,
      confirm: Option[ConfirmationDialogObject] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/time-picker-element
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/TimePickerElement.java
  case class TimePickerElement(
      action_id: String,
      initial_time: Option[String] = None,
      confirm: Option[ConfirmationDialogObject] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
      timezone: Option[String] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/datetime-picker-element
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/DatetimePickerElement.java
  case class DatetimePickerElement(
      action_id: String,
      initial_date_time: Option[Int] = None,
      confirm: Option[ConfirmationDialogObject] = None,
      focus_on_load: Option[Boolean] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/email-input-element
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/EmailTextInputElement.java
  case class EmailInputElement(
      action_id: String,
      initial_value: Option[String] = None,
      dispatch_action_config: Option[DispatchActionConfig] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/url-input-element
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/URLTextInputElement.java
  case class UrlInputElement(
      action_id: String,
      initial_value: Option[String] = None,
      dispatch_action_config: Option[DispatchActionConfig] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/image-element
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/ImageElement.java
  case class ImageElement(
      alt_text: String,
      image_url: Option[String] = None,
      slack_file: Option[SlackFileObject] = None,
      fallback: Option[String] = None,
      image_width: Option[Int] = None,
      image_height: Option[Int] = None,
      image_bytes: Option[Int] = None,
  ) extends BlockElement
      with ContextBlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/select-menu-element#conversations_select
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/ConversationsSelectElement.java
  case class ConversationsSelectElement(
      action_id: String,
      initial_conversation: Option[String] = None,
      default_to_current_conversation: Option[Boolean] = None,
      confirm: Option[ConfirmationDialogObject] = None,
      response_url_enabled: Option[Boolean] = None,
      filter: Option[ConversationsFilter] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/select-menu-element#channels_select
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/ChannelsSelectElement.java
  case class ChannelsSelectElement(
      action_id: String,
      initial_channel: Option[String] = None,
      confirm: Option[ConfirmationDialogObject] = None,
      response_url_enabled: Option[Boolean] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/select-menu-element#users_select
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/UsersSelectElement.java
  case class UsersSelectElement(
      action_id: String,
      initial_user: Option[String] = None,
      confirm: Option[ConfirmationDialogObject] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/select-menu-element#external_select
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/ExternalSelectElement.java
  case class ExternalSelectElement(
      action_id: String,
      initial_option: Option[BlockOption] = None,
      min_query_length: Option[Int] = None,
      confirm: Option[ConfirmationDialogObject] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/multi-select-menu-element#users_multi_select
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/MultiUsersSelectElement.java
  case class MultiUsersSelectElement(
      action_id: String,
      initial_users: Option[List[String]] = None,
      confirm: Option[ConfirmationDialogObject] = None,
      max_selected_items: Option[Int] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/multi-select-menu-element#channel_multi_select
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/MultiChannelsSelectElement.java
  case class MultiChannelsSelectElement(
      action_id: String,
      initial_channels: Option[List[String]] = None,
      confirm: Option[ConfirmationDialogObject] = None,
      max_selected_items: Option[Int] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/multi-select-menu-element#conversation_multi_select
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/MultiConversationsSelectElement.java
  case class MultiConversationsSelectElement(
      action_id: String,
      initial_conversations: Option[List[String]] = None,
      default_to_current_conversation: Option[Boolean] = None,
      confirm: Option[ConfirmationDialogObject] = None,
      max_selected_items: Option[Int] = None,
      filter: Option[ConversationsFilter] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/multi-select-menu-element#external_multi_select
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/MultiExternalSelectElement.java
  case class MultiExternalSelectElement(
      action_id: String,
      initial_options: Option[List[BlockOption]] = None,
      min_query_length: Option[Int] = None,
      confirm: Option[ConfirmationDialogObject] = None,
      max_selected_items: Option[Int] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/rich-text-input-element
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/RichTextInputElement.java
  case class RichTextInputElement(
      action_id: String,
      initial_value: Option[Json] = None,
      dispatch_action_config: Option[DispatchActionConfig] = None,
      focus_on_load: Option[Boolean] = None,
      placeholder: Option[TextObject] = None,
  ) extends BlockElement derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/block-elements/file-input-element
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/FileInputElement.java
  case class FileInputElement(
      action_id: String,
      filetypes: Option[List[String]] = None,
      max_files: Option[Int] = None,
  ) extends BlockElement derives Codec.AsObject

  case class UnknownBlockElement(raw: Json) extends BlockElement

  // --- Element codecs (TypeEntry-based) ---

  private val elementEntries: List[TypeEntry[? <: BlockElement]] = List(
    TypeEntry[ButtonElement]("button"),
    TypeEntry[PlainTextInputElement]("plain_text_input"),
    TypeEntry[NumberInputElement]("number_input"),
    TypeEntry[CheckboxesElement]("checkboxes"),
    TypeEntry[RadioButtonGroupElement]("radio_buttons"),
    TypeEntry[StaticSelectElement]("static_select"),
    TypeEntry[MultiStaticSelectElement]("multi_static_select"),
    TypeEntry[OverflowMenuElement]("overflow"),
    TypeEntry[DatePickerElement]("datepicker"),
    TypeEntry[TimePickerElement]("timepicker"),
    TypeEntry[DatetimePickerElement]("datetimepicker"),
    TypeEntry[EmailInputElement]("email_text_input"),
    TypeEntry[UrlInputElement]("url_text_input"),
    TypeEntry[ImageElement]("image"),
    TypeEntry[ConversationsSelectElement]("conversations_select"),
    TypeEntry[ChannelsSelectElement]("channels_select"),
    TypeEntry[UsersSelectElement]("users_select"),
    TypeEntry[ExternalSelectElement]("external_select"),
    TypeEntry[MultiUsersSelectElement]("multi_users_select"),
    TypeEntry[MultiChannelsSelectElement]("multi_channels_select"),
    TypeEntry[MultiConversationsSelectElement]("multi_conversations_select"),
    TypeEntry[MultiExternalSelectElement]("multi_external_select"),
    TypeEntry[RichTextInputElement]("rich_text_input"),
    TypeEntry[FileInputElement]("file_input"),
  )

  private val elementTypeMap: Map[String, Decoder[BlockElement]] =
    elementEntries.map(e => e.typeName -> e.decoder.map(identity[BlockElement])).toMap

  given Encoder.AsObject[BlockElement] = Encoder.AsObject.instance {
    case e: UnknownBlockElement =>
      e.raw.asObject.getOrElse(JsonObject.empty)
    case elem                   =>
      elementEntries.iterator
        .flatMap { entry =>
          if (entry.ct.runtimeClass.isInstance(elem))
            Some(("type" -> Json.fromString(entry.typeName)) +: entry.encoder.asInstanceOf[Encoder.AsObject[Any]].encodeObject(elem))
          else scala.None
        }
        .next()
  }

  given Decoder[BlockElement] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap { tpe =>
      elementTypeMap.get(tpe) match {
        case Some(decoder) => decoder(cursor)
        case None          => cursor.as[Json].map(UnknownBlockElement(_))
      }
    }
  }

  // --- ContextBlockElement codecs ---

  given Encoder.AsObject[ContextBlockElement] = Encoder.AsObject.instance {
    case p: PlainTextObject    =>
      ("type" -> Json.fromString("plain_text")) +: Encoder.AsObject[PlainTextObject].encodeObject(p)
    case m: MarkdownTextObject =>
      ("type" -> Json.fromString("mrkdwn")) +: Encoder.AsObject[MarkdownTextObject].encodeObject(m)
    case i: ImageElement       =>
      ("type" -> Json.fromString("image")) +: Encoder.AsObject[ImageElement].encodeObject(i)
  }

  given Decoder[ContextBlockElement] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap {
      case "plain_text" => cursor.as[PlainTextObject]
      case "mrkdwn"     => cursor.as[MarkdownTextObject]
      case "image"      => cursor.as[ImageElement]
      case other        => Left(DecodingFailure(s"Unknown context element type: $other", cursor.history))
    }
  }

  // --- Blocks ---

  sealed trait Block

  // https://docs.slack.dev/reference/block-kit/blocks/section-block
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/SectionBlock.java
  case class SectionBlock(
      text: Option[TextObject] = None,
      block_id: Option[String] = None,
      fields: Option[List[TextObject]] = None,
      accessory: Option[BlockElement] = None,
      expand: Option[Boolean] = None,
  ) extends Block derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/blocks/actions-block
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/ActionsBlock.java
  case class ActionsBlock(
      elements: List[BlockElement],
      block_id: Option[String] = None,
  ) extends Block derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/blocks/input-block
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/InputBlock.java
  case class InputBlock(
      label: TextObject,
      element: BlockElement,
      block_id: Option[String] = None,
      optional: Option[Boolean] = None,
      dispatch_action: Option[Boolean] = None,
      hint: Option[TextObject] = None,
  ) extends Block derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/blocks/header-block
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/HeaderBlock.java
  case class HeaderBlock(
      text: TextObject,
      block_id: Option[String] = None,
  ) extends Block derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/blocks/context-block
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/ContextBlock.java
  case class ContextBlock(
      elements: List[ContextBlockElement],
      block_id: Option[String] = None,
  ) extends Block derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/blocks/divider-block
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/DividerBlock.java
  case class DividerBlock(
      block_id: Option[String] = None,
  ) extends Block derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/blocks/image-block
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/ImageBlock.java
  case class ImageBlock(
      alt_text: String,
      image_url: Option[String] = None,
      slack_file: Option[SlackFileObject] = None,
      title: Option[TextObject] = None,
      block_id: Option[String] = None,
      fallback: Option[String] = None,
      image_width: Option[Int] = None,
      image_height: Option[Int] = None,
      image_bytes: Option[Int] = None,
      is_animated: Option[Boolean] = None,
  ) extends Block derives Codec.AsObject

  // --- Rich text element types ---
  // https://docs.slack.dev/reference/block-kit/blocks/rich-text-block
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/RichTextSectionElement.java

  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/RichTextSectionElement.java#TextStyle
  case class RichTextStyle(
      bold: Option[Boolean] = None,
      italic: Option[Boolean] = None,
      strike: Option[Boolean] = None,
      underline: Option[Boolean] = None,
      code: Option[Boolean] = None,
      highlight: Option[Boolean] = None,
      client_highlight: Option[Boolean] = None,
      unlink: Option[Boolean] = None,
  ) derives Codec.AsObject

  sealed trait RichTextElement

  // --- Rich text inline elements ---

  case class RichTextText(
      text: String,
      style: Option[RichTextStyle] = None,
  ) extends RichTextElement derives Codec.AsObject

  case class RichTextLink(
      url: String,
      text: Option[String] = None,
      unsafe: Option[Boolean] = None,
      style: Option[RichTextStyle] = None,
  ) extends RichTextElement derives Codec.AsObject

  case class RichTextEmoji(
      name: String,
      unicode: Option[String] = None,
      skin_tone: Option[Int] = None,
      style: Option[RichTextStyle] = None,
  ) extends RichTextElement derives Codec.AsObject

  case class RichTextUser(
      user_id: String,
      style: Option[RichTextStyle] = None,
  ) extends RichTextElement derives Codec.AsObject

  case class RichTextUserGroup(
      usergroup_id: String,
      style: Option[RichTextStyle] = None,
  ) extends RichTextElement derives Codec.AsObject

  case class RichTextChannel(
      channel_id: String,
      style: Option[RichTextStyle] = None,
  ) extends RichTextElement derives Codec.AsObject

  case class RichTextBroadcast(
      range: String,
      style: Option[RichTextStyle] = None,
  ) extends RichTextElement derives Codec.AsObject

  case class RichTextColor(
      value: String,
      style: Option[RichTextStyle] = None,
  ) extends RichTextElement derives Codec.AsObject

  case class RichTextDate(
      timestamp: Int,
      format: String,
      url: Option[String] = None,
      fallback: Option[String] = None,
      style: Option[RichTextStyle] = None,
  ) extends RichTextElement derives Codec.AsObject

  // --- Rich text container elements ---

  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/RichTextSectionElement.java
  case class RichTextSection(
      elements: List[RichTextElement],
  ) extends RichTextElement derives Codec.AsObject

  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/RichTextListElement.java
  case class RichTextList(
      style: String,
      elements: List[RichTextElement],
      indent: Option[Int] = None,
      offset: Option[Int] = None,
      border: Option[Int] = None,
  ) extends RichTextElement derives Codec.AsObject

  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/RichTextQuoteElement.java
  case class RichTextQuote(
      elements: List[RichTextElement],
      border: Option[Int] = None,
  ) extends RichTextElement derives Codec.AsObject

  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/element/RichTextPreformattedElement.java
  case class RichTextPreformatted(
      elements: List[RichTextElement],
      border: Option[Int] = None,
  ) extends RichTextElement derives Codec.AsObject

  case class UnknownRichTextElement(raw: Json) extends RichTextElement

  // --- Rich text element codecs ---

  private val richTextElementEntries: List[TypeEntry[? <: RichTextElement]] = List(
    TypeEntry[RichTextText]("text"),
    TypeEntry[RichTextLink]("link"),
    TypeEntry[RichTextEmoji]("emoji"),
    TypeEntry[RichTextUser]("user"),
    TypeEntry[RichTextUserGroup]("usergroup"),
    TypeEntry[RichTextChannel]("channel"),
    TypeEntry[RichTextBroadcast]("broadcast"),
    TypeEntry[RichTextColor]("color"),
    TypeEntry[RichTextDate]("date"),
    TypeEntry[RichTextSection]("rich_text_section"),
    TypeEntry[RichTextList]("rich_text_list"),
    TypeEntry[RichTextQuote]("rich_text_quote"),
    TypeEntry[RichTextPreformatted]("rich_text_preformatted"),
  )

  private val richTextElementTypeMap: Map[String, Decoder[RichTextElement]] =
    richTextElementEntries.map(e => e.typeName -> e.decoder.map(identity[RichTextElement])).toMap

  given Encoder.AsObject[RichTextElement] = Encoder.AsObject.instance {
    case e: UnknownRichTextElement =>
      e.raw.asObject.getOrElse(JsonObject.empty)
    case elem                      =>
      richTextElementEntries.iterator
        .flatMap { entry =>
          if (entry.ct.runtimeClass.isInstance(elem))
            Some(("type" -> Json.fromString(entry.typeName)) +: entry.encoder.asInstanceOf[Encoder.AsObject[Any]].encodeObject(elem))
          else scala.None
        }
        .next()
  }

  given Decoder[RichTextElement] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap { tpe =>
      richTextElementTypeMap.get(tpe) match {
        case Some(decoder) => decoder(cursor)
        case scala.None    => cursor.as[Json].map(UnknownRichTextElement(_))
      }
    }
  }

  // https://docs.slack.dev/reference/block-kit/blocks/rich-text-block
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/RichTextBlock.java
  case class RichTextBlock(
      elements: List[RichTextElement],
      block_id: Option[String] = None,
  ) extends Block derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/blocks/file-block
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/FileBlock.java
  case class FileBlock(
      external_id: String,
      source: Option[String] = None,
      block_id: Option[String] = None,
      file_id: Option[String] = None,
      file: Option[Json] = None,
  ) extends Block derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/blocks/video-block
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/block/VideoBlock.java
  case class VideoBlock(
      alt_text: String,
      video_url: String,
      thumbnail_url: String,
      title: TextObject,
      title_url: Option[String] = None,
      description: Option[TextObject] = None,
      author_name: Option[String] = None,
      provider_name: Option[String] = None,
      provider_icon_url: Option[String] = None,
      block_id: Option[String] = None,
  ) extends Block derives Codec.AsObject

  // --- Table cell types ---
  // https://docs.slack.dev/reference/block-kit/blocks/table-block
  // A cell is either a `raw_text` (plain string) or a `rich_text` (same shape as RichTextBlock).

  type TableCell = String | RichTextBlock

  given Encoder[TableCell] = Encoder.instance {
    case s: String        => Json.obj("type" -> Json.fromString("raw_text"), "text" -> Json.fromString(s))
    case r: RichTextBlock => Encoder[Block].apply(r)
  }

  given Decoder[TableCell] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap {
      case "raw_text" => cursor.get[String]("text").map(s => s: TableCell)
      case _          =>
        Decoder[Block].apply(cursor).flatMap {
          case r: RichTextBlock => Right(r: TableCell)
          case other            => Left(DecodingFailure(s"Unsupported table cell block type: ${other.getClass.getSimpleName}", cursor.history))
        }
    }
  }

  case class TableColumnSetting(
      align: Option[TableCellAlign] = None,
      is_wrapped: Option[Boolean] = None,
  ) derives Codec.AsObject

  // https://docs.slack.dev/reference/block-kit/blocks/table-block
  case class TableBlock(
      rows: List[List[TableCell]],
      block_id: Option[String] = None,
      column_settings: Option[List[TableColumnSetting]] = None,
  ) extends Block derives Codec.AsObject {
    require(rows.size <= TableBlock.MaxRows, s"Slack table supports at most ${TableBlock.MaxRows} rows, got ${rows.size}")
    rows.zipWithIndex.foreach { case (row, i) =>
      require(row.size <= TableBlock.MaxColumns, s"Slack table supports at most ${TableBlock.MaxColumns} cells per row, row $i has ${row.size}")
    }
    column_settings.foreach { cs =>
      val maxRowSize = rows.map(_.size).maxOption.getOrElse(0)
      require(cs.size <= maxRowSize, s"column_settings size (${cs.size}) must not exceed the widest row size ($maxRowSize)")
    }
  }

  object TableBlock {
    val MaxRows: Int    = 100
    val MaxColumns: Int = 20
  }

  case class UnknownBlock(raw: Json) extends Block

  // --- Block codecs (TypeEntry-based) ---

  private val blockEntries: List[TypeEntry[? <: Block]] = List(
    TypeEntry[SectionBlock]("section"),
    TypeEntry[ActionsBlock]("actions"),
    TypeEntry[InputBlock]("input"),
    TypeEntry[HeaderBlock]("header"),
    TypeEntry[ContextBlock]("context"),
    TypeEntry[DividerBlock]("divider"),
    TypeEntry[ImageBlock]("image"),
    TypeEntry[RichTextBlock]("rich_text"),
    TypeEntry[FileBlock]("file"),
    TypeEntry[VideoBlock]("video"),
    TypeEntry[TableBlock]("table"),
  )

  private val blockTypeMap: Map[String, Decoder[Block]] =
    blockEntries.map(e => e.typeName -> e.decoder.map(identity[Block])).toMap

  given Encoder.AsObject[Block] = Encoder.AsObject.instance {
    case b: UnknownBlock =>
      b.raw.asObject.getOrElse(JsonObject.empty)
    case block           =>
      blockEntries.iterator
        .flatMap { entry =>
          if (entry.ct.runtimeClass.isInstance(block))
            Some(("type" -> Json.fromString(entry.typeName)) +: entry.encoder.asInstanceOf[Encoder.AsObject[Any]].encodeObject(block))
          else scala.None
        }
        .next()
  }

  given Decoder[Block] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap { tpe =>
      blockTypeMap.get(tpe) match {
        case Some(decoder) => decoder(cursor)
        case None          => cursor.as[Json].map(UnknownBlock(_))
      }
    }
  }

  // --- Views ---

  // https://docs.slack.dev/reference/views/modal-views
  // https://github.com/slackapi/java-slack-sdk/blob/main/slack-api-model/src/main/java/com/slack/api/model/view/View.java
  case class View(
      `type`: ViewType,
      title: TextObject,
      blocks: List[Block],
      callback_id: Option[String] = None,
      submit: Option[TextObject] = None,
      close: Option[TextObject] = None,
      private_metadata: Option[String] = None,
      clear_on_close: Option[Boolean] = None,
      notify_on_close: Option[Boolean] = None,
      external_id: Option[String] = None,
      submit_disabled: Option[Boolean] = None,
  ) derives Codec.AsObject
}
