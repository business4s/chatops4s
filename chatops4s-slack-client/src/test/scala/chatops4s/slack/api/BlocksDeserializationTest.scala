package chatops4s.slack.api

import io.circe.{Decoder, Encoder, Json}
import io.circe.parser.decode
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source
import scala.util.Using

import blocks.*

class BlocksDeserializationTest extends AnyFreeSpec with Matchers {

  // --- helpers ---

  private def parseOk[T: Decoder](json: String)(checks: T => Unit): Unit =
    decode[T](json) match {
      case Right(value) => checks(value)
      case Left(err)    => fail(s"Failed to decode: $err")
    }

  private def parseBlocks(json: String)(checks: List[Block] => Unit): Unit =
    parseOk[List[Block]](json)(checks)

  private def loadFixture(name: String): Option[String] =
    Option(getClass.getResourceAsStream(s"/views/$name")).map { is =>
      Using.resource(Source.fromInputStream(is))(_.mkString)
    }

  /** Encode a Block and return the top-level JSON object for field assertions. */
  private def encodeBlock(block: Block): Json =
    Encoder[Block].apply(block)

  /** Encode a BlockElement and return the JSON object. */
  private def encodeElement(elem: BlockElement): Json =
    Encoder[BlockElement].apply(elem)

  // ---- Block serialization (Scala -> JSON) ----

  "Block serialization" - {

    "SectionBlock with mrkdwn text" in {
      val json = encodeBlock(SectionBlock(text = Some(MarkdownTextObject("Hello *world*"))))
      json.hcursor.get[String]("type").toOption shouldBe Some("section")
      json.hcursor.downField("text").get[String]("type").toOption shouldBe Some("mrkdwn")
      json.hcursor.downField("text").get[String]("text").toOption shouldBe Some("Hello *world*")
    }

    "SectionBlock with fields" in {
      val json   = encodeBlock(
        SectionBlock(
          text = Some(MarkdownTextObject("Summary")),
          fields = Some(
            List(
              MarkdownTextObject("*Priority:*\nHigh"),
              PlainTextObject("Status: Open"),
            ),
          ),
        ),
      )
      val fields = json.hcursor.downField("fields").as[List[Json]].toOption.get
      fields should have size 2
      fields.head.hcursor.get[String]("type").toOption shouldBe Some("mrkdwn")
      fields(1).hcursor.get[String]("type").toOption shouldBe Some("plain_text")
    }

    "SectionBlock with button accessory" in {
      val json = encodeBlock(
        SectionBlock(
          text = Some(MarkdownTextObject("Click the button")),
          accessory = Some(
            ButtonElement(
              text = PlainTextObject("Click Me"),
              action_id = "button-action",
              value = NonEmptyString("click_me_123"),
              style = Some(ButtonStyle.Primary),
              accessibility_label = Some("Click me button"),
            ),
          ),
        ),
      )
      val acc  = json.hcursor.downField("accessory")
      acc.get[String]("type").toOption shouldBe Some("button")
      acc.get[String]("action_id").toOption shouldBe Some("button-action")
      acc.get[String]("value").toOption shouldBe Some("click_me_123")
      acc.get[String]("style").toOption shouldBe Some("primary")
      acc.get[String]("accessibility_label").toOption shouldBe Some("Click me button")
      acc.downField("text").get[String]("type").toOption shouldBe Some("plain_text")
      acc.downField("text").get[String]("text").toOption shouldBe Some("Click Me")
    }

    "SectionBlock with static_select accessory and options" in {
      val json = encodeBlock(
        SectionBlock(
          text = Some(MarkdownTextObject("Pick an item")),
          block_id = Some("section678"),
          accessory = Some(
            StaticSelectElement(
              action_id = "text1234",
              placeholder = Some(PlainTextObject("Select an item")),
              options = Some(
                List(
                  BlockOption(PlainTextObject("Option 1"), "value-0"),
                  BlockOption(PlainTextObject("Option 2"), "value-1"),
                ),
              ),
            ),
          ),
        ),
      )
      json.hcursor.get[String]("block_id").toOption shouldBe Some("section678")
      val acc  = json.hcursor.downField("accessory")
      acc.get[String]("type").toOption shouldBe Some("static_select")
      val opts = acc.downField("options").as[List[Json]].toOption.get
      opts should have size 2
      opts.head.hcursor.get[String]("value").toOption shouldBe Some("value-0")
    }

    "SectionBlock with radio_buttons accessory" in {
      val opt1 = BlockOption(PlainTextObject("Radio 1"), "A1")
      val json = encodeBlock(
        SectionBlock(
          text = Some(PlainTextObject("Check out these rad radio buttons")),
          accessory = Some(
            RadioButtonGroupElement(
              action_id = "this_is_an_action_id",
              options = List(opt1, BlockOption(PlainTextObject("Radio 2"), "A2")),
              initial_option = Some(opt1),
            ),
          ),
        ),
      )
      val acc  = json.hcursor.downField("accessory")
      acc.get[String]("type").toOption shouldBe Some("radio_buttons")
      acc.get[String]("action_id").toOption shouldBe Some("this_is_an_action_id")
      acc.downField("options").as[List[Json]].toOption.get should have size 2
      acc.downField("initial_option").get[String]("value").toOption shouldBe Some("A1")
    }

    "ActionsBlock with multiple buttons" in {
      val json  = encodeBlock(
        ActionsBlock(
          elements = List(
            ButtonElement(PlainTextObject("Approve"), "approve-btn", NonEmptyString("approve"), style = Some(ButtonStyle.Primary)),
            ButtonElement(PlainTextObject("Reject"), "reject-btn", NonEmptyString("reject"), style = Some(ButtonStyle.Danger)),
          ),
          block_id = Some("actions1"),
        ),
      )
      json.hcursor.get[String]("type").toOption shouldBe Some("actions")
      json.hcursor.get[String]("block_id").toOption shouldBe Some("actions1")
      val elems = json.hcursor.downField("elements").as[List[Json]].toOption.get
      elems should have size 2
      elems.head.hcursor.get[String]("style").toOption shouldBe Some("primary")
      elems(1).hcursor.get[String]("style").toOption shouldBe Some("danger")
    }

    "InputBlock with plain_text_input and hint" in {
      val json = encodeBlock(
        InputBlock(
          label = PlainTextObject("Label of input"),
          hint = Some(PlainTextObject("Hint of input")),
          block_id = Some("input123"),
          element = PlainTextInputElement(
            action_id = "plain_input",
            placeholder = Some(PlainTextObject("Enter some plain text")),
            multiline = Some(true),
            min_length = Some(1),
            max_length = Some(500),
          ),
        ),
      )
      json.hcursor.get[String]("type").toOption shouldBe Some("input")
      json.hcursor.get[String]("block_id").toOption shouldBe Some("input123")
      json.hcursor.downField("label").get[String]("text").toOption shouldBe Some("Label of input")
      json.hcursor.downField("hint").get[String]("text").toOption shouldBe Some("Hint of input")
      val elem = json.hcursor.downField("element")
      elem.get[String]("type").toOption shouldBe Some("plain_text_input")
      elem.get[String]("action_id").toOption shouldBe Some("plain_input")
      elem.get[Boolean]("multiline").toOption shouldBe Some(true)
      elem.get[Int]("min_length").toOption shouldBe Some(1)
      elem.get[Int]("max_length").toOption shouldBe Some(500)
    }

    "InputBlock with datepicker" in {
      val json = encodeBlock(
        InputBlock(
          label = PlainTextObject("Pick a date"),
          block_id = Some("date-block"),
          element = DatePickerElement(
            action_id = "date-action",
            initial_date = Some("2024-01-15"),
            placeholder = Some(PlainTextObject("Choose a date")),
          ),
        ),
      )
      val elem = json.hcursor.downField("element")
      elem.get[String]("type").toOption shouldBe Some("datepicker")
      elem.get[String]("initial_date").toOption shouldBe Some("2024-01-15")
    }

    "InputBlock with timepicker" in {
      val json = encodeBlock(
        InputBlock(
          label = PlainTextObject("Pick a time"),
          element = TimePickerElement(
            action_id = "time-action",
            initial_time = Some("14:30"),
            timezone = Some("America/New_York"),
          ),
        ),
      )
      val elem = json.hcursor.downField("element")
      elem.get[String]("type").toOption shouldBe Some("timepicker")
      elem.get[String]("initial_time").toOption shouldBe Some("14:30")
      elem.get[String]("timezone").toOption shouldBe Some("America/New_York")
    }

    "InputBlock with datetimepicker" in {
      val json = encodeBlock(
        InputBlock(
          label = PlainTextObject("Pick date and time"),
          element = DatetimePickerElement(action_id = "dt-action", initial_date_time = Some(1700000000)),
        ),
      )
      val elem = json.hcursor.downField("element")
      elem.get[String]("type").toOption shouldBe Some("datetimepicker")
      elem.get[Int]("initial_date_time").toOption shouldBe Some(1700000000)
    }

    "InputBlock with number_input" in {
      val json = encodeBlock(
        InputBlock(
          label = PlainTextObject("Enter a number"),
          element = NumberInputElement(
            is_decimal_allowed = true,
            action_id = "num-action",
            min_value = Some("0"),
            max_value = Some("100"),
            initial_value = Some("42"),
          ),
        ),
      )
      val elem = json.hcursor.downField("element")
      elem.get[String]("type").toOption shouldBe Some("number_input")
      elem.get[Boolean]("is_decimal_allowed").toOption shouldBe Some(true)
      elem.get[String]("min_value").toOption shouldBe Some("0")
      elem.get[String]("max_value").toOption shouldBe Some("100")
      elem.get[String]("initial_value").toOption shouldBe Some("42")
    }

    "InputBlock with email_text_input" in {
      val json = encodeBlock(
        InputBlock(
          label = PlainTextObject("Email"),
          element = EmailInputElement(action_id = "email-action", initial_value = Some("foo@example.com")),
        ),
      )
      val elem = json.hcursor.downField("element")
      elem.get[String]("type").toOption shouldBe Some("email_text_input")
      elem.get[String]("initial_value").toOption shouldBe Some("foo@example.com")
    }

    "InputBlock with url_text_input" in {
      val json = encodeBlock(
        InputBlock(
          label = PlainTextObject("URL"),
          element = UrlInputElement(action_id = "url-action", initial_value = Some("https://example.com")),
        ),
      )
      val elem = json.hcursor.downField("element")
      elem.get[String]("type").toOption shouldBe Some("url_text_input")
      elem.get[String]("initial_value").toOption shouldBe Some("https://example.com")
    }

    "InputBlock with checkboxes" in {
      val json = encodeBlock(
        InputBlock(
          label = PlainTextObject("Choose"),
          element = CheckboxesElement(
            action_id = "check-action",
            options = List(
              BlockOption(PlainTextObject("A"), "a"),
              BlockOption(PlainTextObject("B"), "b", description = Some(PlainTextObject("Second option"))),
            ),
            initial_options = Some(List(BlockOption(PlainTextObject("A"), "a"))),
          ),
        ),
      )
      val elem = json.hcursor.downField("element")
      elem.get[String]("type").toOption shouldBe Some("checkboxes")
      val opts = elem.downField("options").as[List[Json]].toOption.get
      opts should have size 2
      opts(1).hcursor.downField("description").get[String]("text").toOption shouldBe Some("Second option")
      elem.downField("initial_options").as[List[Json]].toOption.get should have size 1
    }

    "InputBlock with overflow menu" in {
      val json = encodeBlock(
        InputBlock(
          label = PlainTextObject("More"),
          element = OverflowMenuElement(
            action_id = "overflow-action",
            options = List(
              BlockOption(PlainTextObject("Edit"), "edit"),
              BlockOption(PlainTextObject("Delete"), "delete"),
            ),
          ),
        ),
      )
      val elem = json.hcursor.downField("element")
      elem.get[String]("type").toOption shouldBe Some("overflow")
      elem.downField("options").as[List[Json]].toOption.get should have size 2
    }

    "InputBlock with file_input" in {
      val json = encodeBlock(
        InputBlock(
          label = PlainTextObject("Upload"),
          element = FileInputElement(action_id = "file-action", filetypes = Some(List("pdf", "png")), max_files = Some(3)),
        ),
      )
      val elem = json.hcursor.downField("element")
      elem.get[String]("type").toOption shouldBe Some("file_input")
      elem.downField("filetypes").as[List[String]].toOption shouldBe Some(List("pdf", "png"))
      elem.get[Int]("max_files").toOption shouldBe Some(3)
    }

    "InputBlock with dispatch_action_config" in {
      val json = encodeBlock(
        InputBlock(
          label = PlainTextObject("Input"),
          dispatch_action = Some(true),
          element = PlainTextInputElement(
            action_id = "dispatch-action",
            dispatch_action_config = Some(DispatchActionConfig(trigger_actions_on = Some(List(TriggerAction.OnEnterPressed)))),
          ),
        ),
      )
      json.hcursor.get[Boolean]("dispatch_action").toOption shouldBe Some(true)
      val dac  = json.hcursor.downField("element").downField("dispatch_action_config")
      dac.downField("trigger_actions_on").as[List[String]].toOption shouldBe Some(List("on_enter_pressed"))
    }

    "HeaderBlock" in {
      val json = encodeBlock(HeaderBlock(PlainTextObject("This is the headline!"), Some("header1")))
      json.hcursor.get[String]("type").toOption shouldBe Some("header")
      json.hcursor.get[String]("block_id").toOption shouldBe Some("header1")
      json.hcursor.downField("text").get[String]("type").toOption shouldBe Some("plain_text")
      json.hcursor.downField("text").get[String]("text").toOption shouldBe Some("This is the headline!")
    }

    "DividerBlock" in {
      val json = encodeBlock(DividerBlock(Some("div1")))
      json.hcursor.get[String]("type").toOption shouldBe Some("divider")
      json.hcursor.get[String]("block_id").toOption shouldBe Some("div1")
    }

    "ContextBlock with mixed elements" in {
      val json  = encodeBlock(
        ContextBlock(
          elements = List(
            ImageElement(alt_text = "logo", image_url = Some("https://example.com/logo.png")),
            PlainTextObject("Author Name"),
            MarkdownTextObject("*bold text*"),
          ),
          block_id = Some("ctx1"),
        ),
      )
      json.hcursor.get[String]("type").toOption shouldBe Some("context")
      val elems = json.hcursor.downField("elements").as[List[Json]].toOption.get
      elems should have size 3
      elems(0).hcursor.get[String]("type").toOption shouldBe Some("image")
      elems(0).hcursor.get[String]("image_url").toOption shouldBe Some("https://example.com/logo.png")
      elems(1).hcursor.get[String]("type").toOption shouldBe Some("plain_text")
      elems(2).hcursor.get[String]("type").toOption shouldBe Some("mrkdwn")
    }

    "ImageBlock with image_url" in {
      val json = encodeBlock(
        ImageBlock(
          alt_text = "A photo",
          image_url = Some("https://example.com/photo.jpg"),
          title = Some(PlainTextObject("My Photo")),
          block_id = Some("img1"),
          image_width = Some(800),
          image_height = Some(600),
          image_bytes = Some(102400),
          is_animated = Some(false),
        ),
      )
      json.hcursor.get[String]("type").toOption shouldBe Some("image")
      json.hcursor.get[String]("image_url").toOption shouldBe Some("https://example.com/photo.jpg")
      json.hcursor.get[String]("alt_text").toOption shouldBe Some("A photo")
      json.hcursor.downField("title").get[String]("text").toOption shouldBe Some("My Photo")
      json.hcursor.get[Int]("image_width").toOption shouldBe Some(800)
      json.hcursor.get[Int]("image_height").toOption shouldBe Some(600)
      json.hcursor.get[Int]("image_bytes").toOption shouldBe Some(102400)
      json.hcursor.get[Boolean]("is_animated").toOption shouldBe Some(false)
    }

    "ImageBlock with slack_file" in {
      val json = encodeBlock(
        ImageBlock(
          alt_text = "Slack file image",
          slack_file = Some(
            SlackFileObject(
              id = Some("F111111"),
              url = Some("https://files.slack.com/files-pri/T111-F111/foo.png"),
            ),
          ),
        ),
      )
      json.hcursor.downField("slack_file").get[String]("id").toOption shouldBe Some("F111111")
      json.hcursor.downField("slack_file").get[String]("url").toOption shouldBe Some("https://files.slack.com/files-pri/T111-F111/foo.png")
    }

    "RichTextBlock encodes typed elements" in {
      val block       = RichTextBlock(
        elements = List(RichTextSection(elements = List(RichTextText("Hello")))),
        block_id = Some("hUBz"),
      )
      val json        = encodeBlock(block)
      json.hcursor.get[String]("type").toOption shouldBe Some("rich_text")
      json.hcursor.get[String]("block_id").toOption shouldBe Some("hUBz")
      val elems       = json.hcursor.downField("elements").as[List[Json]].toOption.get
      elems should have size 1
      elems.head.hcursor.get[String]("type").toOption shouldBe Some("rich_text_section")
      val inlineElems = elems.head.hcursor.downField("elements").as[List[Json]].toOption.get
      inlineElems should have size 1
      inlineElems.head.hcursor.get[String]("type").toOption shouldBe Some("text")
      inlineElems.head.hcursor.get[String]("text").toOption shouldBe Some("Hello")
    }

    "FileBlock" in {
      val json = encodeBlock(FileBlock(external_id = "ABCDE", source = Some("remote"), block_id = Some("file1")))
      json.hcursor.get[String]("type").toOption shouldBe Some("file")
      json.hcursor.get[String]("external_id").toOption shouldBe Some("ABCDE")
      json.hcursor.get[String]("source").toOption shouldBe Some("remote")
    }

    "VideoBlock" in {
      val json = encodeBlock(
        VideoBlock(
          alt_text = "Product demo",
          video_url = "https://example.com/video.mp4",
          thumbnail_url = "https://example.com/thumb.jpg",
          title = PlainTextObject("Demo Video"),
          title_url = Some("https://example.com/video"),
          description = Some(PlainTextObject("A product demo")),
          author_name = Some("Acme Corp"),
          provider_name = Some("YouTube"),
          provider_icon_url = Some("https://example.com/yt-icon.png"),
          block_id = Some("video1"),
        ),
      )
      json.hcursor.get[String]("type").toOption shouldBe Some("video")
      json.hcursor.get[String]("video_url").toOption shouldBe Some("https://example.com/video.mp4")
      json.hcursor.get[String]("thumbnail_url").toOption shouldBe Some("https://example.com/thumb.jpg")
      json.hcursor.get[String]("title_url").toOption shouldBe Some("https://example.com/video")
      json.hcursor.get[String]("author_name").toOption shouldBe Some("Acme Corp")
      json.hcursor.get[String]("provider_name").toOption shouldBe Some("YouTube")
    }

    "TableBlock with mixed raw_text and rich_text cells" in {
      val json    = encodeBlock(
        TableBlock(
          block_id = Some("table_123"),
          column_settings = Some(
            List(
              TableColumnSetting(is_wrapped = Some(true)),
              TableColumnSetting(align = Some(TableCellAlign.Right)),
            ),
          ),
          rows = List(
            List("Header A", "Header B"),
            List(
              "Data 1A",
              RichTextBlock(
                elements = List(
                  RichTextSection(elements = List(RichTextLink(url = "https://slack.com", text = Some("Link")))),
                ),
              ),
            ),
          ),
        ),
      )
      json.hcursor.get[String]("type").toOption shouldBe Some("table")
      json.hcursor.get[String]("block_id").toOption shouldBe Some("table_123")
      val cols    = json.hcursor.downField("column_settings").as[List[Json]].toOption.get
      cols should have size 2
      cols.head.hcursor.get[Boolean]("is_wrapped").toOption shouldBe Some(true)
      cols(1).hcursor.get[String]("align").toOption shouldBe Some("right")
      val rows    = json.hcursor.downField("rows").as[List[List[Json]]].toOption.get
      rows should have size 2
      rows.head.head.hcursor.get[String]("type").toOption shouldBe Some("raw_text")
      rows.head.head.hcursor.get[String]("text").toOption shouldBe Some("Header A")
      val rich    = rows(1)(1).hcursor
      rich.get[String]("type").toOption shouldBe Some("rich_text")
      val rtElems = rich.downField("elements").as[List[Json]].toOption.get
      rtElems.head.hcursor.get[String]("type").toOption shouldBe Some("rich_text_section")
    }

    "TableBlock decodes a literal JSON payload from Slack docs" in {
      val rawJson = """{
                      |  "type": "table",
                      |  "block_id": "table_123",
                      |  "column_settings": [
                      |    { "is_wrapped": true },
                      |    { "align": "right" }
                      |  ],
                      |  "rows": [
                      |    [
                      |      { "type": "raw_text", "text": "Header A" },
                      |      { "type": "raw_text", "text": "Header B" }
                      |    ],
                      |    [
                      |      { "type": "raw_text", "text": "Data 1A" },
                      |      {
                      |        "type": "rich_text",
                      |        "elements": [
                      |          {
                      |            "type": "rich_text_section",
                      |            "elements": [
                      |              { "type": "link", "text": "Link", "url": "https://slack.com" }
                      |            ]
                      |          }
                      |        ]
                      |      }
                      |    ]
                      |  ]
                      |}""".stripMargin
      parseOk[Block](rawJson) { block =>
        val table = block.asInstanceOf[TableBlock]
        table.block_id shouldBe Some("table_123")
        table.column_settings.get should have size 2
        table.column_settings.get(1).align shouldBe Some(TableCellAlign.Right)
        table.rows should have size 2
        table.rows.head.head shouldBe "Header A"
        val cell  = table.rows(1)(1).asInstanceOf[RichTextBlock]
        cell.elements.head shouldBe a[RichTextSection]
      }
    }

    "None fields are encoded as null" in {
      val json = encodeBlock(SectionBlock(text = Some(PlainTextObject("x"))))
      json.hcursor.get[Option[String]]("block_id").toOption shouldBe Some(None)
      json.hcursor.get[Option[List[Json]]]("fields").toOption shouldBe Some(None)
    }
  }

  // ---- Element serialization ----

  "Element serialization" - {

    "multi_static_select with options" in {
      val json = encodeElement(
        MultiStaticSelectElement(
          action_id = "text1234",
          placeholder = Some(PlainTextObject("Select items")),
          options = Some(
            List(
              BlockOption(PlainTextObject("Option 1"), "value-0"),
              BlockOption(PlainTextObject("Option 2"), "value-1"),
              BlockOption(PlainTextObject("Option 3"), "value-2"),
            ),
          ),
        ),
      )
      json.hcursor.get[String]("type").toOption shouldBe Some("multi_static_select")
      json.hcursor.downField("options").as[List[Json]].toOption.get should have size 3
      json.hcursor.downField("placeholder").get[String]("text").toOption shouldBe Some("Select items")
    }

    "multi_external_select with min_query_length" in {
      val json = encodeElement(MultiExternalSelectElement(action_id = "ext1234", min_query_length = Some(3)))
      json.hcursor.get[String]("type").toOption shouldBe Some("multi_external_select")
      json.hcursor.get[Int]("min_query_length").toOption shouldBe Some(3)
    }

    "multi_users_select" in {
      val json = encodeElement(MultiUsersSelectElement(action_id = "users1234"))
      json.hcursor.get[String]("type").toOption shouldBe Some("multi_users_select")
      json.hcursor.get[String]("action_id").toOption shouldBe Some("users1234")
    }

    "multi_conversations_select" in {
      val json = encodeElement(MultiConversationsSelectElement(action_id = "conv1234"))
      json.hcursor.get[String]("type").toOption shouldBe Some("multi_conversations_select")
    }

    "multi_channels_select" in {
      val json = encodeElement(MultiChannelsSelectElement(action_id = "chan1234"))
      json.hcursor.get[String]("type").toOption shouldBe Some("multi_channels_select")
    }

    "conversations_select with filter" in {
      val json = encodeElement(
        ConversationsSelectElement(
          action_id = "conv",
          filter = Some(
            ConversationsFilter(
              include = Some(List("public", "mpim")),
              exclude_external_shared_channels = Some(true),
              exclude_bot_users = Some(false),
            ),
          ),
        ),
      )
      json.hcursor.get[String]("type").toOption shouldBe Some("conversations_select")
      val f    = json.hcursor.downField("filter")
      f.downField("include").as[List[String]].toOption shouldBe Some(List("public", "mpim"))
      f.get[Boolean]("exclude_external_shared_channels").toOption shouldBe Some(true)
      f.get[Boolean]("exclude_bot_users").toOption shouldBe Some(false)
    }

    "channels_select with response_url_enabled" in {
      val json = encodeElement(ChannelsSelectElement(action_id = "chan", response_url_enabled = Some(true)))
      json.hcursor.get[String]("type").toOption shouldBe Some("channels_select")
      json.hcursor.get[Boolean]("response_url_enabled").toOption shouldBe Some(true)
    }

    "conversations_select with default_to_current_conversation" in {
      val json = encodeElement(ConversationsSelectElement(action_id = "conv", default_to_current_conversation = Some(true)))
      json.hcursor.get[Boolean]("default_to_current_conversation").toOption shouldBe Some(true)
    }

    "button with confirmation dialog" in {
      val json = encodeElement(
        ButtonElement(
          text = PlainTextObject("Delete"),
          action_id = "delete-btn",
          value = NonEmptyString("delete"),
          style = Some(ButtonStyle.Danger),
          confirm = Some(
            ConfirmationDialogObject(
              title = PlainTextObject("Are you sure?"),
              text = MarkdownTextObject("This action *cannot* be undone."),
              confirm = PlainTextObject("Yes, delete"),
              deny = PlainTextObject("Cancel"),
              style = Some(ButtonStyle.Danger),
            ),
          ),
        ),
      )
      json.hcursor.get[String]("type").toOption shouldBe Some("button")
      val c    = json.hcursor.downField("confirm")
      c.downField("title").get[String]("text").toOption shouldBe Some("Are you sure?")
      c.downField("text").get[String]("type").toOption shouldBe Some("mrkdwn")
      c.downField("confirm").get[String]("text").toOption shouldBe Some("Yes, delete")
      c.downField("deny").get[String]("text").toOption shouldBe Some("Cancel")
      c.get[String]("style").toOption shouldBe Some("danger")
    }

    "static_select with option_groups" in {
      val json   = encodeElement(
        StaticSelectElement(
          action_id = "grouped",
          option_groups = Some(
            List(
              OptionGroupObject(
                label = PlainTextObject("Group 1"),
                options = List(
                  BlockOption(PlainTextObject("A"), "a"),
                  BlockOption(PlainTextObject("B"), "b"),
                ),
              ),
              OptionGroupObject(
                label = PlainTextObject("Group 2"),
                options = List(BlockOption(PlainTextObject("C"), "c")),
              ),
            ),
          ),
        ),
      )
      json.hcursor.get[String]("type").toOption shouldBe Some("static_select")
      val groups = json.hcursor.downField("option_groups").as[List[Json]].toOption.get
      groups should have size 2
      groups.head.hcursor.downField("label").get[String]("text").toOption shouldBe Some("Group 1")
      groups.head.hcursor.downField("options").as[List[Json]].toOption.get should have size 2
    }
  }

  // ---- View deserialization from fixtures (views come FROM Slack) ----

  "View deserialization from fixtures" - {

    "view1 - meeting arrangement with datepicker, multi_external_select, plain_text_input" in {
      val json = loadFixture("view1.json")
      assume(json.isDefined, "Fixture views/view1.json not found")
      parseOk[View](json.get) { view =>
        view.`type` shouldBe ViewType.Modal
        view.callback_id shouldBe Some("view-callback-id")
        view.notify_on_close shouldBe Some(true)
        view.title.text shouldBe "Meeting Arrangement"
        view.submit.get.text shouldBe "Submit"
        view.close.get.text shouldBe "Cancel"
        view.blocks should have size 3

        val inputBlock = view.blocks(0).asInstanceOf[InputBlock]
        inputBlock.block_id shouldBe Some("date")
        inputBlock.element shouldBe a[DatePickerElement]
        inputBlock.element.asInstanceOf[DatePickerElement].initial_date shouldBe Some("2019-10-22")

        val sectionBlock = view.blocks(1).asInstanceOf[SectionBlock]
        sectionBlock.accessory.get shouldBe a[MultiExternalSelectElement]
        sectionBlock.accessory.get.asInstanceOf[MultiExternalSelectElement].min_query_length shouldBe Some(1)

        val agendaBlock = view.blocks(2).asInstanceOf[InputBlock]
        agendaBlock.element shouldBe a[PlainTextInputElement]
        agendaBlock.element.asInstanceOf[PlainTextInputElement].multiline shouldBe Some(true)
      }
    }

    "view2 - satisfaction survey with static_select options" in {
      val json = loadFixture("view2.json")
      assume(json.isDefined, "Fixture views/view2.json not found")
      parseOk[View](json.get) { view =>
        view.callback_id shouldBe Some("app-satisfaction-survey")
        view.blocks should have size 3

        val ratingSection = view.blocks(1).asInstanceOf[SectionBlock]
        val sel           = ratingSection.accessory.get.asInstanceOf[StaticSelectElement]
        sel.options.get should have size 3
        sel.options.get.head.value shouldBe "3"

        val feedbackInput = view.blocks(2).asInstanceOf[InputBlock]
        feedbackInput.optional shouldBe Some(true)
      }
    }

    "view3 - read-it-later with private_metadata" in {
      val json = loadFixture("view3.json")
      assume(json.isDefined, "Fixture views/view3.json not found")
      parseOk[View](json.get) { view =>
        view.callback_id shouldBe Some("read-it-later")
        view.private_metadata shouldBe Some("some-private-data")
        view.blocks should have size 3
        view.blocks(0) shouldBe a[SectionBlock]
        view.blocks(1) shouldBe a[SectionBlock]
        view.blocks(2) shouldBe a[InputBlock]
      }
    }

    "view4 - channels_select and conversations_select with filter" in {
      val json = loadFixture("view4.json")
      assume(json.isDefined, "Fixture views/view4.json not found")
      parseOk[View](json.get) { view =>
        view.blocks should have size 2

        val channelSelect = view.blocks(0).asInstanceOf[InputBlock].element.asInstanceOf[ChannelsSelectElement]
        channelSelect.response_url_enabled shouldBe Some(true)

        val convSelect = view.blocks(1).asInstanceOf[InputBlock].element.asInstanceOf[ConversationsSelectElement]
        convSelect.filter.get.include shouldBe Some(List("public"))
        convSelect.filter.get.exclude_external_shared_channels shouldBe Some(true)
        convSelect.filter.get.exclude_bot_users shouldBe Some(false)
      }
    }

    "view5 - conversations_select and multi_conversations_select with default_to_current" in {
      val json = loadFixture("view5.json")
      assume(json.isDefined, "Fixture views/view5.json not found")
      parseOk[View](json.get) { view =>
        view.blocks should have size 2

        val single = view.blocks(0).asInstanceOf[InputBlock].element.asInstanceOf[ConversationsSelectElement]
        single.default_to_current_conversation shouldBe Some(true)

        val multi = view.blocks(1).asInstanceOf[InputBlock].element.asInstanceOf[MultiConversationsSelectElement]
        multi.default_to_current_conversation shouldBe Some(true)
      }
    }
  }

  // ---- Unknown type handling (deserialization resilience) ----

  "Unknown type fallback" - {

    "unknown block type falls back to UnknownBlock" in {
      parseBlocks(
        """[{
          |  "type": "unknown_block",
          |  "block_id": "unk1",
          |  "some_field": "some_value"
          |}]""".stripMargin,
      ) { blocks =>
        blocks should have size 1
        blocks.head shouldBe a[UnknownBlock]
        val raw = blocks.head.asInstanceOf[UnknownBlock].raw
        raw.hcursor.get[String]("type").toOption shouldBe Some("unknown_block")
        raw.hcursor.get[String]("some_field").toOption shouldBe Some("some_value")
      }
    }

    "unknown element type falls back to UnknownBlockElement" in {
      parseBlocks(
        """[{
          |  "type": "input",
          |  "label": {"type": "plain_text", "text": "Label"},
          |  "element": {
          |    "type": "unknown_element",
          |    "action_id": "unk",
          |    "some_field": 42
          |  }
          |}]""".stripMargin,
      ) { blocks =>
        val elem = blocks.head.asInstanceOf[InputBlock].element
        elem shouldBe a[UnknownBlockElement]
        val raw  = elem.asInstanceOf[UnknownBlockElement].raw
        raw.hcursor.get[String]("type").toOption shouldBe Some("unknown_element")
        raw.hcursor.get[Int]("some_field").toOption shouldBe Some(42)
      }
    }

    "mixed known and unknown blocks" in {
      parseBlocks(
        """[
          |  {"type": "header", "text": {"type": "plain_text", "text": "Hello"}},
          |  {"type": "future_block_type", "data": [1, 2, 3]},
          |  {"type": "divider"}
          |]""".stripMargin,
      ) { blocks =>
        blocks should have size 3
        blocks(0) shouldBe a[HeaderBlock]
        blocks(1) shouldBe a[UnknownBlock]
        blocks(2) shouldBe a[DividerBlock]
      }
    }
  }

  // ---- Message with blocks (deserialization - messages come FROM Slack) ----

  "Message with blocks" - {

    "message containing multiple block types" in {
      parseOk[Message](
        """{
          |  "type": "message",
          |  "user": "U12345",
          |  "text": "fallback text",
          |  "ts": "1503435956.000247",
          |  "blocks": [
          |    {
          |      "type": "header",
          |      "text": {"type": "plain_text", "text": "Deployment Status"}
          |    },
          |    {"type": "divider"},
          |    {
          |      "type": "section",
          |      "text": {"type": "mrkdwn", "text": "*Service:* my-app\n*Version:* 1.2.3"}
          |    },
          |    {
          |      "type": "section",
          |      "fields": [
          |        {"type": "mrkdwn", "text": "*Environment:*\nProduction"},
          |        {"type": "mrkdwn", "text": "*Status:*\n:white_check_mark: Healthy"}
          |      ]
          |    },
          |    {
          |      "type": "actions",
          |      "elements": [
          |        {
          |          "type": "button",
          |          "text": {"type": "plain_text", "text": "View Logs"},
          |          "url": "https://example.com/logs",
          |          "action_id": "view-logs"
          |        },
          |        {
          |          "type": "button",
          |          "text": {"type": "plain_text", "text": "Rollback"},
          |          "style": "danger",
          |          "value": "rollback",
          |          "action_id": "rollback"
          |        }
          |      ]
          |    },
          |    {
          |      "type": "context",
          |      "elements": [
          |        {"type": "mrkdwn", "text": "Deployed by <@U12345> at 2024-01-15 14:30 UTC"}
          |      ]
          |    }
          |  ]
          |}""".stripMargin,
      ) { msg =>
        msg.`type` shouldBe Some("message")
        msg.user shouldBe Some(UserId("U12345"))
        msg.text shouldBe Some("fallback text")
        msg.ts shouldBe Some(Timestamp("1503435956.000247"))
        val blocks = msg.blocks.get
        blocks should have size 6
        blocks(0) shouldBe a[HeaderBlock]
        blocks(1) shouldBe a[DividerBlock]
        blocks(2) shouldBe a[SectionBlock]
        blocks(3) shouldBe a[SectionBlock]
        blocks(4) shouldBe a[ActionsBlock]
        blocks(5) shouldBe a[ContextBlock]

        val actionsBlock = blocks(4).asInstanceOf[ActionsBlock]
        actionsBlock.elements should have size 2
        actionsBlock.elements.head.asInstanceOf[ButtonElement].url shouldBe Some("https://example.com/logs")
      }
    }

    "message with rich_text block from real Slack" in {
      parseOk[Message](
        """{
          |  "client_msg_id": "70c82df9-9db9-48b0-bf4e-9c43db3ed097",
          |  "type": "message",
          |  "text": "This is a *rich text* message",
          |  "user": "U0JD3BPNC",
          |  "ts": "1565629075.001000",
          |  "team": "T0JD3BPMW",
          |  "blocks": [
          |    {
          |      "type": "rich_text",
          |      "block_id": "hUBz",
          |      "elements": [
          |        {
          |          "type": "rich_text_section",
          |          "elements": [
          |            {"type": "text", "text": "This is a "},
          |            {"type": "text", "text": "rich text ", "style": {"bold": true}},
          |            {"type": "text", "text": "message"}
          |          ]
          |        }
          |      ]
          |    }
          |  ]
          |}""".stripMargin,
      ) { msg =>
        msg.client_msg_id shouldBe Some("70c82df9-9db9-48b0-bf4e-9c43db3ed097")
        msg.team shouldBe Some(TeamId("T0JD3BPMW"))
        val rt      = msg.blocks.get.head.asInstanceOf[RichTextBlock]
        rt.block_id shouldBe Some("hUBz")
        rt.elements should have size 1
        val section = rt.elements.head.asInstanceOf[RichTextSection]
        section.elements should have size 3
        val text1   = section.elements(0).asInstanceOf[RichTextText]
        text1.text shouldBe "This is a "
        text1.style shouldBe None
        val text2   = section.elements(1).asInstanceOf[RichTextText]
        text2.text shouldBe "rich text "
        text2.style shouldBe Some(RichTextStyle(bold = Some(true)))
        val text3   = section.elements(2).asInstanceOf[RichTextText]
        text3.text shouldBe "message"
      }
    }
  }

  // ---- Roundtrip (encode -> decode) ----

  "Roundtrip encode/decode" - {

    "View roundtrip" in {
      val view = View(
        `type` = ViewType.Modal,
        title = PlainTextObject("Test Modal", Some(true)),
        blocks = List(
          InputBlock(
            label = PlainTextObject("Name"),
            element = PlainTextInputElement(action_id = "name-input", placeholder = Some(PlainTextObject("Enter name"))),
            block_id = Some("name-block"),
          ),
          InputBlock(
            label = PlainTextObject("Date"),
            element = DatePickerElement(action_id = "date-input", initial_date = Some("2024-01-01")),
            block_id = Some("date-block"),
          ),
          SectionBlock(
            text = Some(MarkdownTextObject("Pick a rating")),
            accessory = Some(
              StaticSelectElement(
                action_id = "rating",
                options = Some(
                  List(
                    BlockOption(PlainTextObject("Good"), "good"),
                    BlockOption(PlainTextObject("Bad"), "bad"),
                  ),
                ),
              ),
            ),
          ),
        ),
        callback_id = Some("test-modal"),
        submit = Some(PlainTextObject("Submit")),
        close = Some(PlainTextObject("Cancel")),
        notify_on_close = Some(true),
      )

      val json    = Encoder[View].apply(view)
      val decoded = json.as[View]
      decoded shouldBe Right(view)
    }

    "View fixture roundtrip preserves structure" in {
      val json      = loadFixture("view1.json")
      assume(json.isDefined, "Fixture views/view1.json not found")
      val view      = decode[View](json.get).toOption.get
      val reEncoded = Encoder[View].apply(view)
      val reDecoded = reEncoded.as[View]
      reDecoded shouldBe Right(view)
    }

    "all element types encode their type field and roundtrip" in {
      val elements: List[(BlockElement, String)] = List(
        ButtonElement(PlainTextObject("x"), "a") -> "button",
        PlainTextInputElement("a")               -> "plain_text_input",
        NumberInputElement(true, "a")            -> "number_input",
        CheckboxesElement(Nil, "a")              -> "checkboxes",
        RadioButtonGroupElement(Nil, "a")        -> "radio_buttons",
        StaticSelectElement("a")                 -> "static_select",
        MultiStaticSelectElement("a")            -> "multi_static_select",
        OverflowMenuElement(Nil, "a")            -> "overflow",
        DatePickerElement("a")                   -> "datepicker",
        TimePickerElement("a")                   -> "timepicker",
        DatetimePickerElement("a")               -> "datetimepicker",
        EmailInputElement("a")                   -> "email_text_input",
        UrlInputElement("a")                     -> "url_text_input",
        ImageElement("alt")                      -> "image",
        ConversationsSelectElement("a")          -> "conversations_select",
        ChannelsSelectElement("a")               -> "channels_select",
        UsersSelectElement("a")                  -> "users_select",
        ExternalSelectElement("a")               -> "external_select",
        MultiUsersSelectElement("a")             -> "multi_users_select",
        MultiChannelsSelectElement("a")          -> "multi_channels_select",
        MultiConversationsSelectElement("a")     -> "multi_conversations_select",
        MultiExternalSelectElement("a")          -> "multi_external_select",
        RichTextInputElement("a")                -> "rich_text_input",
        FileInputElement("a")                    -> "file_input",
      )

      elements.foreach { (elem, expectedType) =>
        val json    = Encoder[BlockElement].apply(elem)
        json.hcursor.get[String]("type").toOption shouldBe Some(expectedType)
        val decoded = json.as[BlockElement]
        decoded shouldBe Right(elem)
      }
    }

    "all block types encode their type field and roundtrip" in {
      val blocks: List[(Block, String)] = List(
        SectionBlock(text = Some(PlainTextObject("x")))                                                 -> "section",
        ActionsBlock(elements = Nil)                                                                    -> "actions",
        InputBlock(label = PlainTextObject("x"), element = PlainTextInputElement("a"))                  -> "input",
        HeaderBlock(text = PlainTextObject("x"))                                                        -> "header",
        ContextBlock(elements = Nil)                                                                    -> "context",
        DividerBlock()                                                                                  -> "divider",
        ImageBlock(alt_text = "x", image_url = Some("https://example.com/x.png"))                       -> "image",
        RichTextBlock(elements = Nil)                                                                   -> "rich_text",
        FileBlock(external_id = "x")                                                                    -> "file",
        VideoBlock("x", "https://example.com/v.mp4", "https://example.com/t.jpg", PlainTextObject("x")) -> "video",
        TableBlock(rows = List(List("x")))                                                              -> "table",
      )

      blocks.foreach { (block, expectedType) =>
        val json    = Encoder[Block].apply(block)
        json.hcursor.get[String]("type").toOption shouldBe Some(expectedType)
        val decoded = json.as[Block]
        decoded shouldBe Right(block)
      }
    }
  }
}
