package shipreq.webapp.client.project.app.pages.config.reqtypes

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.data._
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.ui.AutosizeTextarea
import shipreq.webapp.base.ui.semantic.{Icon, Message}
import shipreq.webapp.base.ui.widgets.Form
import shipreq.webapp.client.project.app.Style.{reqTypeConfig => *}

/** This isn't really an editor; it's read/only! But it's what appears in place of the editor. */
private[reqtypes] object StaticReqTypeEditor {

  final case class Props(reqType: StaticReqType) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private val readOnly =
    Message(
      style   = Message.Style(Message.Type.Info),
      icon    = Icon.InfoCircle,
      header  = "Read-only",
      content = "This req type is built-in and cannot be removed or modified.",
    )(*.staticReadOnly)

  private implicit def vux = ValidationUX.Off

  private def render(p: Props): VdomNode = {

    val mnemonics =
      Form.Field.text
        .withLabel(FieldNames.mnemonic)
        .withValue(p.reqType.mnemonic.value)
        .disable
        .modEditor(f => e => f(TagMod(e, *.editorMnemonic)))

    val name =
      Form.Field.text
        .withLabel(FieldNames.name)
        .withValue(p.reqType.name)
        .disable

    val imp =
      Form.Field.booleanSelect(Mandatory)(_.toText)
        .withLabel(Shared.implication)
        .withValue(p.reqType.implication)
        .disable

    val desc =
      Form.Field.text
        .withEditor(AutosizeTextarea.editor)
        .withLabel(FieldNames.desc)
        .withValue(p.reqType.description.getOrElse(""))
        .disable

    <.div(
      readOnly,
      Form(mnemonics, name, imp, desc))
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
