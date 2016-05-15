package shipreq.webapp.client.project.widgets.high

import japgolly.scalajs.react.extra._
import japgolly.scalajs.react._, vdom.prefix_<^._
import shipreq.base.util.{Ref => _, _}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.GrammarSpec.SeqFormat
import shipreq.webapp.base.validation.{Validator, ValidUpdateVR}
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.lib.{AutoComplete, TextEditor}
import Validators.{reqCode => V}

sealed abstract class ReqCodeEditor[In: Reusability, Out] {

  val textEditor: TextEditor

  val validator: Validator[V.VS, String, _, In]

  def liveCorrect(t: String): String

  def dataToSet(d: Option[In]): Set[ReqCode.Value]

  def editValidation(p: Props): EditValidationFeature.Result[Out]

  /** Extra properties to apply to the tag. */
  type Extra = ValidUpdateVR[Out] ~=> TagMod

  case class Props(edit        : ReusableVar[String],
                   initialValue: Option[In],
                   trie        : ReqCode.Trie,
                   extra       : Extra) {

    val parseResult =
      validator.correctAndValidate(V.VS(trie, dataToSet(initialValue)), edit.value)

    def render = Component(this)
  }

  implicit lazy val reusabilityProps: Reusability[Props] =
    Reusability.caseClass

  private val editorRef = Ref[textEditor.Dom]("i")

  final class Backend($: BackendScope[Props, Unit]) {
    private val pxTrie = Px.bs($).propsA(_.trie)

    val pxAutoComplete = pxTrie.map(t =>
      AutoComplete.reqCode.prefixes(t))

    def render(p: Props) = {
      val validated = editValidation(p)

      <.div(
        textEditor.tag(
          ^.ref        := editorRef,
          ^.value      := p.edit.value,
          ^.onChange  ==> ((e: ReactEventI) => p.edit.set(liveCorrect(e.target.value))),
          p.extra(validated)),
        validated.renderFailure)
    }
  }

  @inline private implicit def impTextEditor = textEditor.asImplicit

  // lazy else there'll be a FieldNotInitialised error via .configure → impTextEditor → textEditor
  lazy val Component =
    ReactComponentB[Props]("ReqCodeEditor")
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .configure(AutoCompleteFeature.installBP(editorRef, _.pxAutoComplete.value(), _.edit.set))
      .build
}

object ReqCodeEditor {

  // ===================================================================================================================

  /**
    * Editor for a single ReqCode (as is the case in ReqCodeGroups).
    */
  object Single extends ReqCodeEditor[ReqCode.Value, ReqCode.Value] {
    override val textEditor                          = TextEditor.Input
    override val validator                           = V.code
    override def liveCorrect(txt: String)            = V.code.liveCorrect(txt)
    override def dataToSet(d: Option[ReqCode.Value]) = d.toSet

    override def editValidation(p: Props) =
      EditValidationFeature.compareOption(p.parseResult)(p.initialValue)
  }

  // ===================================================================================================================

  /**
    * Editor for multiple ReqCodes.
    */
  object Multiple extends ReqCodeEditor[Set[ReqCode.Value], SetDiff.NE[ReqCode.Value]] {

    override val textEditor = TextEditor.TextArea

    val seqFmt = SeqFormat(_.trim, "\\s*[\n\r]\\s*".r.pattern, identity, _.isEmpty, _ mkString "\n")

    override val validator =
      Validator.seqText(seqFmt)(V.code.correctAndValidate _ curried)
        .map(_.toSet)
        .andThen(V.codeSet.liftS)

    override def liveCorrect(txt: String): String =
      if (txt.trim.isEmpty)
        ""
      else {
        val r = txt.split("[\n\r]").map(V.code.liveCorrect).mkString("\n")
        Util.fixBeforeAfter(txt, r)(_ endsWith "\n", _ + "\n")
      }

    override def dataToSet(d: Option[Set[ReqCode.Value]]) =
      d getOrElse UnivEq.emptySet

    override def editValidation(p: Props) =
      EditValidationFeature.compareSetOption(p.parseResult)(p.initialValue)
  }
}
