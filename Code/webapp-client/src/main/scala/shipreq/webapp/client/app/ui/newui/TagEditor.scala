package shipreq.webapp.client.app.ui.newui

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import shipreq.base.util.IMap
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Grammar.{hashRefKey => G}
import shipreq.webapp.base.validation._
import shipreq.webapp.client.app.ui.reqtable.edit.AutoComplete
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.lib.ui.feature._
import shipreq.webapp.client.lib.{HideDead, Plain}

object TagEditor {

  /**
   * Lookup of tags by their names.
   * Required for validation.
   */
  type Lookup = IMap[String, ApplicableTag]

  object Lookup {
    def empty: Lookup =
      IMap.empty(_.key.value)

    def apply(tags: TraversableOnce[ApplicableTag]): Lookup =
      empty ++ tags.toIterator.filter(_.live :: Live)

    def forTagField(f: CustomField.Tag.Id)(p: Project): Lookup =
      apply(p.config.liveTagColumnDistribution.tags inColumn f)

    def notUsedInTagFields(p: Project): Lookup =
      apply(p.config.liveTagColumnDistribution.tags.notUsedInColumns)
  }

  def initialValues(initial: Set[ApplicableTagId], pc: ProjectConfig, l: Lookup): (Set[ApplicableTagId], String) = {
    val ls = l.valuesIterator.map(_.id).toSet
    val ids = initial & ls
    val text =
      ids.toVector
        .map(a => pc.atag(a).key.value)
        .sorted |>
        G.seqFormat.merge
    (ids, text)
  }

  case class Props(edit  : ExternalVar[String],
                   lookup: Lookup,
                   tagMod: Option[Stream[ApplicableTag]] => TagMod)

  implicit val reusabilityLookup: Reusability[Lookup] =
    Reusability.byRef[Lookup] || Reusability.byUnivEq(_.underlyingMap)

  private val editorRef = Ref[dom.html.Input]("i")

  val validator =
    Validator.seqText(G.seqFormat)((l: Lookup) =>
      i => ValidationResult.option(l get i, VFailure looseMsg s"Invalid tag: $i"))

  class Backend($: BackendScope[Props, Unit]) {
    private val pxLookup = Px.bs($).propsA(_.lookup)

    val pxAutoComplete = pxLookup.map(l =>
      AutoCompleteFeature.Strategies( // TODO Fix AutoComplete
        AutoComplete.tag(l.values.toStream, HideDead)(Plain)))

    def render(p: Props) = {
      val validated = EditValidationFeature(validator.correctAndValidate(p.lookup, p.edit.value))

      <.div(
        <.input.text(
          p.tagMod(validated.validated),
          ^.onChange  ==> ((e: ReactEventI) => p.edit.set(e.target.value)),
          ^.ref        := editorRef,
          ^.value      := p.edit.value),
        validated.renderFailure)
    }
  }

  val Component =
    ReactComponentB[Props]("TagEditor")
      .renderBackend[Backend]
      // TODO .configure(Reusability.shouldComponentUpdate)
      .configure(AutoCompleteFeature.installBP(editorRef, _.pxAutoComplete.value(), _.edit.set))
      .build
}
