package shipreq.webapp.client.project.widgets.editors_with_controls

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.PotentialChange
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.{EditControlsFeature, EditorStatus}
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.ui.semantic.{Dropdown => _, _}
import shipreq.webapp.base.ui.widgets.Dropdown
import shipreq.webapp.client.project.app.Style.widgets.{reqTypeSelector => *}
import shipreq.webapp.client.project.feature.editor.{PotentialValue, PotentialValueAcceptor}

object ReqTypeSelector {

  type RT = CustomReqType

  final case class Props(initialValue: Option[RT],
                         edit        : StateSnapshot[RT],
                         choices     : NonEmptySet[RT],
                         asyncStatus : Option[EditorStatus.Async],
                         abort       : Option[Callback],
                         commitFn    : Option[RT ~=> Callback]) {

    val change: PotentialChange[Nothing, RT] =
      PotentialChange.Success(edit.value).ignoreOption(initialValue)

    val commit: Option[Callback] =
      change.toOption.flatMap(v => commitFn.map(_ apply v))

    val status: EditorStatus =
      asyncStatus getOrElse EditorStatus.fromValidatedChange(change)(_ => commit, abort)

    @inline def render: VdomNode = Component(this)
  }

  // implicit val reusabilityProps: Reusability[Props] =
  //   Reusability.derive

  private def key(rt: RT): Dropdown.ItemKey =
    rt.id.value.toString

  private def render(p: Props): VdomNode =
    EditControlsFeature.renderEditor(
      status       = p.status,
      editor       = _ => editor(p),
      readOnlyView = p.edit.value.fullName,
      instructions = EmptyVdom)

  private def editor(p: Props): VdomElement = {
    val options =
      MutableArray(p.choices.whole)
        .sortBy(_.fullName)
        .map(rt => Dropdown.Item(key(rt), rt.fullName, rt))
        .arraySeq

    val dropdown = Dropdown.Props.Optional(options, Some(key(p.edit.value)), tagMod = *.dropdown)(p.edit setState _.value)

    val commitButton = Button(
      tipe = Button.Type.IconOnly(Icon.Checkmark),
      state = Button.State.enabledWhen(p.commit.isDefined)
    ).tag(*.commit, ^.onClick -->? p.commit)

    val abortButton = Button(
      tipe = Button.Type.IconOnly(Icon.Remove),
      state = Button.State.enabledWhen(p.abort.isDefined)
    ).tag(*.abort, ^.onClick -->? p.abort)

    val buttons = Button.group(commitButton, abortButton)(*.buttons)

    <.div(dropdown.render, buttons)
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    // .configure(Reusability.shouldComponentUpdate)
    .build

  // ===================================================================================================================

  def potentialValueAcceptor(choices: Iterable[RT]): PotentialValueAcceptor[RT] =
    PotentialValueAcceptor {
      case PotentialValue.Clipboard(cd) => parseText(choices, cd.text)
      case PotentialValue.Text(txt)     => parseText(choices, txt)
      case PotentialValue.Emptiness     => None
    }

  private def parseText(choices: Iterable[RT], t: String): Option[RT] = {
    val input = Grammar.reqTypeMnemonic.caseInsensitiveParsePost(t.takeWhile(_ != ':').trim)
    choices.find(_.mnemonic.value ==* input)
  }

  def pxCustomReqTypes(p: Px[Project]): Px[Set[RT]] =
    p.map(_.config.reqTypes.custom.values.toSet)

  def pxChoices(initial: RT, pxCustomReqTypes: Px[Set[RT]]): Px[NonEmptySet[RT]] =
    pxCustomReqTypes
      .map(_.filter(_.live is Live))
      .map(NonEmptySet(initial, _))
}
