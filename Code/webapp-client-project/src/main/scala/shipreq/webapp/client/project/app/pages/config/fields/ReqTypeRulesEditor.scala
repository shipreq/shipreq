package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import monocle.Lens
import monocle.macros.Lenses
import nyaya.util.Multimap
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/, \/-}
import scalaz.std.option._
import scalaz.std.list._
import scalaz.syntax.traverse._
import shipreq.base.util._
import shipreq.webapp.base.data.{Colour => _, _}
import shipreq.webapp.base.data.FieldReqTypeRules.Resolution
import shipreq.webapp.base.lib.ReactKeyGen
import shipreq.webapp.base.lib.ReactKeyGen.UnivEqImplicits._
import shipreq.webapp.base.ui.GeneralTheme
import shipreq.webapp.base.ui.semantic._
import shipreq.webapp.client.project.app.Style.{fieldConfig => *}
import shipreq.webapp.client.project.lib.DataReusability._

object ReqTypeRulesEditor {

  val NoDefault = new ReqTypeRulesEditor[Impossible](allowDefaults = false)

  final class Validation[D](state: State[D], reqTypes: ReqTypes) {
    import shipreq.webapp.base.validation.Simple.Invalidity
    import shipreq.webapp.base.validation._

    private val mnemonic = DataValidators.reqTypeAuditor(reqTypes)

    val stringValidator: Composite.Stateless[String, Vector[String \/ String], Vector[ReqTypeId]] =
      DataValidators.reqTypeSeqStr(mnemonic)
        .appendInvalidator(CommonValidation.invalidator.nonEmptyVector)

    private val pass1: Vector[Invalidity \/ Vector[ReqTypeId]] =
      state.perReqType.map(s => stringValidator.unnamed(s.text))

    private var allWithRows = Multimap.empty[ReqTypeId, Set, Int]
    for {
      i <- pass1.indices
      ids <- pass1(i)
    } allWithRows = allWithRows.addks(ids.toSet, i)

    private val dups =
      allWithRows.iterator.filter(_._2.size > 1).map(_._1).toSet

    val all: Set[ReqTypeId] =
      allWithRows.keySet

    val results: Vector[Invalidity \/ Set[ReqTypeId]] =
      pass1.indices.iterator.map { i =>
        pass1(i) match {
          case \/-(ids) =>
            val localDups = ids.iterator.filter(dups.contains).toSet
            if (localDups.isEmpty)
              \/-(ids.toSet)
            else
              -\/(Invalidity(s"Defined elsewhere: ${reqTypes.makeSeqStr(localDups, ", ")}"))
          case e@ -\/(_) => e
        }
      }.toVector

    private def validateRes(value: State.ResValue[D]): Option[Resolution[D]] =
      value.res match {
        case Resolution.NotApplicable => Some(Resolution.NotApplicable)
        case Resolution.Optional      => Some(Resolution.Optional)
        case Resolution.Mandatory     => Some(Resolution.Mandatory)
        case Resolution.DefaultTo(_)  => value.default.map(Resolution.DefaultTo(_))
      }

    def resultWhenValid: Option[FieldReqTypeRules[D]] = {
      val perReqTypeO: Option[List[(Resolution[D], Set[ReqTypeId])]] =
        results.indices.iterator.map { i =>
          for {
            reqTypeIds <- results(i).toOption
            row         = state.perReqType(i)
            res        <- validateRes(row.res)
          } yield (res, Util.mergeSets(reqTypeIds, row.deadReqTypes))
        }.toList.sequence

      for {
        perReqType <- perReqTypeO
        otherwise  <- validateRes(state.otherwise)
      } yield FieldReqTypeRules.ByResolution.build(perReqType, otherwise).toRules
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  final case class Props[D](state        : StateSnapshot[State[D]],
                            reqTypes     : ReqTypes,
                            renderDefault: D ~=> VdomNode,
                            defaults     : Vector[D],
                            filterDead   : FilterDead,
                            enabled      : Enabled) {

    lazy val validation = state.value.validation(reqTypes)
  }

  object Props {

    def noDefaults(state     : StateSnapshot[State[Impossible]],
                   reqTypes  : ReqTypes,
                   filterDead: FilterDead,
                   enabled   : Enabled): Props[Impossible] =
      apply(
        state         = state,
        reqTypes      = reqTypes,
        renderDefault = renderImpossible,
        defaults      = Vector.empty,
        filterDead    = filterDead,
        enabled       = enabled,
      )

    private val renderImpossible: Impossible ~=> VdomNode =
      Reusable.always(_.impossible)

    implicit def reusabilityProps[D: UnivEq]: Reusability[Props[D]] = {
      implicit val a: Reusability[Vector[D]] = Reusability.byRefOrUnivEq
      Reusability.derive
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  @Lenses
  final case class State[D](perReqType: Vector[State.PerReqType[D]], otherwise: State.ResValue[D]) {

    def validation(reqTypes: ReqTypes): Validation[D] =
      new Validation(this, reqTypes)

    val allDead: Set[ReqTypeId] =
      perReqType.foldLeft(Set.empty[ReqTypeId])((q, p) => Util.mergeSets(q, p.deadReqTypes))

    def addRow: State[D] =
      State(perReqType :+ State.PerReqType.empty[D], otherwise)

    def delRow(idx: Int): State[D] =
      State(perReqType.delete(idx).getOrElse(perReqType), otherwise)
  }

  object State {
    def empty[D]: State[D] =
      apply(Vector.empty, State.ResValue.empty)

    def init[D](cfg: ProjectConfig, rules: FieldReqTypeRules.ByResolution[D]): State[D] = {
      val rows =
        MutableArray(rules.perRes.iterator.map { case (res, ids) => PerReqType.from(cfg, ids, res) })
          .sortBy(_.text)
          .iterator
          .toVector
      apply(rows, ResValue.from(rules.otherwise))
    }

    private val keyGen = new ReactKeyGen

    @Lenses
    final case class PerReqType[D](text: String, deadReqTypes: Set[ReqTypeId], res: ResValue[D], key: Key)

    object PerReqType {
      def empty[D]: PerReqType[D] =
        apply("", Set.empty, ResValue.empty, keyGen.next())

      def from[D](cfg: ProjectConfig, ids: NonEmptySet[ReqTypeId], res: Resolution[D]): PerReqType[D] = {
        def reqTypes(live: Live) = ids.iterator.map(cfg.reqTypes.need).filter(_.live is live)
        val txt = MutableArray(reqTypes(Live).map(_.mnemonic.value)).sort.mkString(", ")
        val dead = reqTypes(Dead).map(_.reqTypeId).toSet
        apply(txt, dead, ResValue.from(res), keyGen.next())
      }
    }

    final case class ResValue[D](res: Resolution[Unit], default: Option[D])

    object ResValue {
      def empty[D]: ResValue[D] =
        apply(Resolution.default, None)

      def from[D](res: Resolution[D]): ResValue[D] =
        res match {
          case Resolution.DefaultTo(d)     => apply(Resolution.DefaultTo(()), Some(d))
          case r@ Resolution.Mandatory     => apply(r, None)
          case r@ Resolution.Optional      => apply(r, None)
          case r@ Resolution.NotApplicable => apply(r, None)
        }
    }

    implicit def univEqV[D: UnivEq]: UnivEq[ResValue  [D]] = UnivEq.derive
    implicit def univEqP[D: UnivEq]: UnivEq[PerReqType[D]] = UnivEq.derive
    implicit def univEqS[D: UnivEq]: UnivEq[State     [D]] = UnivEq.derive

    implicit def reusabilityV[D: UnivEq]: Reusability[ResValue  [D]] = Reusability.byRefOrUnivEq
    implicit def reusabilityP[D: UnivEq]: Reusability[PerReqType[D]] = Reusability.byRefOrUnivEq
    implicit def reusabilityS[D: UnivEq]: Reusability[State     [D]] = Reusability.byRefOrUnivEq

    def perReqTypeRow[D](idx: Int): Lens[State[D], PerReqType[D]] =
      perReqType[D] ^|-> Optics.vectorElementUnsafe[PerReqType[D]](idx)
  }

  // -------------------------------------------------------------------------------------------------------------------

  private[fields] object Internals {
    val resOptionKey: Resolution[Any] => Select.OptionKey = {
      case Resolution.DefaultTo(_)  => "d"
      case Resolution.NotApplicable => "n"
      case Resolution.Mandatory     => "m"
      case Resolution.Optional      => "o"
    }

    final val otherNew = "future req types"
  }
}

// =====================================================================================================================

final class ReqTypeRulesEditor[D: UnivEq](allowDefaults: Boolean) {

  type Props           = ReqTypeRulesEditor.Props[D]
  type State           = ReqTypeRulesEditor.State[D]
  type StatePerReqType = ReqTypeRulesEditor.State.PerReqType[D]
  type StateResValue   = ReqTypeRulesEditor.State.ResValue[D]

  final class Backend($: BackendScope[Props, Unit]) {
    import ReqTypeRulesEditor.Internals._

    private val header =
      <.thead(
        <.tr(
          <.th("Req Types"),
          <.th("Rule"),
          <.th(*.rulesEditorButton)))

    private val newRowButton =
      Enabled.memo(e =>
        Button(tipe = Button.Type.BasicIconOnly(Icon.Plus), colour = Colour.Green)
          .disableMaybe(e)
          .onClick($.props.flatMap(_.state.modState(_.addRow))))

    private val delRowButton =
      Button(tipe = Button.Type.BasicIconOnly(Icon.Trash), colour = ColourPlus.Negative)

    private val resOptions: List[Select.Option[Resolution[Unit]]] = {
      def option(title: String, res: Resolution[Unit]): Select.Option[Resolution[Unit]] =
        Select.Option(resOptionKey(res), title, res)

      def defaultTo     = option("Default to…", Resolution.DefaultTo(()))
      val mandatory     = option("Mandatory", Resolution.Mandatory)
      val notApplicable = option("Not applicable", Resolution.NotApplicable)
      val optional      = option("Optional", Resolution.Optional)

      if (allowDefaults)
        List(defaultTo, mandatory, notApplicable, optional)
      else
        List(mandatory, notApplicable, optional)
    }

    private val dropdownItemSelected =
      VdomAttr("data-value") := "1"

    private val perReqTypeLens: Int => Lens[State, StatePerReqType] =
      Memo.int(idx => ReqTypeRulesEditor.State.perReqTypeRow[D](idx))

    def render(p: Props): VdomNode = {
      val s = p.state.value
      val showDead = p.filterDead is ShowDead
      val validation = p.validation

      def delRowButton(idx: Int) =
        this.delRowButton
          .disableMaybe(p.enabled)
          .onClick(p.state.modState(_.delRow(idx)))

      @UsesSemanticUiManually
      def renderRes(ss: StateSnapshot[StateResValue]): TagMod = {

        val resSelect =
          Select(
            options = resOptions,
            selected = resOptionKey(ss.value.res))(
            onChange = o => ss.modState(_.copy(res = o.value)))

        def defaultSelect = {
          val default = ss.value.default

          def item(d: D) =
            <.div(
              ^.cls := "item",
              dropdownItemSelected.when(default.exists(_ ==* d)),
              p.renderDefault(d))

          <.div(
            *.rulesEditorDefault,
            ^.cls := "ui selection dropdown",
            (^.cls := "error").when(default.isEmpty),
            <.input.hidden(^.value := "1"),
            Icon.Dropdown.tag,
            <.div(^.cls := "default text", "&nbsp;"),
            <.div(^.cls := "menu", p.defaults.toTagMod(item)))
        }

        if (ss.value.res.isDefault)
          TagMod(resSelect, defaultSelect)
        else
          resSelect
      }

      def renderPerReqType(idx: Int): VdomTagOf[html.TableRow] = {
        val row = s.perReqType(idx)
        val lens = perReqTypeLens(idx)
        val ss = p.state.zoomStateL(lens)

        def onChange(e: ReactEventFromInput): Callback = {
          val t = validation.stringValidator.corrector.live(e.target.value)
          ss.modState(_.copy(text = t))
        }

        val reqTypesValidated = validation.results(idx)

        val deadReqTypes =
          Option.when(showDead & row.deadReqTypes.nonEmpty) {
            <.div(*.rulesDeadReqTypes,
              "Dead req types:",
              <.span(*.rulesDeadReqTypesInner,
                p.reqTypes.makeSeqStr(row.deadReqTypes, ", ")))
          }

        val reqTypes =
          Input.Text(
            TagMod(
              ^.value := row.text,
              ^.onChange ==> onChange
            ),
            deadReqTypes.whenDefined,
            GeneralTheme.renderSimpleInvalidity(reqTypesValidated)
          )

        <.tr(
          ^.key := row.key,
          <.td(reqTypes),
          <.td(renderRes(ss.zoomStateL(ReqTypeRulesEditor.State.PerReqType.res))),
          <.td(*.rulesEditorButton, delRowButton(idx)))
      }

      def renderOtherwise(ss: StateSnapshot[StateResValue]): VdomTagOf[html.TableRow] = {

        def fullReqTypeDesc =
          MutableArray(
            p.reqTypes
              .all
              .iterator
              .filter(rt => p.filterDead.filter(rt.live))
              .filter(rt => !validation.all.contains(rt.reqTypeId))
              .filter(rt => !s.allDead.contains(rt.reqTypeId))
          )
            .sortBy(_.mnemonic.value)
            .iterator
            .map[VdomNode](rt =>
              if (rt.live is Live) rt.mnemonic.value else <.span(*.rulesOtherDeadReqType, rt.mnemonic.value))
            .++(Iterator.single[VdomNode](otherNew))
            .mkTagMod(TagMod("Other", <.br, "("), ", ", ")")

        val desc: TagMod =
          if (s.perReqType.isEmpty)
            "All"
          else
            fullReqTypeDesc

        <.tr(
          ^.key := "o",
          <.td(*.rulesEditorOtherwise, desc),
          <.td(renderRes(ss)),
          <.td(*.rulesEditorButton, newRowButton(p.enabled)))
      }

      <.table(
        *.rulesEditor,
        header,
        <.tbody(
          s.perReqType.indices.toVdomArray(i => renderPerReqType(i)),
          renderOtherwise(p.state.zoomStateL(ReqTypeRulesEditor.State.otherwise))))
    }
  }

  val Component = ScalaComponent.builder[Props]("ReqTypeRulesEditor")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}