package shipreq.webapp.client.project.widgets

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
import scala.annotation.nowarn
import scala.collection.immutable.ArraySeq
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/, \/-}
import scalaz.std.option._
import scalaz.std.list._
import scalaz.syntax.traverse._
import shipreq.base.util._
import shipreq.webapp.base.data.{Colour => _, _}
import shipreq.webapp.base.data.FieldReqTypeRules.Resolution
import shipreq.webapp.base.feature.AutoCompleteFeature._
import shipreq.webapp.base.lib.ReactKeyGen
import shipreq.webapp.base.lib.ReactKeyGen.UnivEqImplicits._
import shipreq.webapp.base.ui.GeneralTheme
import shipreq.webapp.base.ui.semantic.{Dropdown => _, _}
import shipreq.webapp.client.project.app.Style.{fieldConfig => *}
import shipreq.webapp.base.ui.widgets.Dropdown
import shipreq.webapp.client.project.lib.DataReusability._

object ReqTypeRulesEditor {

  val NoDefault            = new ReqTypeRulesEditor[Impossible](allowDefaults = false, keyFor = _.impossible)
  val ApplicableTagDefault = new ReqTypeRulesEditor[ApplicableTagId](allowDefaults = true, keyFor = _.value.toString)

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
              -\/(Invalidity(s"Defined elsewhere: ${reqTypes.mkStringByIds(localDups, ", ")}"))
          case e@ -\/(_) => e
        }
      }.toVector

    private def validateRes(value: State.ResValue[D], legalOptions: => Set[D]): Option[Resolution[D]] =
      value.res match {
        case Resolution.NotApplicable => Some(Resolution.NotApplicable)
        case Resolution.Optional      => Some(Resolution.Optional)
        case Resolution.Mandatory     => Some(Resolution.Mandatory)
        case Resolution.DefaultTo(_)  => value.legalDefault(legalOptions).map(Resolution.DefaultTo(_))
      }

    @nowarn("cat=unused")
    def resultWhenValidI(implicit ev: D =:= Impossible): Option[FieldReqTypeRules[D]] =
      resultWhenValid(Set.empty)

    def resultWhenValid(legalOptions: => Set[D]): Option[FieldReqTypeRules[D]] = {
      lazy val _legalOptions = legalOptions

      val deadO: Option[List[(Resolution[D], Set[ReqTypeId])]] =
        state.dead.traverse(d => validateRes(d.res, _legalOptions).map((_, d.ids.whole)))

      def perReqTypeO: Option[List[(Resolution[D], Set[ReqTypeId])]] =
        results.indices.iterator.map { i =>
          for {
            reqTypeIds <- results(i).toOption
            row         = state.perReqType(i)
            res        <- validateRes(row.res, _legalOptions)
          } yield (res, reqTypeIds)
        }
          .toList
          .sequence

      for {
        dead       <- deadO
        perReqType <- perReqTypeO
        otherwise  <- validateRes(state.otherwise, _legalOptions)
      } yield FieldReqTypeRules.ByResolution.build(perReqType.iterator ++ dead, otherwise).toRules
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  final case class Props[D](state        : StateSnapshot[State[D]],
                            reqTypes     : ReqTypes,
                            renderDefault: D ~=> VdomNode,
                            defaults     : ArraySeq[D],
                            filterDead   : FilterDead,
                            enabled      : Enabled) {

    lazy val defaultSet = defaults.toSet
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
        defaults      = ArraySeq.empty,
        filterDead    = filterDead,
        enabled       = enabled,
      )

    private val renderImpossible: Impossible ~=> VdomNode =
      Reusable.always(_.impossible)

    implicit def reusabilityProps[D: UnivEq]: Reusability[Props[D]] = {
      implicit val a: Reusability[ArraySeq[D]] = Reusability.byRefOrUnivEq
      locally(a) // -Wunused:locals gets it wrong
      Reusability.derive
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  @Lenses
  final case class State[D](dead      : List[State.DeadRow[D]],
                            perReqType: Vector[State.PerReqType[D]],
                            otherwise : State.ResValue[D]) {

    def validation(reqTypes: ReqTypes): Validation[D] =
      new Validation(this, reqTypes)

    val allDead: Set[ReqTypeId] =
      dead.foldLeft(Set.empty[ReqTypeId])((q, p) => Util.mergeSets(q, p.ids.whole))

    def addRow: State[D] =
      copy(perReqType = perReqType :+ State.PerReqType.empty[D])

    def delRow(idx: Int): State[D] =
      copy(perReqType = perReqType.delete(idx).getOrElse(perReqType))
  }

  object State {
    def empty[D]: State[D] =
      apply(Nil, Vector.empty, State.ResValue.empty)

    def init[D](cfg: ProjectConfig, rules: FieldReqTypeRules.ByResolution[D]): State[D] = {
      val dead: List[DeadRow[D]] =
        MutableArray(
          rules.perRes.iterator.flatMap { case (res, ids) =>
            val deadIds = ids.whole.filter(cfg.reqTypes.live(_, Live) is Dead)
            NonEmptySet.option(deadIds).iterator.map(i =>
              DeadRow(i, ResValue.from(res), keyGen.next())
            )
          }
        )
          .sortBySchwartzian(x => cfg.reqTypes.sortIdsByMnemonic(x.ids.whole).mkString(","))
          .iterator
          .toList

      val rows: Vector[PerReqType[D]] =
        MutableArray(
          rules.perRes
            .iterator
            .map { case (res, ids) => PerReqType.from(cfg, ids, res) }
            .filter(_.text.nonEmpty)
        )
          .sortBy(_.text)
          .iterator
          .toVector

      val otherwise =
        ResValue.from(rules.otherwise)

      apply(dead, rows, otherwise)
    }

    private val keyGen = new ReactKeyGen

    @Lenses
    final case class DeadRow[D](ids: NonEmptySet[ReqTypeId], res: ResValue[D], key: Key)

    @Lenses
    final case class PerReqType[D](text: String, res: ResValue[D], key: Key)

    object PerReqType {
      def empty[D]: PerReqType[D] =
        apply("", ResValue.empty, keyGen.next())

      def from[D](cfg: ProjectConfig, ids: NonEmptySet[ReqTypeId], res: Resolution[D]): PerReqType[D] = {
        def reqTypes(live: Live) = ids.iterator.flatMap(cfg.reqTypes.get).filter(_.live is live)
        val txt = cfg.reqTypes.mkString(reqTypes(Live), ", ")
        apply(txt, ResValue.from(res), keyGen.next())
      }
    }

    final case class ResValue[D](res: Resolution[Unit], default: Option[D]) {
      def legalDefault(legalOptions: Set[D]): Option[D] =
        default.filter(legalOptions.contains)
    }

    object ResValue {
      def empty[D]: ResValue[D] =
        apply(Resolution.default, None)

      def from[D](res: Resolution[D]): ResValue[D] =
        res match {
          case Resolution.DefaultTo(d)          => apply(Resolution.DefaultTo(()), Some(d))
          case r: Resolution.Mandatory.type     => apply(r, None)
          case r: Resolution.Optional.type      => apply(r, None)
          case r: Resolution.NotApplicable.type => apply(r, None)
        }
    }

    implicit def univEqD[D: UnivEq]: UnivEq[DeadRow   [D]] = UnivEq.derive
    implicit def univEqV[D: UnivEq]: UnivEq[ResValue  [D]] = UnivEq.derive
    implicit def univEqP[D: UnivEq]: UnivEq[PerReqType[D]] = UnivEq.derive
    implicit def univEqS[D: UnivEq]: UnivEq[State     [D]] = UnivEq.derive

    implicit def reusabilityD[D: UnivEq]: Reusability[DeadRow   [D]] = Reusability.byRefOrUnivEq
    implicit def reusabilityV[D: UnivEq]: Reusability[ResValue  [D]] = Reusability.byRefOrUnivEq
    implicit def reusabilityP[D: UnivEq]: Reusability[PerReqType[D]] = Reusability.byRefOrUnivEq
    implicit def reusabilityS[D: UnivEq]: Reusability[State     [D]] = Reusability.byRefOrUnivEq

    def perReqTypeRow[D](idx: Int): Lens[State[D], PerReqType[D]] =
      perReqType[D] ^|-> Optics.vectorElementUnsafe[PerReqType[D]](idx)
  }

  // -------------------------------------------------------------------------------------------------------------------

  object Internals {
    val resOptionKey: Resolution[Any] => Dropdown.ItemKey = {
      case Resolution.DefaultTo(_)  => "d"
      case Resolution.NotApplicable => "n"
      case Resolution.Mandatory     => "m"
      case Resolution.Optional      => "o"
    }

    final val otherNew = "future req types"
  }
}

// =====================================================================================================================

final class ReqTypeRulesEditor[D: UnivEq](allowDefaults: Boolean, keyFor: D => String) {

  type Props           = ReqTypeRulesEditor.Props[D]
  type State           = ReqTypeRulesEditor.State[D]
  type StateDeadRow    = ReqTypeRulesEditor.State.DeadRow[D]
  type StatePerReqType = ReqTypeRulesEditor.State.PerReqType[D]
  type StateResValue   = ReqTypeRulesEditor.State.ResValue[D]
  type Validation      = ReqTypeRulesEditor.Validation[D]

  final class Backend($: BackendScope[Props, Unit]) {
    import ReqTypeRulesEditor.Internals._

    private val pxState: Px[State] =
      Px.props($).map(_.state.value).withReuse.autoRefresh

    private val pxReqTypes: Px[ReqTypes] =
      Px.props($).map(_.reqTypes).withReuse.autoRefresh

    private val pxValidation: Px[Validation] =
      for {
        s <- pxState
        r <- pxReqTypes
      } yield s.validation(r)

    private val rowAutoComplete: Int => RowAutoComplete =
      Memo.int(new RowAutoComplete(_))

    private val pxAllText: Px[String] =
      Px.props($).map(_.state.value.perReqType.iterator.map(_.text).mkString(" ")).withReuse.autoRefresh

    private val pxReqTypesInAllText: Px[Set[String]] =
      (for {
        text <- pxAllText
        vali <- pxValidation
      } yield vali.stringValidator.corrector(text).iterator.flatMap(_.toOption).toSet
      ).withReuse

    private class RowAutoComplete(i: Int) {

      private val pxAutoComplete: Px[AutoComplete.Strategies] =
        for {
          rt <- pxReqTypes
          ex <- pxReqTypesInAllText
        } yield AutoComplete.Project.reqTypeMnemonics(rt, ex)

      val render =
        AutoComplete.InputComponent(pxAutoComplete.toCallback) _
    }

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

    private val resOptions: ArraySeq[Dropdown.Item[Resolution[Unit]]] = {
      def option(title: String, res: Resolution[Unit]): Dropdown.Item[Resolution[Unit]] =
        Dropdown.Item(resOptionKey(res), title, res)

      def defaultTo     = option("Default to…", Resolution.DefaultTo(()))
      val mandatory     = option("Mandatory", Resolution.Mandatory)
      val notApplicable = option("Not applicable", Resolution.NotApplicable)
      val optional      = option("Optional", Resolution.Optional)

      if (allowDefaults)
        ArraySeq(defaultTo, mandatory, notApplicable, optional)
      else
        ArraySeq(mandatory, notApplicable, optional)
    }

    private val perReqTypeLens: Int => Lens[State, StatePerReqType] =
      Memo.int(idx => ReqTypeRulesEditor.State.perReqTypeRow[D](idx))

    def render(p: Props): VdomNode = {
      val s          = p.state.value
      val validation = pxValidation.value()

      def delRowButton(idx: Int) =
        this.delRowButton
          .disableMaybe(p.enabled)
          .onClick(p.state.modState(_.delRow(idx)))

      @UsesSemanticUiManually
      def renderRes(ss: StateSnapshot[StateResValue], enabled: Enabled): TagMod = {

        val resSelect =
          Dropdown.Props.Optional(
            items    = resOptions,
            enabled  = enabled,
            selected = Some(resOptionKey(ss.value.res)))(
            onChange = o => ss.modState(_.copy(res = o.value))
          ).render

        def defaultSelect = {
          val default = ss.value.legalDefault(p.defaultSet)

          val defaultItems =
            p.defaults.map(d => Dropdown.Item(keyFor(d), p.renderDefault(d), d))

          Dropdown.Props.Optional(
            items    = defaultItems,
            enabled  = enabled,
            tagMod   = *.rulesEditorDefault,
            validity = Invalid when default.isEmpty,
            selected = default.map(keyFor))(
            onChange = o => ss.modState(_.copy(default = Some(o.value)))
          ).render
        }

        if (ss.value.res.isDefault)
          TagMod(resSelect, defaultSelect)
        else
          resSelect
      }

      def renderDeadRow(row: StateDeadRow): VdomTagOf[html.TableRow] = {
        val reqTypes =
          p.reqTypes.sortIdsByMnemonic(row.ids.whole)
            .map(rt => <.span(*.rulesDeadReqTypesInner, rt.mnemonic.value))
            .mkTagMod(", ")

        <.tr(
          ^.key := row.key,
          <.td(*.rulesDeadReqTypes, "Dead req types:", reqTypes),
          <.td(*.rulesEditorRule, renderRes(StateSnapshot(row.res).readOnly, Disabled)),
          <.td(*.rulesEditorButton))
      }

      def renderPerReqType(idx: Int): VdomTagOf[html.TableRow] = {
        val row          = s.perReqType(idx)
        val lens         = perReqTypeLens(idx)
        val ss           = p.state.zoomStateL(lens)
        val autoComplete = rowAutoComplete(idx)

        def onChange(e: ReactEventFromInput): Callback = {
          val t = validation.stringValidator.corrector.live(e.target.value)
          ss.modState(_.copy(text = t))
        }

        val reqTypesValidated = validation.results(idx)

        val reqTypes =
          autoComplete.render(autoCompletion =>
            <.div(
              *.rulesEditorReqTypes,
              Input.Text.withError(
                input = TagMod(
                  ^.value := row.text,
                  ^.onChange ==> onChange,
                  ^.spellCheck := false,
                  autoCompletion,
                ),
                error = GeneralTheme.renderSimpleInvalidity(reqTypesValidated),
                enabled = p.enabled)))

        <.tr(
          ^.key := row.key,
          <.td(reqTypes),
          <.td(*.rulesEditorRule, renderRes(ss.zoomStateL(ReqTypeRulesEditor.State.PerReqType.res), p.enabled)),
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

        val deadRowsVisible =
          p.filterDead.is(ShowDead) && s.dead.nonEmpty

        val desc: TagMod =
          if (s.perReqType.isEmpty && !deadRowsVisible)
            "All"
          else
            fullReqTypeDesc

        <.tr(
          ^.key := "o",
          <.td(*.rulesEditorOtherwise, desc),
          <.td(*.rulesEditorRule, renderRes(ss, p.enabled)),
          <.td(*.rulesEditorButton, newRowButton(p.enabled)))
      }

      <.table(
        *.rulesEditor,
        header,
        <.tbody(
          TagMod.when(p.filterDead is ShowDead)(s.dead.toVdomArray(renderDeadRow)),
          s.perReqType.indices.toVdomArray(renderPerReqType),
          renderOtherwise(p.state.zoomStateL(ReqTypeRulesEditor.State.otherwise))))
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}