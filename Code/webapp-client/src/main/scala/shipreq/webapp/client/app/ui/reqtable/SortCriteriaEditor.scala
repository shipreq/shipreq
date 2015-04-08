package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom
import scalaz._
import scalaz.effect.IO
import scalaz.std.option.optionEqual
import scalaz.syntax.equal._
import scalaz.syntax.foldable.ToFoldableOps
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{UnivEq, Util}
import shipreq.webapp.client.app.ui.SelectOne
import shipreq.webapp.client.app.ui.Style.reqtable.{sortingSettings => *}
import shipreq.webapp.client.util.DND

object SortCriteriaEditor {

  case class Props(value          : SortCriteria,
                   sortableColumns: Set[Column],
                   columnName     : Column.NameResolver,
                   change         : SortCriteria => IO[Unit]) {
    @inline def component = Component(this)
  }

  val Component =
    ReactComponentB[Props]("SortCriteriaEditor")
      .initialState(DND.Parent.initialState[Column.SortInconclusive])
      .backend(new Backend(_))
      .render(_.backend.render)
      .domType[dom.html.Div]
      .build

  final class Backend($: BackendScope[Props, DND.Parent.PState[Column.SortInconclusive]]) {

    def render = {
      val p = $.props

      def modIO(f: EndoFn[Vector[SortCriterion.Inconclusive]], g: EndoFn[SortCriterion.Conclusive]): IO[Unit] = {
        val s = p.value
        p change SortCriteria(f(s.init), g(s.last))
      }

      val li = inconclusive.li(_: inconclusive.OSM, p.columnName, modIO(_, identity))

      var lis            = Vector.empty[ReactElement]
      var activeCols     = UnivEq.emptySet[Column]
      var conclusiveCols = UnivEq.emptySet[Column.SortConclusive]

      // Active criteria
      p.value.init.foreach(sc =>
        if (p.sortableColumns.contains(sc.column)) {
          activeCols += sc.column
          lis :+= li(\/-(sc))
        })

      // Inactive criteria
      Util.filterOutAndSortByName(p.sortableColumns)(activeCols.contains, p.columnName.fn).foreach {
        case c: Column.SortInconclusive => lis :+= li(-\/(c))
        case c: Column.SortConclusive   => conclusiveCols += c
      }

      <.div(^.cls := "sortCriteriaEd",
        <.ol(lis),
        <.div(
          "And finally",
          conclusive.ctrls(p.value.last, conclusiveCols, p.columnName, modIO(identity, _))))
    }

    import SelectOne.Choice

    // =================================================================================================================
    object inconclusive {
      type SC    = SortCriterion.Inconclusive
      type Col   = Column.SortInconclusive
      type OSM   = Col \/ SC
      type ModIO = EndoFn[Vector[SortCriterion.Inconclusive]] => IO[Unit]

      UnivEq[Col] // Prove it's safe to key memo by Col
      private val choicesForColumn =
        Memo.mutableHashMapMemo[Col, Vector[Choice[OSM]]]{c =>
          val choice  = (sc: SC) => Choice[OSM](\/-(sc), sc.method.optionLabel, false)
          val choices = SortCriterion.possibilitiesI(c) map choice
          val unused  = Choice[OSM](-\/(c), "Unused", false) // English
          unused +: choices.whole
        }

      private val smSelectComponent = SelectOne.Component[OSM]

      private def updateIO(modIO: ModIO): OSM => IO[Unit] = {
        case -\/(c) =>
          // Remove
          modIO(_ filterNot (_.column ≟ c))

        case \/-(sc) =>
          // Set
          modIO(ss => {
            val i = ss indexWhere (_.column ≟ sc.column)
            if (i >= 0)
              ss.updated(i, sc)
            else
              ss :+ sc
          })
      }

      private val Row = DND.Child.dndItemComponentB[Col, (OSM, Column.NameResolver, ModIO)]({
        case (outerAttr, draghnd, col, (value, columnName, modIO)) =>

          val on = value.isRight
          val selectProps = SelectOne.Props(
            value, choicesForColumn(col), Some(updateIO(modIO)), *.dirSelect)

          <.li(outerAttr, *.row(on),
            draghnd(*.dragHnd),
            smSelectComponent(selectProps),
            <.span(*.field(on), columnName(col)))
      })

      val columnFromOSM: OSM => Col = {
        case -\/(c) => c
        case \/-(c) => c.column
      }

      def li(value: OSM, columnName: Column.NameResolver, modIO: ModIO): ReactElement = {
        val col = columnFromOSM(value)
        Row((col, DND.Parent.cProps($, col, moveIO(modIO)), (value, columnName, modIO)))
      }

      private def moveIO(modIO: ModIO)(from: Col, to: Col): IO[Unit] =
        modIO(scs =>
          scs.find(_.column ≟ from).map(a =>
            scs.find(_.column ≟ to).fold(
              // Column removed to the Unused section
              scs.filterNot(_ eq a)
            )(b =>
              // Normal move
              DND.move(a, b)(scs)(Equal[Col].contramap(_.column)))
          ) getOrElse scs)
    }

    // =================================================================================================================
    object conclusive {
      type Col   = Column.SortConclusive
      type SM    = SortMethod.IgnoreBlanks
      type SC    = SortCriterion.Conclusive
      type ModIO = EndoFn[SC] => IO[Unit]

      private val smChoices: Vector[Choice[SM]] =
        SortMethod.ignoreBlanks.whole.map(m =>
          Choice[SM](m, m.optionLabel, false))

      private val smSelectComponent = SelectOne.Component[SM]

      def sortMethodDropdown(value: SM, modIO: ModIO): ReactElement = {
        val onSelect = Some((v: SM) => modIO(_.copy(method = v)))
        smSelectComponent(SelectOne.Props(value, smChoices, onSelect, *.conclusiveDir))
      }

      private val colSelectComponent = SelectOne.Component[Col]

      def columnDropdown(value: Col, cols: Set[Col], columnName: Column.NameResolver, modIO: ModIO): ReactElement = {
        val colChoices =
          cols.foldLeft(Vector.empty[Choice[Col]])((q, c) => q :+ Choice(c, columnName(c), false))
            .sortBy(_.label)
        val onSelect = Some((v: Col) => modIO(_.copy(column = v)))
        colSelectComponent(SelectOne.Props(value, colChoices, onSelect, *.conclusiveField))
      }

      def ctrls(value: SC, cols: Set[Col], columnName: Column.NameResolver, modIO: ModIO) =
        <.div(
          conclusive.sortMethodDropdown(value.method, modIO),
          conclusive.columnDropdown(value.column, cols, columnName, modIO))
    }
  }
}