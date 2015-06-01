package shipreq.webapp.client.app.ui.reqtable

import scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom
import scalaz.{Equal, \/, -\/, \/-}
import scalaz.effect.IO
import scalaz.syntax.equal._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{NonEmptySet, UnivEq, Util}
import shipreq.base.util.UnivEq.{mutableHashMapMemo => memo}
import shipreq.webapp.client.app.ui.SelectOne
import shipreq.webapp.client.app.ui.Style.reqtable.{sortCriteriaEditor => *}
import shipreq.webapp.client.util.{On, Enabled, DND}

object SortCriteriaEditor {

  case class Props(value          : SortCriteria,
                   sortableColumns: NonEmptySet[Column],
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
      Util.filterOutAndSortByName(p.sortableColumns.whole)(activeCols.contains, p.columnName.fn).foreach {
        case c: Column.SortInconclusive => lis :+= li(-\/(c))
        case c: Column.SortConclusive   => conclusiveCols += c
      }

      // Too hard (and invasive, and awkward) to prove non-emptyness
      // Would first need to prove ViewSettings always contain a Conclusive column, like:
      // -- class VisibleColumns(a: Vector[Column], b: Column.SortConclusive, c: Vector[Column])
      assert(conclusiveCols.nonEmpty, "No conclusiveCols found?! Impossible.")
      val conclusiveColsN = NonEmptySet(conclusiveCols.head, conclusiveCols.tail)

      <.div(^.cls := "sortCriteriaEd",
        <.ol(lis),
        <.div(
          "And finally",
          conclusive.ctrls(p.value.last, conclusiveColsN, p.columnName, modIO(identity, _))))
    }

    import SelectOne.{Choice, Choices}

    // =================================================================================================================
    object inconclusive {
      type SC    = SortCriterion.Inconclusive
      type Col   = Column.SortInconclusive
      type OSM   = Col \/ SC
      type ModIO = EndoFn[Vector[SortCriterion.Inconclusive]] => IO[Unit]

      private val choicesForColumn =
        memo[Col, Choices[OSM]]{c =>
          val choice  = (sc: SC) => Choice[OSM](\/-(sc), sc.method.optionLabel, Enabled)
          val choices = SortCriterion.possibilitiesI(c) map choice
          val unused  = Choice[OSM](-\/(c), "Unused", Enabled) // English
          unused +: choices
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

          val on = On <~ value.isRight
          val selectProps = SelectOne.Props(
            value, choicesForColumn(col), Some(updateIO(modIO)), *.inconclusiveSortMethod)

          <.li(outerAttr, *.inconclusiveCriterionRow(on),
            draghnd(*.dragHnd),
            smSelectComponent(selectProps),
            <.span(*.inconclusiveColumnName(col.alive, on), columnName(col)))
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

      private val smChoices: Choices[SM] =
        SortMethod.ignoreBlanks.map(m =>
          Choice[SM](m, m.optionLabel, Enabled))

      private val smSelectComponent = SelectOne.Component[SM]

      def sortMethodDropdown(value: SM, modIO: ModIO): ReactElement = {
        val onSelect = Some((v: SM) => modIO(_.copy(method = v)))
        smSelectComponent(SelectOne.Props(value, smChoices, onSelect, *.conclusiveSortMethod))
      }

      private val colSelectComponent = SelectOne.Component[Col]

      def columnDropdown(value: Col, cols: NonEmptySet[Col], columnName: Column.NameResolver, modIO: ModIO): ReactElement = {
        val colChoices =
          cols.toNonEmptyVector.map(c => Choice(c, columnName(c), Enabled))
            .sortBy(_.label)
        val onSelect = Some((v: Col) => modIO(_.copy(column = v)))
        colSelectComponent(SelectOne.Props(value, colChoices, onSelect, *.conclusiveColumnName))
      }

      def ctrls(value: SC, cols: NonEmptySet[Col], columnName: Column.NameResolver, modIO: ModIO) =
        <.div(
          conclusive.sortMethodDropdown(value.method, modIO),
          conclusive.columnDropdown(value.column, cols, columnName, modIO))
    }
  }
}