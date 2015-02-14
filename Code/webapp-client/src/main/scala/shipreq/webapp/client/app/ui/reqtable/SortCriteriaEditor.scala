package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom
import scalaz.Memo
import scalaz.effect.IO
import scalaz.std.option._
import scalaz.syntax.equal._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.Util
import shipreq.webapp.client.app.ui.SelectOne
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
      .initialState(DND.Parent.initialState[Column])
      .backend(new Backend(_))
      .render(_.backend.render)
      .domType[dom.html.Div]
      .build

  final class Backend($: BackendScope[Props, DND.Parent.PState[Column]]) {

    def render = {
      val p = $.props

      def modIO(f: EndoFn[Vector[SortCriterion.Inconclusive]], g: EndoFn[SortCriterion.Conclusive]): IO[Unit] = {
        val s = p.value
        p change SortCriteria(f(s.init), g(s.last))
      }

      val li = inconclusive.li(_: Column.SortInconclusive, _: inconclusive.OSM, p.columnName, modIO(_, identity))

      var lis            = Vector.empty[ReactElement]
      var activeCols     = Set.empty[Column]
      var conclusiveCols = Set.empty[Column.SortConclusive]

      // Active criteria
      p.value.init.foreach(s =>
        if (p.sortableColumns.contains(s.column)) {
          activeCols += s.column
          lis :+= li(s.column, s.method.some)
        })

      // Inactive criteria
      Util.filterOutAndSortByName(p.sortableColumns)(activeCols.contains, p.columnName.fn).foreach{
        case c: Column.SortConclusive   => conclusiveCols += c
        case c: Column.SortInconclusive => lis :+= li(c, None)
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
      type OSM = Option[SortMethod]
      type ModIO = EndoFn[Vector[SortCriterion.Inconclusive]] => IO[Unit]

      private val unusedChoice: Choice[OSM] =
        Choice(None, "Unused", false) // English

      private val choicesForColumn =
        Memo.mutableHashMapMemo[Column.SortInconclusive, Vector[Choice[OSM]]](c =>
          unusedChoice +:
            SortMethod.valuesAllowed(c).map(m =>
              Choice(m.some, m.optionLabel, false)))

      private val smSelectComponent = SelectOne.Component[OSM]

      def li(column: Column.SortInconclusive, value: OSM, columnName: Column.NameResolver, modIO: ModIO): ReactElement = {

        def sortMethodDropdown = {
          def changeIO(o: OSM): IO[Unit] = {
            def isSubjectColumn: SortCriterion.Inconclusive => Boolean =
              _.column ≟ column

            def remove =
              modIO(_ filterNot isSubjectColumn)

            def set(m: SortMethod) =
              modIO(ss => {
                val i = ss indexWhere isSubjectColumn
                if (i >= 0)
                  ss.updated(i, ss(i).copy(method = m))
                else
                  ss :+ SortCriterion.Inconclusive(column, m)
              })

            o.fold(remove)(set)
          }

          smSelectComponent(
            SelectOne.Props(
              selected = value,
              choices  = choicesForColumn(column),
              select   = Some(changeIO)))
        }

        <.li(
          sortMethodDropdown,
          columnName(column))
      }
    }

    // =================================================================================================================
    object conclusive {
      type Col   = Column.SortConclusive
      type SM    = SortMethod.IgnoreBlanks
      type SC    = SortCriterion.Conclusive
      type ModIO = EndoFn[SC] => IO[Unit]

      private val smChoices: Vector[Choice[SM]] =
        SortMethod.ignoreBlanks.map(m =>
          Choice[SM](m, m.optionLabel, false))

      private val smSelectComponent = SelectOne.Component[SM]

      def sortMethodDropdown(value: SM, modIO: ModIO): ReactElement =
        smSelectComponent(
          SelectOne.Props(
            selected = value,
            choices  = smChoices,
            select   = Some(v => modIO(_.copy(method = v)))))

      private val colSelectComponent = SelectOne.Component[Col]

      def columnDropdown(value: Col, cols: Set[Col], columnName: Column.NameResolver, modIO: ModIO): ReactElement = {
        val colChoices =
          cols.foldLeft(Vector.empty[Choice[Col]])((q, c) => q :+ Choice(c, columnName(c), false))
            .sortBy(_.label)

        colSelectComponent(
          SelectOne.Props(
            selected = value,
            choices  = colChoices,
            select   = Some(v => modIO(_.copy(column = v)))))
      }

      def ctrls(value: SC, cols: Set[Col], columnName: Column.NameResolver, modIO: ModIO) =
        <.div(
          conclusive.sortMethodDropdown(value.method, modIO),
          conclusive.columnDropdown(value.column, cols, columnName, modIO))
    }
  }
}