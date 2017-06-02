package shipreq.webapp.client.project.app.reqtable

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scala.annotation.tailrec
import scalacss.ScalaCssReact._
import scalaz.Scalaz.Id
import scalaz.{-\/, \/-}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.{CustomReqType, ReqType, StaticReqType}
import shipreq.webapp.base.protocol.CreateContentCmd
import shipreq.webapp.client.base.ui.semantic.{Button, Colour, Icon, Table => SemTable}
import shipreq.webapp.client.project.app.Style.reqtable.{creation => *}
import shipreq.webapp.client.project.feature.CreateFeature
import shipreq.webapp.client.project.feature.CreateFeature.FieldKey
import shipreq.webapp.client.project.lib.DataReusability._

object NewForm {

  object ForCodeGroup extends NewForm {
    override type Input            = Unit
    override type FK               = FieldKey.ForCodeGroup
    override val columnToField     = Column.creationFieldCG.getOption
    override val createButtonLabel = Function const NewForm.createButtonLabel(UiText.codeGroup)
    override protected def createCmd(i: Input, o: Output): Option[CreateContentCmd] = {
      var _code: Option[FieldKey.Code.Value] = None
      var title: FieldKey.CodeGroupTitle.Value = Vector.empty
      val fold = FieldKey.FoldForCodeGroup[? => Unit](
        f => (v: f.Value) => _code = Some(v),
        f => (v: f.Value) => title = v)
      o.foreach(_.foldValue(fold))
      for {code <- _code} yield CreateContentCmd.CreateCodeGroup(code, title)
    }
  }

  object ForGenericReq extends NewForm {
    override type Input            = CustomReqType
    override type FK               = FieldKey.ForGenericReq
    override val columnToField     = Column.creationFieldGR.getOption
    override val createButtonLabel = NewForm.createButtonLabel(_)
    override protected def createCmd(i: Input, o: Output): Option[CreateContentCmd] = {
      var c = CreateContentCmd.CreateGenericReq.empty(i.id)
      val fold = FieldKey.FoldForGenericReq[? => Unit](
        codes           = f => (v: f.Value) => c = c.copy(codes = v),
        customTextField = f => (v: f.Value) => c = c.addCustomText(f.field, v),
        implications    = f => (v: f.Value) => c = c.addImps(f.dir, v),
        tags            = f => (v: f.Value) => c = c.addTags(v),
        title           = f => (v: f.Value) => c = c.copy(title = v))
      o.foreach(_.foldValue(fold))
      Some(c)
    }
  }

  object ForUseCase extends NewForm {
    override type Input            = Unit
    override type FK               = FieldKey.ForUseCase
    override val columnToField     = Column.creationFieldUC.getOption
    override val createButtonLabel = Function const NewForm.createButtonLabel(StaticReqType.UseCase)
    override protected def createCmd(i: Input, o: Output): Option[CreateContentCmd] = {
      var c = CreateContentCmd.CreateUseCase.empty
      val fold = FieldKey.FoldForUseCase[? => Unit](
        codes           = f => (v: f.Value) => c = c.copy(codes = v),
        customTextField = f => (v: f.Value) => c = c.addCustomText(f.field, v),
        implications    = f => (v: f.Value) => c = c.addImps(f.dir, v),
        tags            = f => (v: f.Value) => c = c.addTags(v),
        title           = f => (v: f.Value) => c = c.copy(title = v))
      o.foreach(_.foldValue(fold))
      Some(c)
    }
  }

  private def createButtonLabel(name: String): String = "Create " + name
  private def createButtonLabel(rt: ReqType) : String = createButtonLabel(rt.mnemonic.value)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

sealed trait NewForm {

  /** Additional input threaded through to [[CreateContentCmd]] creation. */
  type Input

  /** Subset of FieldKeys that are applicable here */
  type FK <: FieldKey

  protected val columnToField: Column => Option[FK]

  protected val createButtonLabel: Input => String

  protected def createCmd(i: Input, o: Output): Option[CreateContentCmd]

  // ↑ abstract
  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // ↓ concrete

  /** This will contain all values for all fields that:
    * 1. Map to a visible column
    * 2. Are applicable to the creation subject
    */
  final type Output = List[FK#AndValue[Id]]

  final type Editor = FK#AndValue[CreateFeature.ReadWrite.ForEditor]

  sealed case class Props(input        : Input,
                          activeColumns: NonEmptyVector[ColumnPlus],
                          createFeature: CreateFeature.ReadWrite.ForRow[FK],
                          cancel       : Callback) {
    def render: VdomElement = Component(this)

    // TODO test with mandatory columns only
    val editableCols: NonEmptyVector[(ColumnPlus, Editor)] =
      NonEmptyVector.force(
        activeColumns
          .iterator
          .map(cp => columnToField(cp.column).flatMap(f => createFeature(f) match {
            case \/-(e) => Some((cp, f.andValue(e)))
            case -\/(_) => None // Field is N/A
          }))
          .filterDefined
          .toVector)

    val create: Option[Callback] =
      validOutput(editableCols.iterator.map(_._2))
        .flatMap(createCmd(input, _))
        .map(createFeature.create(_))
  }

  /** @return None if any fields have invalid contents */
  private def validOutput(es: Iterator[Editor]): Option[Output] = {
    @tailrec
    def go(o: Output): Option[Output] =
      if (es.isEmpty)
        Some(o)
      else {
        val e = es.next()
        e.value.value() match {
          case \/-(v) => go(e.withValue[Id](v) :: o)
          case -\/(_) => None // Invalidity found -- abort everything
        }
      }
    go(Nil)
  }

  final class Backend($: BackendScope[Props, Unit]) {

    private val cancelButton: VdomElement =
      Button(
        tipe = Button.Type.BasicIconAndText(Icon.Remove, "Close"),
        colour = Colour.Black)
        .tag(*.formCancelButton,
          ^.onClick --> $.props.flatMap(_.cancel))

    def render(p: Props): VdomElement = {

      val editorCells: VdomArray =
        p.editableCols.whole.toVdomArray { case (cp, e) =>
          <.td(
            ^.key := cp.column.key,
            e.value.render())
        }

      val createButton: VdomElement =
        Button(
          tipe = Button.Type.BasicIconAndText(Icon.Plus, createButtonLabel(p.input)),
          colour = Colour.Green,
          state = Button.State.enabledWhen(p.create.isDefined))
          .tag(*.formCreateButton,
            ^.onClick -->? p.create)

      <.section(*.formOuter,
        SemTable.celledCompactUnstackable(
          *.formTable,
          Header.Component(p.editableCols.map(_._1)),
          <.tbody(
            <.tr(*.formMiddleRow,
              editorCells),
            <.tr(
              <.td(*.formBottomRow,
                ^.colSpan := p.editableCols.length,
                cancelButton,
                createButton)))))
    }
  }

  val Component = ScalaComponent.builder[Props]("NewForm")
    .renderBackend[Backend]
    .build

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object Header {

    type Props = NonEmptyVector[ColumnPlus]

    private def render(p: Props): VdomElement =
      <.thead(
        <.tr(
          p.whole.toTagMod(col =>
            <.th(*.formHeaderCell, col.name))))

    val Component = ScalaComponent.builder[Props]("Header")
      .render_P(render)
      .configure(shouldComponentUpdate)
      .build
  }
}