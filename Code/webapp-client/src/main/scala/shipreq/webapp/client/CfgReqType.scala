package shipreq.webapp.client

import monocle.SimpleLens
import org.scalajs.dom.console
import scalaz.effect.IO
import scalaz.std.anyVal.booleanInstance
import scalaz.std.string.stringInstance
import scalaz.std.tuple._
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.experiment.{Listenable, OnUnmount}
import japgolly.scalajs.react.ScalazReact._

import shipreq.base.util.TaggedTypes.taggedStringInstance
import shipreq.webapp.shared.data._
import shipreq.webapp.shared.data.delta.Partition
import shipreq.webapp.shared.protocol.Routines
import shipreq.webapp.client.delta.LocalDeltas
import shipreq.webapp.client.protocol.{FailureIO, ClientProtocol}
import shipreq.webapp.client.ui.table._
import shipreq.webapp.client.ui.{Editors => E, Util}
import Validators.{reqType => V}
import ReqType.Mnemonic

object CfgReqType {

  type P = CustReqType
  type D = CustReqType.Id
  type X = (Routines.ForCfgReqType, ClientData)

  private val prespec = TableSpecBuilder[P](
    FieldSpec[P](_.mnemonic.value)(V.mnemonic)(E.TextInputEditor),
    FieldSpec[P](_.name)(V.name)(E.TextInputEditor),
    FieldSpec[P].noValidation(_.imp, ImplicationRequired)(E.CheckboxEditor))
    .dataId[D]

  private val spec = prespec
    .tableConstraints(
      Some(mnemonicUniqueness),
      Some(prespec.uniquenessCheck(_.name).fieldName("Name")),
      None)
    .saveNotNeededWhenE(p => (p.mnemonic, p.name, p.imp))
    .asyncSaveP(_.id, saveIO)

  private def mnemonicUniqueness =
    TableConstraint.uniquenessE[prespec.S, prespec.R, Mnemonic](
      (s, r) => {
        val custom: Stream[ReqType] =
          s._1.toStream
            .filterNot(dpi => r.fold(false)(_ == dpi._1)) // exclude own row
            .map(_._2.p)
        val static: Stream[ReqType] = ReqType.static.toStream
        (static #::: custom).flatMap(p => p.mnemonic #:: p.oldMnemonics.toStream)
      }).fieldName("Mnemonic")

  case class Props(x: X, showDeleted: Boolean)

  final class MyBack extends OnUnmount

  private def recvExtUpdate(d: LocalDeltas) = ReactS.mod[prespec.S](s1 => {
    val ds = LocalDeltas.filter(Partition.CustReqType, d)
    val s2 = (s1 /: ds.del)((t,id) => spec.savedRemoveF(id)(t))
    val s3 = (s2 /: ds.upd)((t,p) => spec.savedSetF(p.id, p)(t))
    s3
  })

  val Component = ReactComponentB[Props]("CfgReqTypes")
    .getInitialState(p => p.showDeleted)
    .render(Render.renderOuter _)
    .build

  private val InnerComponent = ReactComponentB[Props]("CfgReqTypesⁱ")
    .getInitialState(p => spec.initialState(p.x._2.project.customReqTypes.data, _.id))
    .backend(_ => new MyBack)
    .render(Render.renderInner _)
    .configure(Listenable.installS(_.x._2, recvExtUpdate))
    .build

  private def saveIO(x: X, op: Option[P], u: prespec.U, f: FailureIO): IO[Unit] =
    op match {
      case None =>
        ClientProtocol.call(x._1.create)(u, o => IO(console.log(s"Ajax Result = $o")), f)
      case Some(p) =>
        ClientProtocol.call(x._1.update)((p.id, u), o => IO(console.log(s"Ajax Result = $o")), f)
    }

  private val deletion =
    new AsyncDeletion(spec)(
      SimpleLens[P](_.alive)((a,b) => a.copy(alive = b)),
      deleteIO)

  private def deleteIO(x: X, id: D, a: DeletionAction, f: FailureIO): IO[Unit] =
    // TODO Merge these callbacks
    a match {
      case HardDelete => ClientProtocol.call(x._1.hardDelete)(id, o => IO(console.log(s"Ajax Result = $o")), f)
      case SoftDelete => ClientProtocol.call(x._1.softDelete)(id, o => IO{console.log(s"Ajax Result = $o"); x._2.update(o) }, f)
      case Restore    => ClientProtocol.call(x._1.restore   )(id, o => IO(console.log(s"Ajax Result = $o")), f)
    }

  private val newRowS = spec.unsavedInitS(("","",false))

  private object Render {
    import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
    import Util.checkbox

    def renderOuter(S: ComponentScopeU[Props, Boolean, Unit]): VDom = {
      val s = S.state
      div(
        label(
          checkbox(s)(onchange --> S.modState(b => !b)),
          raw(if (s) "Showing deleted" else "Not showing deleted")),
        InnerComponent(S.props.copy(showDeleted = s)))
    }

    type ScopeI = ComponentScopeU[Props, prespec.S, MyBack]
    type FocusI = ComponentStateFocus[prespec.S]
    type RowStream = Stream[(Mnemonic, Tag)]

    def renderInner(S: ScopeI): VDom = {
      implicit val x: X = S.props.x
      val newRow = Render.newRow.render(S)(())
      val nonNewRows = (staticRows #::: savedRows(S) #::: deletedRows(S)).sortBy(_._1.value).map(_._2).toJsArray
      div(
        button(onclick ~~> S.runState(newRowS), "New"),
        table(
          thead(tr(th("Mnemonic"), th("Name"), th("Implication Required"), th("Ctrls"))),
          tbody(newRow, nonNewRows)))
    }

    private def row(mnemonic: Modifier, name: Modifier, impReq: Modifier, delButton: Modifier) =
      Seq(td(mnemonic), td(name), td(impReq), td(delButton))

    def newRow(implicit x: X) =
      spec.unsavedRow((F, vv) => {
        val (mnemonic, name, impReq) = vv
        val delButton = button(onclick ~~> F.runState(spec.unsavedRemoveS))("Cancel")
        tr(keyAttr := "new", row(mnemonic, name, impReq, delButton))
      })

    def savedRows(S: ScopeI)(implicit x: X): RowStream = {
      val rr = savedRow.render(S)
      deletion.getSavedP(S, Alive).map(p => p.mnemonic -> rr(p.id))
    }

    def savedRow(implicit x: X) =
      spec.savedRowP((F, id, p, vv) => {
        val (mnemonic, name, impReq) = vv
        tr(keyAttr := id.value, row(mnemonic, name, impReq, deletion.buttons(F, id, HardDelete, SoftDelete)))
      })

    def deletedRows(S: ScopeI)(implicit x: X): RowStream =
      if (S.props.showDeleted)
        deletion.getSavedP(S, Dead).map(p => p.mnemonic -> deletedRow(S, p))
      else
        Stream.empty

    def deletedRow(F: FocusI, p: P)(implicit x: X) = {
      val imp = checkbox(ImplicationRequired from p.imp)(disabled := true)
      val del = deletion.button(F, p.id, Restore)
      tr(cls := "del", key := p.id.value, row(raw(p.mnemonic), raw(p.name), imp, del))
    }

    val staticRows: RowStream =
      ReqType.static.map(r => r.mnemonic -> staticRow(r)).toStream

    def staticRow(r: ReqType.Static) = {
      val imp = checkbox(ImplicationRequired from r.imp)(disabled := true)
      tr(key := r.mnemonic.value, row(raw(r.mnemonic), raw(r.name), imp, EmptyTag))
    }
  }
}