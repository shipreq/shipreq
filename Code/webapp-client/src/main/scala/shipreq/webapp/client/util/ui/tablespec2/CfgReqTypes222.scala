package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react._, vdom.ReactVDom.implicits._, prefix_<*._, ScalazReact._
import japgolly.scalajs.react.experiment.{Listenable, OnUnmount}

import shipreq.base.util.ScalaExt._
import shipreq.base.util.TaggedTypes.taggedStringInstance
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.protocol.Routine.Remote
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.util.ui.Util

import scalaz.effect.IO
import shipreq.base.util.GenTuple, GenTuple._

//import shipreq.webapp.base.data.Validators.{reqType => V}
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.delta.Partition
import shipreq.webapp.base.protocol.Routines.CustomReqTypeCrud
//import shipreq.webapp.client.lib._
//import shipreq.webapp.client.util.ui.Util.checkbox
//import shipreq.webapp.client.util.ui.table._
//import shipreq.webapp.client.util.ui.{Editors => E}

import scalaz.std.anyVal.booleanInstance
import scalaz.std.string.stringInstance
import scalaz.std.tuple._

object __Validators {
  import shipreq.webapp.base.AppConsts._
  import shipreq.webapp.base.TextMod._
  import shipreq.webapp.base.UiText.FieldNames
  import shipreq.webapp.base.validation2._
  import Constraints._
  import GenericValidators._

  object reqType {

    type S = (Stream[CustomReqType], Option[CustomReqType.Id])

    val mnemonicU = {
      val validChars = WhitelistCharsR("A-Z", "may only consist of letters.")
      val validLength = LengthInRange(reqTypeMnemonicLength)
      Validator(
        CorrectionPart.endo(noWhitespace andThen upperCase)
          .addLiveCorrect(upperCase.run andThen validChars.live.run andThen validLength.live.run),
        ValidationPart.forConstraint(FieldNames.mnemonic, nonEmpty >> (validChars.constraint + validLength))
          .map(ReqType.Mnemonic)
        )
    }

    private def mnemonicUniqueness = {
      val static = (none[CustomReqType.Id],  ReqType.staticMnemonics)
      Uniqueness.againstSetByKeyO[S, CustomReqType.Id, Mnemonic](
        sr => sr._2,
        sr => static #:: sr._1.map(_.tmap2(_.id.some, _.allMnemonics))
      ).fieldName(FieldNames.mnemonic)
    }

    val mnemonicS = mnemonicU.liftS[S].addValidation(mnemonicUniqueness)

    val nameU = mandatoryShortText("Name")

    private def nameUniqueness =
      Uniqueness.entity[CustomReqType].applyO(_.id.some, _.name).fieldName("Name")

    val nameS = nameU.liftS[S].addValidation(nameUniqueness)

    val all = mnemonicS ⊗ nameS ⊗ Validator.nop[ImplicationRequired].liftS[S]
  }

  object customIncmpType {
    def key = refKey
    def desc = optionalLargeText(FieldNames.desc)
  }

  val refKey = {
    // DD-18: Hashtag-like refkeys (groupings, incmp) must match this format: /[A-Za-z0-9][A-Za-z0-9_-=.]*/
    // Must not contain: []{}<>
    // TODO should uniqueness and matching be case-insensitive?
    val validChars = WhitelistCharsR("""A-Za-z0-9\._=\-""", "may only consist of letters, numbers, and these symbols: . _ = -")
    val validLength = LengthInRange(refKeyLength)
    Validator(
      CorrectionPart.endo(noWhitespace)
        .addLiveCorrect(validChars.run andThen truncateToLength(refKeyLength).run),
      ValidationPart.forConstraint(FieldNames.refKey, nonEmpty >> (startsWithAlphaNumeric + validChars + validLength))
        .map(RefKey.apply)
      )
  }
}
import __Validators.{reqType => V}

import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.validation2._
import scala.language.reflectiveCalls
import Editors.{EditorExtII, EditorExtV, EditorExt}
import monocle._
import monocle.syntax._

object CfgReqTypes222 {

//  val tableIO = new TableIO[CustomReqTypeAndId, CustomReqTypeCrud, CustomReqTypeCrud.type]
//  import tableIO.{D, P}

  val fields = FieldSet3[CustomReqType](_.mnemonic.value, _.name, _.imp)(("", "", ImplicationNotRequired))

  val savedRowStore = SavedRowStore.of(fields).keyedBy[CustomReqType.Id]
  val newRowStore   = NewRowStore.of(fields)
  case class State(newRow: newRowStore.State, savedRows: savedRowStore.State, showDeleted: Boolean)
  object State {
    private[this] def l = Lenser[State]
    val _newRow      = l(_.newRow)
    val _savedRows   = l(_.savedRows)
    val _showDeleted = l(_.showDeleted)
  }
  type S = State
  val ST = ReactS.FixT[IO, S]
  type ST = ReactST[IO, S, Unit]
  val savedRowStoreS = savedRowStore.contramap(State._savedRows)
  val newRowStoreS   = newRowStore  .contramap(State._newRow)

  case class Props(remote: CustomReqTypeCrud.Remote, clientData: ClientData, showDeleted: Boolean)

  def initialState(p: Props): State =
    State(
      newRowStore.initState,
      savedRowStore.initStateS(p.clientData.project.customReqTypes.data, _.id),
      p.showDeleted)

  class Backend(c: BackendScope[Props, State]) extends OnUnmount {

    val tableIO = TableIO2(CustomReqType, CustomReqTypeCrud)(c.props.remote, c.props.clientData)

    def stateForValidator(k: Option[CustomReqType.Id]): S => V.S =
      s => (savedRowStoreS.getAllP(s), k)

    val needSave = NeoSaves.SaveNeed.cmpToExtract((p: CustomReqType) => (p.mnemonic, p.name, p.imp))

    val mnemonicE = Editors.textInputEditor.applyValidator(V.mnemonicS)
    val nameE     = Editors.textInputEditor.applyValidator(V.nameS)
    val impE      = Editors.checkboxEditor.imap(ImplicationRequired).strengthL[V.S]

    val rowE = {
      var e = Editor.merge3S(fields, mnemonicE, nameE, impE).tupleI.zoomU[S]

      e = Editors.applyRowUpdateAndRevert(e, savedRowStoreS, newRowStoreS)(_._1._2)

      val savef = NeoSaves.validateAndSaveBoth(V.all, savedRowStoreS)(
        newRowStoreS,
        stateForValidator(None),
        k => stateForValidator(Some(k)),
        needSave,
        tableIO.createIO,
        tableIO.updateIO,
        c runState _)
      e.applyOnEditFinishedK(savef)(_._1._2)
    }

    val toggleShowDeleted: IO[Unit] = {
      val st = ST.modT(State._showDeleted.modifyF(v => !v))
      c runState st
    }

    def render: ReactElement =
      <.div(
        showDeletedElement(c.state.showDeleted, toggleShowDeleted),
        ???)
  }

  val xxxx = new RemoteDeltaListener2[CustomReqType, CustomReqType.Id, CustomReqTypeCrud.type]

  def showDeletedElement(show: Boolean, toggle: => IO[Unit]): ReactElement =
      <.label(
        Util.checkbox(show)(*.onchange ~~> toggle),
        if (show) "Deleted items visible." else "Deleted items hidden.")

  val Top =
    ReactComponentB[Props]("top")
      .getInitialState(initialState)
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(xxxx.recvExtUpdates(savedRowStoreS, Partition.CustomReqTypes, _.clientData))
      .build


//  private val specD = TableSpecD(spec)(_.alive, tableIO.deleteIO)
//
//  private val compI = tableIO.innerComponent(spec, Partition.CustomReqTypes, renderInner)
//
//  val comp = tableIO.outerComponent("Cfg: Requirement Types", compI)

  import japgolly.scalajs.react.vdom.ReactVDom.all._
  import shipreq.webapp.client.util.ui.Util.checkbox

//  private def cells = new CfgTableCells[P, (Modifier,Modifier,Modifier), (Modifier, Set[ReqType.Mnemonic], Modifier, Modifier)] {
//    override def mklist = {
//      case (mnemonic, oldMnemonics, name, impReq) =>
//        val mn: Modifier =
//          if (oldMnemonics.isEmpty)
//            mnemonic
//          else
//            Seq(mnemonic, div(cls := "oldMnemonics", oldMnemonics.toStream.map(_.value).sorted.mkString(", ")))
//        List(mn, name, impReq)
//    }
//    override def newRow = {
//      case (mnemonic, name, impReq) => (mnemonic, Set.empty, name, impReq)
//    }
//    override def savedRow = {
//      case ((mnemonic, name, impReq), p) => (mnemonic, p.oldMnemonics, name, impReq)
//    }
//    override def deletedRow = p =>
//      (raw(p.mnemonic), p.oldMnemonics, raw(p.name), checkbox(ImplicationRequired from p.imp)(disabled := true))
//  }

//  private val tbl = CfgTable[CustomReqTypeAndId].b1(spec)(specC, specD, ("", "", false), _.mnemonic).b2(cells)
//
//  private def renderInner(S: ComponentScopeU[tableIO.Props, prespec.S, _]): ReactElement =
//    tbl(S.props.showDeleted, S)(S.props.x)
//      .tableness(List("Mnemonic", "Name", "Implication Required"), staticRows #::: _)
//
//  private val staticRows: tbl.RowStream = {
//    def rr(r: ReqType.Static) = {
//      val imp = checkbox(ImplicationRequired from r.imp)(disabled := true)
//      tbl.row("static", RowStatus.Sync, (raw(r.mnemonic), r.oldMnemonics, raw(r.name), imp), EmptyTag)(keyAttr := r.mnemonic.value)
//    }
//    ReqType.static.map(r => r.mnemonic -> rr(r)).toStream
//  }
}