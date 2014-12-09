package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react._, vdom.prefix_<*._, ScalazReact._
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
import shipreq.webapp.client.util.ui.Util.checkbox


object CfgReqTypes222 {

  val fields = FieldSet3[CustomReqType](_.mnemonic.value, _.name, _.imp)(("", "", ImplicationNotRequired))

  val storesAndState = TypicalStoresAndState(fields).keyedBy[CustomReqType.Id]
  import storesAndState._

  case class Props(remote: CustomReqTypeCrud.Remote, clientData: ClientData, showDeleted: Boolean) {
    def component = Component(this)
  }

  val Component =
    ReactComponentB[Props]("Cfg: Req Types")
      .getInitialState(initialState)
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(
        RemoteDeltaListener(CustomReqType, CustomReqTypeCrud)
          .recvExtUpdates(savedRowStoreS, Partition.CustomReqTypes, _.clientData))
      .build

  private def initialState(p: Props): S =
    State(newRowStore.initState,
      savedRowStore.initStateS(p.clientData.project.customReqTypes.data, _.id),
      p.showDeleted)

  // ===================================================================================================================
  final class Backend(c: BackendScope[Props, S]) extends OnUnmount {

    val tableIO = TableIO(CustomReqType, CustomReqTypeCrud)(c.props.remote, c.props.clientData)

    val deletion = Persistence.deleterAsync(savedRowStoreS)(_.alive, tableIO._deleteIO, c runState _)

    val rowE = {
      val mnemonicE = Editors.textInputEditor.applyValidator(V.mnemonicS)
      val nameE     = Editors.textInputEditor.applyValidator(V.nameS)
      val impE      = Editors.checkboxEditor.imap(ImplicationRequired).strengthL[V.S]

      var e = Editor.merge3S(fields, mnemonicE, nameE, impE).tupleI.zoomU[S]

      e = Editors.applyRowUpdateAndRevert(e, savedRowStoreS, newRowStoreS)(_._1._2)

      val needSave = Persistence.SaveNeed.cmpToExtract((p: CustomReqType) => (p.mnemonic, p.name, p.imp))
      val savef = Persistence.typicalValidateAndSave(V.all, storesAndState)(needSave, tableIO, c runState _)
      e.applyOnEditFinishedK(savef)(_._1._2)
    }

    val table = {
      def rowRenderer =
        new CfgTable.RowRenderer[CustomReqType, rowE.View, (Modifier, Set[ReqType.Mnemonic], Modifier, Modifier)] {

          override def newRow = {
            case (mnemonic, name, impReq) => (mnemonic, Set.empty, name, impReq)
          }

          override def savedRow = {
            case ((mnemonic, name, impReq), p) => (mnemonic, p.oldMnemonics, name, impReq)
          }

          override def deletedRow = p =>
            (p.mnemonic.value, p.oldMnemonics, p.name, checkbox(ImplicationRequired from p.imp)(*.disabled := true))

          override def render = {
            case (mnemonic, oldMnemonics, name, impReq) =>
              val mn: Modifier =
                if (oldMnemonics.isEmpty)
                  mnemonic
                else
                  Seq(mnemonic, <.div(*.cls := "oldMnemonics", oldMnemonics.toStream.map(_.value).sorted.mkString(", ")))
              Seq(mn, name, impReq)
          }
        }

      val t = CfgTable.typical(storesAndState)(rowE)(_.mnemonic, rowRenderer, deletion, c)

      val headerRow = CfgTable.header(List("Mnemonic", "Name", "Implication Required"))

      val staticRows: t.RowStream = {
        def rr(r: ReqType.Static): ReactElement = {
          val imp = checkbox(ImplicationRequired from r.imp)(*.disabled := true)
          val norm: t.RowContent = (r.mnemonic.value, r.oldMnemonics, r.name, imp)
          t.row("static", RowStatus.Sync, norm, EmptyTag)(*.keyAttr := r.mnemonic.value)
        }
        ReqType.static.map(r => r.mnemonic -> rr(r)).toStream
      }

      () => t.table(headerRow, staticRows)
    }

    def render: ReactElement =
      <.div(
        ShowDeletedToggler(storesAndState)(c),
        table())
  }
}