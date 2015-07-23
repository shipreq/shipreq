package shipreq.webapp.base.protocol

import boopickle._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.DeletionAction
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecData._
import AtomPicklers.instances._

object BinCodecProtocolData {

  implicit final val pickleGenericFailure = pickleCaseClass[GenericFailure]

  implicit final val pickleDeletionAction = pickleEnum(DeletionAction.values)

  def pickleCrudAction[I, V](implicit PI: Pickler[I], PV: Pickler[V]): Pickler[CrudAction[I, V]] = {
    import CrudAction._
    implicit val create = pickleCaseClass[Create[I, V]]
    implicit val update = pickleCaseClass[Update[I, V]]
    implicit val delete = pickleCaseClass[Delete[I, V]]
//    pickleADT // TODO SI-7046
    unsafeSelector[CrudAction[I, V]](create, update, delete) {
      case _: Create[I, V] => 0
      case _: Update[I, V] => 1
      case _: Delete[I, V] => 2
    }
  }

  // ------------------------------------------------------------------------------------
  // Field

  import shipreq.webapp.base.protocol.{FieldProtocol => FP}
  implicit final val pickleFieldProtocolValues = {
    import FP._, Field.ApplicableReqTypes
    implicit val pText        = pickleCaseClass[TextFieldValues]
    implicit val pTag         = pickleCaseClass[TagFieldValues]
    implicit val pImplication = pickleCaseClass[ImplicationFieldValues]
//    pickleADT[Values] // TODO SI-7046
    unsafeSelector[Values](pText, pTag, pImplication) {
      case _: TextFieldValues        => 0
      case _: TagFieldValues         => 1
      case _: ImplicationFieldValues => 2
    }
  }
  implicit final val pickleFieldProtocolCfgAction = {
    import FP._, CfgAction._
    implicit val pCreate       = pickleCaseClass[Create]
    implicit val pUpdateValues = pickleCaseClass[UpdateValues]
    implicit val pUpdateOrder  = pickleCaseClass[UpdateOrder]
    implicit val pDelete       = pickleCaseClass[Delete]
//    pickleADT[CfgAction] // TODO SI-7046
    unsafeSelector[CfgAction](pCreate, pUpdateValues, pUpdateOrder, pDelete) {
      case _: Create       => 0
      case _: UpdateValues => 1
      case _: UpdateOrder  => 2
      case _: Delete       => 3
    }
  }

  // ------------------------------------------------------------------------------------
  // Tag

  import shipreq.webapp.base.protocol.{TagProtocol => TP}
  implicit final val pickleTagPovRelations     = pickleCaseClass[MMTree.Relations[TagId]]
  implicit final val pickleTagGroupValues      = pickleCaseClass[TP.TagGroupValues]
  implicit final val pickleApplicableTagValues = pickleCaseClass[TP.ApplicableTagValues]
  //implicit final val pickleTagValues           = _pickleADT[TP.Values] // TODO SI-7046
  implicit final val pickleTagValues = unsafeSelector[TP.Values](pickleTagGroupValues, pickleApplicableTagValues) {
    case _: TP.TagGroupValues      => 0
    case _: TP.ApplicableTagValues => 1
  }

  // ------------------------------------------------------------------------------------
  // ContentUpdate

  import shipreq.webapp.base.protocol.{ContentUpdate => CU}
  implicit final val pickleCUPatchReqTags         = pickleCaseClass[CU.PatchReqTags]
  implicit final val pickleCUPatchImplicationSrc  = pickleCaseClass[CU.PatchImplicationSrc]
  implicit final val pickleCUPatchImplicationTgt  = pickleCaseClass[CU.PatchImplicationTgt]
  implicit final val pickleCUPatchReqCodes        = pickleCaseClass[CU.PatchReqCodes]
  implicit final val pickleCUSetGenericReqType    = pickleCaseClass[CU.SetGenericReqType]
  implicit final val pickleCUSetReqCodeGroupCode  = pickleCaseClass[CU.SetReqCodeGroupCode]
  implicit final val pickleCUSetReqCodeGroupTitle = pickleCaseClass[CU.SetReqCodeGroupTitle]
  implicit final val pickleCUSetGenericReqTitle   = pickleCaseClass[CU.SetGenericReqTitle]
  implicit final val pickleCUSetCustomTextField   = pickleCaseClass[CU.SetCustomTextField]
//  implicit final val pickleContentUpdate          = pickleADT[CU] // TODO SI-7046
  implicit final val pickleContentUpdate = unsafeSelector[CU](
    pickleCUPatchReqTags         ,
    pickleCUPatchImplicationSrc  ,
    pickleCUPatchImplicationTgt  ,
    pickleCUPatchReqCodes        ,
    pickleCUSetGenericReqType    ,
    pickleCUSetReqCodeGroupCode  ,
    pickleCUSetReqCodeGroupTitle ,
    pickleCUSetGenericReqTitle   ,
    pickleCUSetCustomTextField   ){
      case _: CU.PatchReqTags         => 0
      case _: CU.PatchImplicationSrc  => 1
      case _: CU.PatchImplicationTgt  => 2
      case _: CU.PatchReqCodes        => 3
      case _: CU.SetGenericReqType    => 4
      case _: CU.SetReqCodeGroupCode  => 5
      case _: CU.SetReqCodeGroupTitle => 6
      case _: CU.SetGenericReqTitle   => 7
      case _: CU.SetCustomTextField   => 8
    }

}