package shipreq.webapp.shared

import shipreq.webapp.shared.prop.{RngGen, Gen}
import shipreq.webapp.shared.data._
import shipreq.webapp.shared.data.delta._
import shipreq.webapp.shared.protocol._

object RandomData {

  lazy val alive =
    Gen.oneof[Alive](Alive, Dead)

  lazy val implicationRequired =
    Gen.oneof[ImplicationRequired](ImplicationRequired, ImplicationNotRequired)

  lazy val reqTypeMnemonic =
    Gen.uppers1.lim(6).map(cs => ReqType.Mnemonic(cs.list.mkString))

  lazy val customReqTypeId =
    Gen.long.map(CustomReqType.Id)

  lazy val customReqTypeName =
    Gen.alphanumericstring1 // TODO reenable after Jawn bugfix Gen.string1

  lazy val customReqType =
    for {
      id <- customReqTypeId
      n  <- customReqTypeName
      mn <- reqTypeMnemonic
      om <- reqTypeMnemonic.set
      ir <- implicationRequired
      a  <- alive
    } yield CustomReqType(id, mn, om - mn, n, ir, a)

  lazy val rev =
    Gen.positivelong.map(delta.Rev)

  object remoteDeltaG {
    def forPart: Partition => RngGen[RemoteDeltaG] = {
      case p@ Partition.CustomReqTypes => customReqTypes
    }

    lazy val customReqTypes =
      for {
        r1  <- rev
        r2  <- rev
        ids <- customReqTypeId.list
        cs  <- customReqType.list
      } yield RemoteDeltaG(Partition.CustomReqTypes, r1, r2)(ids, cs)
  }

  object remoteDelta {
    def forPart: Partition => RngGen[RemoteDelta] =
      remoteDeltaG.forPart(_).map(List(_))
  }

  lazy val deletionAction = {
    import DeletionAction._
    Gen.oneof[DeletionAction](HardDel, SoftDel, Restore)
  }

  object routines {
    import Routine._, Routines._

    lazy val remoteName =
      Gen.alphanumericstring1

    def remote[D <: Desc](d: D) =
      remoteName.map(Remote(_, d))

    lazy val forCfgReqType =
      remote(CustomReqTypeCrud).map(ForCfgReqType)

    class CrudActionGens[C <: Crudable] (idG: RngGen[C#Id], vG: RngGen[C#V]) {
      import Gen.Covariance._

      lazy val create =
        vG.map(CrudAction.Create[C])

      lazy val update =
        for {
          id <- idG
          vs <- vG
        } yield CrudAction.Update[C](id, vs)

      lazy val delete =
        for {
          id <- idG
          da <- deletionAction
        } yield CrudAction.Delete[C](id, da)

      lazy val any =
        Gen.oneofG[CrudAction[C]](create, update, delete)
    }

    lazy val customReqTypeCrud = new CrudActionGens[CustomReqTypeCrud](
      RandomData.customReqTypeId,
      for {
        m <- reqTypeMnemonic
        n <- customReqTypeName
        i <- implicationRequired
      } yield (m, n, i))
  }
}
