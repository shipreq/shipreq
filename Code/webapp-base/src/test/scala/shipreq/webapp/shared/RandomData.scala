package shipreq.webapp.shared

import shipreq.webapp.shared.prop.{Distinct, RngGen, Gen}
import shipreq.webapp.shared.data._
import shipreq.webapp.shared.data.delta._
import shipreq.webapp.shared.protocol._
import shipreq.base.util.Debug._

object RandomData {

  lazy val rev =
    Gen.positivelong.map(delta.Rev)

  lazy val revPair =
    for {
      r1 <- rev
      r2 <- rev
    } yield if (r1.value <= r2.value) (r1, r2) else (r2, r1)

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
      om <- reqTypeMnemonic.set.lim(16)
      ir <- implicationRequired
      a  <- alive
    } yield CustomReqType(id, mn, om - mn, n, ir, a)

  lazy val customReqTypes = {
    def distinctId = Distinct.on[CustomReqType](_.id.value).long((a, b) => a.copy(id = CustomReqType.Id(b)))
    def distinctName = Distinct.on[CustomReqType](_.name).str((a, b) => a.copy(name = b))
    def distinctMnemonics = Distinct.on[CustomReqType]
      .n(c => c.oldMnemonics + c.mnemonic)(
        (a, bs) => {
          var c = a
          c = a.copy(oldMnemonics = c.oldMnemonics -- bs)
          if (c.oldMnemonics.contains(c.mnemonic) || bs.contains(c.mnemonic))
            c = c.copy(mnemonic = ReqType.Mnemonic("\uffff" + bs.max.value))
          c
        })
      .blacklist(ReqType.static.map(_.mnemonic).toSet)
    for {
      r  <- rev
      cs <- customReqType.list.distinct(distinctId + distinctName + distinctMnemonics)
    } yield CustomReqTypes(r, cs)
  }

  lazy val project =
    customReqTypes.map(Project)

  // -------------------------------------------------------------------------------------------------------------------
  object remoteDeltaG {
    def forPart: Partition => RngGen[RemoteDeltaG] = {
      case p@ Partition.CustomReqTypes => customReqTypes
    }

    lazy val customReqTypes =
      for {
        (r1, r2) <- revPair
        ids      <- customReqTypeId.list
        cs       <- customReqType.list
      } yield RemoteDeltaG(Partition.CustomReqTypes, r1, r2)(ids, cs)
  }

  object remoteDelta {
    def forPart: Partition => RngGen[RemoteDelta] =
      remoteDeltaG.forPart(_).map(List(_))
  }

  // -------------------------------------------------------------------------------------------------------------------
  object routines {
    import Routine._, Routines._

    lazy val deletionAction = {
      import DeletionAction._
      Gen.oneof[DeletionAction](HardDel, SoftDel, Restore)
    }

    lazy val remoteName =
      Gen.alphanumericstring1

    def remote[D <: Desc](d: D) =
      remoteName.map(Remote(_, d))

    lazy val forCfgReqType =
      for {
        pi   <- remote(ProjectInit)
        crud <- remote(CustomReqTypeCrud)
      } yield ForCfgReqType(pi, crud)

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
