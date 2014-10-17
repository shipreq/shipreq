package shipreq.webapp.shared

import shipreq.webapp.shared.prop.{Distinct, RngGen, Gen}
import shipreq.webapp.shared.data._
import shipreq.webapp.shared.data.delta._
import shipreq.webapp.shared.protocol._
import shipreq.base.util.Debug._

object RandomData {

  lazy val id =
    Gen.positivelong

  lazy val rev =
    Gen.positivelong.map(Rev)

  lazy val revPair =
    for {
      r1 <- rev
      r2 <- rev
    } yield if (r1.value <= r2.value) (r1, r2) else (r2, r1)

  lazy val optionalLargeText =
    Gen.alphanumericstring1 // TODO reenable after Jawn bugfix: Gen.string1
      .lim(AppConsts.largeTextMaxLength).option

  lazy val alive =
    Gen.oneof[Alive](Alive, Dead)

  lazy val implicationRequired =
    Gen.oneof[ImplicationRequired](ImplicationRequired, ImplicationNotRequired)

  lazy val refKey =
    for {
      h <- Gen.alphanumeric
      t <- Gen.charof("._=-", 'a' to 'z', 'A' to 'Z', '0' to '9').list.lim(AppConsts.refKeyLength.end)
    } yield RefKey((h :: t).mkString)

  lazy val customIncmpTypeId =
    id map CustomIncmpType.Id

  lazy val customIncmpType =
    Gen.apply4(CustomIncmpType.apply)(customIncmpTypeId, refKey, optionalLargeText, alive)

  lazy val customIncmpTypes = {
    // TODO copied customIncmpTypes.distinctId copy of customReqTypes'
    def distinctId = Distinct.on[CustomIncmpType](_.id.value).long((a, b) => a.copy(id = CustomIncmpType.Id(b)))
    Gen.apply2(CustomIncmpTypes)(rev, customIncmpType.list.distinct(distinctId))
  }

  lazy val reqTypeMnemonic =
    Gen.uppers1.lim(6).map(cs => ReqType.Mnemonic(cs.list.mkString))

  lazy val customReqTypeId =
    id map CustomReqType.Id

  lazy val customReqTypeName =
    Gen.alphanumericstring1 // TODO reenable after Jawn bugfix: Gen.string1

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
    def distinctRules = distinctId + distinctName + distinctMnemonics
    Gen.apply2(CustomReqTypes)(rev, customReqType.list.distinct(distinctRules))
  }

  lazy val project =
    Gen.apply2(Project)(customIncmpTypes, customReqTypes)

  // -------------------------------------------------------------------------------------------------------------------
  object remoteDeltaG {
    def forPart: Partition => RngGen[RemoteDeltaG] = {
      case Partition.CustomIncmpTypes => customIncmpTypesDG
      case Partition.CustomReqTypes   => customReqTypesDG
    }

    // TODO another copy/paste/search/replace
    lazy val customIncmpTypesDG =
      for {
        (r1, r2) <- revPair
        ids      <- customIncmpTypeId.list
        cs       <- customIncmpType.list
      } yield RemoteDeltaG(Partition.CustomIncmpTypes, r1, r2)(ids, cs)

    lazy val customReqTypesDG =
      for {
        (r1, r2) <- revPair
        ids      <- customReqTypeId.list
        cs       <- customReqType.list
      //} yield RemoteDeltaG(Partition.CustomReqTypes, r1, r2)(ids -- cs.map(_.id), cs) // TODO make set
      } yield RemoteDeltaG(Partition.CustomReqTypes, r1, r2)(ids, cs)
  }

  object remoteDelta {
    def forPart: Partition => RngGen[RemoteDelta] =
      remoteDeltaG.forPart(_).map(List(_))
  }

  // -------------------------------------------------------------------------------------------------------------------
  object routines {
    import Routine._, Routines._

    lazy val deletionAction =
      Gen.oneofL(DeletionAction.values)

    lazy val remoteName =
      Gen.alphanumericstring1

    def remote[D <: Desc](d: D) =
      remoteName.map(Remote(_, d))

    lazy val forCfgReqType =
      Gen.apply3(ForCfgReqType)(remote(ProjectInit), remote(CustomIncmpTypeCrud), remote(CustomReqTypeCrud))

    class CrudActionGens[C <: Crudable] (idG: RngGen[C#Id], vG: RngGen[C#V]) {
      import Gen.Covariance._
      lazy val create = vG.map(CrudAction.Create[C])
      lazy val update = Gen.apply2(CrudAction.Update[C])(idG, vG)
      lazy val delete = Gen.apply2(CrudAction.Delete[C])(idG, deletionAction)
      lazy val any    = Gen.oneofG[CrudAction[C]](create, update, delete)
    }

    lazy val customIncmpTypeCrud = new CrudActionGens[CustomIncmpTypeCrud](
      RandomData.customIncmpTypeId,
      Gen.tuple2(refKey, optionalLargeText))

    lazy val customReqTypeCrud = new CrudActionGens[CustomReqTypeCrud](
      RandomData.customReqTypeId,
      Gen.tuple3(reqTypeMnemonic, customReqTypeName, implicationRequired))
  }
}
