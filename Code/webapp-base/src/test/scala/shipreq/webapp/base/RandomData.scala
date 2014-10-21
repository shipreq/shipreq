package shipreq.webapp.base

import shipreq.webapp.base.prop.{Distinct, RngGen, Gen}
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.delta._
import shipreq.webapp.base.protocol._
import shipreq.base.util.Debug._
import DataImplicits._

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

  lazy val customIncmpTypes =
    dataSet[CustomIncmpTypeAndId](customIncmpType)

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
    dataSet[CustomReqTypeAndId](customReqType, Some(distinctName + distinctMnemonics))
  }

  def dataSet[T <: DataAndId](r: RngGen[T#Data], dist: Option[Distinct[T#Data]] = None)(implicit i: IdAccessor[T]): RngGen[DataSet[T]] = {
    def distId = Distinct.on[T#Data](_.id.value).long((a,b) => i.setId(a, i mkId b))
    val dist2 = dist.fold(distId: Distinct[T#Data])(distId + _)
    Gen.apply2(DataSet[T])(rev, r.list.distinct(dist2))
  }

  lazy val project =
    Gen.apply2(Project.apply)(customIncmpTypes, customReqTypes)

  // -------------------------------------------------------------------------------------------------------------------
  object remoteDeltaG {
    def forPart: Partition => RngGen[RemoteDeltaG] = {
      case Partition.CustomIncmpTypes => customIncmpTypesDG
      case Partition.CustomReqTypes   => customReqTypesDG
    }

    def generic[T <: Partition](p: T)(ir: RngGen[T#Id], dr: RngGen[T#Data]): RngGen[RemoteDeltaG] =
      for {
        (r1, r2) <- revPair
        i        <- ir.list
        d        <- dr.list
      } yield RemoteDeltaG(p, r1, r2)(i, d)
    //} yield RemoteDeltaG(Partition.CustomReqTypes, r1, r2)(ids -- cs.map(_.id), cs) // TODO make set

    lazy val customIncmpTypesDG =
      generic(Partition.CustomIncmpTypes)(customIncmpTypeId, customIncmpType)

    lazy val customReqTypesDG =
      generic(Partition.CustomReqTypes)(customReqTypeId, customReqType)
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
      Gen.apply4(ForCfgReqType)(
        remote(ProjectInit),
        remote(CustomIncmpTypeCrud),
        remote(CustomReqTypeCrud),
        remote(CustomReqTypeImpUpd))

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
