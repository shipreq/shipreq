package shipreq.webapp.base

import scalaz.std.list._
import scalaz.std.set._
import shipreq.prop.test.{Distinct, RngGen, Gen}
import shipreq.webapp.base.data._, ReqType.Mnemonic
import shipreq.webapp.base.data.delta._
import shipreq.webapp.base.protocol._
import shipreq.base.util.Debug._
import DataImplicits._

// TODO RandomData is inaccurate in that CorrectionParts aren't applied.

object RandomData {

  lazy val id =
    Gen.positivelong

  def shortText1 =
    Gen.alphanumericstring1.lim(AppConsts.shortTextMaxLength) // TODO reenable after Jawn bugfix: Gen.string1

  def shortText =
    Gen.alphanumericstring.lim(AppConsts.shortTextMaxLength) // TODO reenable after Jawn bugfix: Gen.string

  lazy val optionalLargeText =
    shortText1.lim(AppConsts.largeTextMaxLength).option

  lazy val rev =
    Gen.positivelong.map(Rev)

  lazy val revPair =
    for {
      r1 <- rev
      r2 <- rev
    } yield if (r1.value <= r2.value) (r1, r2) else (r2, r1)

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

  /** RefKey uniqueness enforced in Project, not here */
  lazy val customIncmpTypes =
    dataSet[CustomIncmpTypeAndId](customIncmpType, identity)

  lazy val reqTypeMnemonic =
    Gen.uppers1.lim(6).map(cs => Mnemonic(cs.list.mkString))

  lazy val customReqTypeId =
    id map CustomReqType.Id

  def customReqTypeName =
    shortText1

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
    def dname = Distinct.str.at(CustomReqTypeL.name)
    def dmnemonic = {
      val distm = Distinct.fstr.xmap(Mnemonic.apply)(_.value).addhs(ReqType.staticMnemonics).distinct
      val cur = distm.at(CustomReqTypeL.mnemonic)
      val old = distm.lift[Set].at(CustomReqTypeL.oldMnemonics)
      cur + old
    }
    val d = (dname * dmnemonic).lift[List]
    dataSet[CustomReqTypeAndId](customReqType, d.run)
  }

  def distinctId[T <: DataAndId](implicit i: IdAccessor[T]) =
    Distinct.flong.xmap(i.mkId)(_.value).distinct.contramap[T#Data](i.id, i.setId)

  def dataSet[T <: DataAndId](r: RngGen[T#Data], mod: List[T#Data] => List[T#Data])(implicit i: IdAccessor[T]): RngGen[DataSet[T]] = {
    val d = distinctId[T].lift[List]
    val f = mod compose d.run
    Gen.apply2(DataSet[T])(rev, r.list.map(f))
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
        remote(CustomReqTypeImplicationMod))

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
