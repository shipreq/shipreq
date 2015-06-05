package shipreq.webapp.base.data

import japgolly.nyaya.CycleDetector
import japgolly.nyaya.util.Multimap
import monocle.macros.Lenses
import monocle.Prism
import scalaz.Order
import scalaz.std.string.stringInstance
import scalaz.syntax.equal._
import shapeless.{Generic, :+:, CNil, Coproduct, Inl, Inr}
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.TaggedTypes._
import shipreq.webapp.base.text.Text, Text.Equality._
import shipreq.webapp.base.TransitiveClosure
import shipreq.webapp.base.TypeclassDerivation._

// ===================================================================================================================
// ReqCodes: A hierarchy of semantic IDs

final case class ReqCodeId(value: Long) extends TaggedLong

/**
 * [[ReqCode.Trie]] contains the hierarchy of codes and their targets.
 * [[ReqCodes]] is a bundle of all req-codes in a project.
 */
object ReqCode {

  /**
   * A textual ID that refers to a requirement.
   *
   * Eg. "system.email.failure" would be `NonEmptyVector(Node("system"), Node("email"), Node("failure"))`.
   */
  type Value = NonEmptyVector[Node]

  /**
   * Portion of a [[ReqCode]], separated by ".".
   *
   * Eg. "mail" in "system.mail.failure"
   */
  final class Node private (val value: String) {
    override def equals(o: Any) = o match {case b:Node => this eq b; case _ => false }
    override def hashCode = value.##
    override def toString = value
  }

  trait NodeUnivEq {
    implicit def nodeUnivEq: UnivEq[Node] = UnivEq.force
  }

  object Node extends NodeUnivEq {
    implicit val order: Order[Node] = {
      import scalaz.Ordering
      val S = Order[String]
      new Order[Node] {
        override def equal(a: Node, b: Node): Boolean =
          a eq b

        override def order(a: Node, b: Node): Ordering =
          if (a eq b)
            Ordering.EQ
          else
            S.order(a.value, b.value)
      }
    }

    val applyFn: String => Node =
      Platform.memo[String, Node](new Node(_))

    @inline def apply(value: String): Node =
      applyFn(value)
  }

  /**
   * Something to which a [[ReqCode]] can refer.
   *
   * type [[Target]] = [[ReqCodeGroup]] | [[ReqId]]
   */
  sealed trait Target

  object Target {
    implicit object GenericInstance extends Generic[Target] {
      override type Repr = ReqId :+: ReqCodeGroup :+: CNil
      override def to  (t: Target): Repr = t match {
        case a: ReqId        => Coproduct[Repr](a)
        case a: ReqCodeGroup => Coproduct[Repr](a)
      }
      override def from(co: Repr): Target = co match {
        case Inl(a)      => a
        case Inr(Inl(a)) => a
        case _ => ???
      }
    }

    implicit def equality: UnivEq[Target] = deriveUnivEq

    lazy val reqId        = Prism[Target, ReqId]       ({case a: ReqId        => Some(a); case _ => None})(t => t)
    lazy val reqCodeGroup = Prism[Target, ReqCodeGroup]({case a: ReqCodeGroup => Some(a); case _ => None})(t => t)
  }

  /**
   * Data associated with a ReqCode in the case that the ReqCode exists in the current user-visible tree of ReqCodes.
   * (As opposed to a ReqCode that exists for technical reasons and doesn't exist as far as the user is concerned.)
   */
  @Lenses
  final case class ActiveData(id: ReqCodeId, target: Target)

  /**
   * Data associated with each [[ReqCode.Value]].
   *
   * See `Design/req_codes.ods`.
   *
   * @param refsToGroup Previous IDs still referenced in rich text.
   * @param refsToReqs Previous req-associations still referenced in rich text.
   */
  @Lenses
  final case class Data(active     : Option[ActiveData],
                        refsToGroup: Set[ReqCodeId],
                        refsToReqs : Multimap[ReqId, Set, ReqCodeId]) {

    def ids: Stream[ReqCodeId] =
      active.toStream.map(_.id) append
        refsToGroup.toStream append
        refsToReqs.allValues

    def reqIds: Stream[ReqId] =
      refsToReqs.keys.toStream append active.map(_.target).toList.filterT[ReqId]
  }

  implicit val activeDataEquality: UnivEq[ActiveData] = deriveUnivEq
  implicit val dataEquality      : UnivEq[Data]       = deriveUnivEq

  type Trie = MTrie.Trie[Node, Data]
  def emptyTrie: Trie = MTrie.empty[Node, Data]
}

/**
 * A row that exists just to provide a description or summary of its children in the ReqCode hierarchy.
 *
 * Previously called "Semantic Header Row" or "SHR" in the requirements.
 */
final case class ReqCodeGroup(title: Text.ReqCodeGroupTitle.OptionalText) extends ReqCode.Target
object ReqCodeGroup {
  implicit val equality: UnivEq[ReqCodeGroup] = deriveUnivEq
}

/**
 * All req code data for in a project.
 */
final case class ReqCodes(trie: ReqCode.Trie) {
  import ReqCode._
  import MTrie.Ops

  def cataA[A](z: A)(f: (A, Value, ActiveData) => A): A =
    trie.cataV(z)((a, v, d) => d.active.fold(a)(f(a, v, _)))

  lazy val activeReqCodesByTarget: Multimap[Target, Set, Value] =
    cataA(UnivEq.emptySetMultimap[Target, Value])((q, c, d) =>
      q.add(d.target, c))

  lazy val reqCodesById: Map[ReqCodeId, Value] =
    trie.cataV(UnivEq.emptyMap[ReqCodeId, Value])((q, c, d) =>
      d.ids.foldLeft(q)(_.updated(_, c)))

  def reqCode(id: ReqCodeId): Must[Value] =
    Must.fromOption(reqCodesById get id, s"No req code associated with $id.")

  def apply(code: Value): Option[Data] =
    trie.lookup(code)

  def applyM(code: Value): Must[Data] =
    Must.fromOption(apply(code), s"No node at reqcode ${code.whole mkString "."}.")

  def allIds: Stream[ReqCodeId] =
    trie.flatStream.flatMap(_._2.ids)

//  lazy val targetToIds: Multimap[Target, Set, Id] =
//    trie.cataV(UnivEq.emptySetMultimap[Target, Id])((q, _, d) =>
//      q.add(d.target, d.id))
}

// ===================================================================================================================
// Public IDs (like MF-3)

/**
 * A position (ordinal) in a req-type's ordered list of requirements.
 *
 * Eg. the "3" in "FR-3".
 *
 * @param value ≥ 1.
 */
final case class ReqTypePos(value: Int) extends TaggedInt

/**
 * Public ID: A requirement's ID from the public's point-of-view.
 *
 * Eg. "FR-3"
 */
final case class PubidT[+T <: ReqTypeId](reqTypeId: T, pos: ReqTypePos)

object PubidT {
  UnivEq[ReqTypePos]
  implicit def equality[T <: ReqTypeId : UnivEq]: UnivEq[PubidT[T]] = UnivEq.force
}

/**
 * Once a (reqtype x position) is allocated, it is never removed.
 * Thus, the 0-based position in the vector corresponds with 1-based [[ReqTypePos]] values.
 */
case class PubidRegister(value: Multimap[ReqTypeId, Vector, ReqId]) {

  def allocC(reqTypeId: CustomReqTypeId)(reqId: ReqIdC): (PubidRegister, PubidC) =
    _alloc(reqTypeId)(reqId)

  private def _alloc[T <: ReqTypeId](reqTypeId: T)(reqId: ReqIdT[T]): (PubidRegister, PubidT[T]) = {
    val cur = value(reqTypeId)
    val i = cur.indexWhere(_ ≟ reqId)
    if (i >= 0)
      (this, PubidT(reqTypeId, ReqTypePos(i + 1)))
    else
      (PubidRegister(value.add(reqTypeId, reqId)), PubidT(reqTypeId, ReqTypePos(cur.size + 1)))
  }

   def apply[T <: ReqTypeId](id: PubidT[T]): Option[ReqIdT[T]] = {
    val v = value(id.reqTypeId)
    val i = id.pos.value - 1
    @inline def cast(r: ReqId) = r.asInstanceOf[ReqIdT[T]]
    try {
      Some(cast(v(i)))
    } catch {
      case _: IndexOutOfBoundsException => None
    }
  }
}

object PubidRegister {
  implicit def equality: UnivEq[PubidRegister] = deriveUnivEq
  def empty = PubidRegister(UnivEq.emptyMultimap)
}

// ===================================================================================================================
// Requirements

/** type [[ReqIdT]] = [[GenericReqId]] */
sealed trait ReqIdT[+RT <: ReqTypeId] extends TaggedLong with ReqCode.Target

/** [[Req]] = [[GenericReq]] */
sealed abstract class ReqT[+RT <: ReqTypeId] {
  val id: ReqIdT[RT]
  val pubid: PubidT[RT]
  val alive: Alive

  @inline final def reqTypeId: RT = pubid.reqTypeId
}

object ReqT {
  object IdAccess extends ObjDataId[ReqT.type, Req, ReqId] {
    override def id(d: Req) = d.id
    override val unapplyData: AnyRef => Option[Req] = {case r: Req => Some(r); case _ => None}
  }

  val idProof: RelationProof[ReqTypeId, ReqT, ReqIdT] =
    new RelationProof[ReqTypeId, ReqT, ReqIdT] {
      override def apply[A <: ReqTypeId](v: ReqT[A]): ReqIdT[A] = v.id
    }
}

final case class GenericReqId(value: Long) extends TaggedLong with ReqIdT[CustomReqTypeId]

final case class GenericReq(id   : GenericReqId,
                            pubid: PubidC,
                            title: Text.GenericReqTitle.OptionalText,
                            alive: Alive) extends ReqT[CustomReqTypeId]

object GenericReq {
  implicit val equality: UnivEq[GenericReq] = deriveUnivEq
}

object ReqFieldData {
  /** U = Unidirectional */
  type ImplicationsU = Multimap[ReqId, Set, ReqId]
  type Tags          = Multimap[ReqId, Set, ApplicableTagId]
  type Text          = Map[CustomField.Text.Id, Map[ReqId, Text.CustomTextField.NonEmptyText]]

  def implicationCycleDetector =
    CycleDetector.Directed.multimap[Set, ReqId, Long](_.value, UnivEq.emptySet)

  def implicationTransitiveClosure(keys: Iterable[ReqId], dead: Set[ReqId], is: ImplicationsU): TransitiveClosure[ReqId] =
    TransitiveClosure.auto[ReqId](keys)(is.apply, !dead.contains(_))

  case class Implications(srcToTgt: ImplicationsU) {
    lazy val tgtToSrc: ImplicationsU = srcToTgt.reverse

    def members: Set[ReqId] =
      srcToTgt.m.toStream.foldLeft(UnivEq.emptySet[ReqId]) {
        case (q, (k, vs)) => q + k ++ vs
      }
  }
}

case class ReqFieldData(text        : ReqFieldData.Text,
                        tags        : ReqFieldData.Tags,
                        implications: ReqFieldData.Implications)

object Requirements {
  type Data = IMapK[ReqTypeId, ReqIdT, ReqT]
  def emptyData = ReqT.idProof.emptyIMapK
  def empty = Requirements(emptyData, PubidRegister.empty)
}

@Lenses
case class Requirements(reqs: Requirements.Data, pubids: PubidRegister) {

  lazy val dead: Set[ReqId] =
    reqs.filterV(_.alive :: Dead).keySet

  def req[T <: ReqTypeId](id: ReqIdT[T]): Option[ReqT[T]] =
    reqs.get(id)

  def reqByPubid[T <: ReqTypeId](id: PubidT[T]): Option[ReqT[T]] =
    pubids(id) flatMap req

  def reqM[T <: ReqTypeId](id: ReqIdT[T]): Must[ReqT[T]] =
    Must.fromOption(req(id), s"Req $id not found.")

  def reqsM[M[X] <: TraversableOnce[X]: Monoidish, T <: ReqTypeId](ids: M[ReqIdT[T]]): Must[M[ReqT[T]]] =
    Must.foldMapM(ids)(reqM)

  def reqByPubidM[T <: ReqTypeId](id: PubidT[T]): Must[ReqT[T]] =
    Must.fromOption(reqByPubid(id), s"Req for $id not found.")

  def reqIdByPubidM[T <: ReqTypeId](id: PubidT[T]): Must[ReqIdT[T]] =
    Must.fromOption(pubids(id), s"Req for $id not found.")

  def reqsByPubidM[M[X] <: TraversableOnce[X]: Monoidish, T <: ReqTypeId](ids: M[PubidT[T]]): Must[M[ReqT[T]]] =
    Must.foldMapM(ids)(reqByPubidM)

  lazy val reqsByType: Multimap[ReqTypeId, Vector, Req] =
    UnivEq.emptyMultimap[ReqTypeId, Vector, Req]
      .addPairs(reqs.vstream(_.mapStrengthL(_.reqTypeId)): _*)
}
