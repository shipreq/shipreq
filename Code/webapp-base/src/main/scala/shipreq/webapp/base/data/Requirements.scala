package shipreq.webapp.base.data

import japgolly.nyaya.CycleDetector
import japgolly.nyaya.util.Multimap
import monocle.macros.Lenses
import scalaz.Order
import scalaz.std.string.stringInstance
import scalaz.syntax.equal._
import shapeless.{Generic, :+:, CNil, Coproduct, Inl, Inr}
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.TaggedTypes._
import shipreq.webapp.base.text.Text, Text.Equality._
import shipreq.webapp.base.TypeclassDerivation._

// ===================================================================================================================
// ReqCodes: A hierarchy of semantic IDs

// TODO all req code text should be lowercase

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
   * type [[Target]] = [[ReqCodeGroup]] | [[Req.Id]]
   */
  sealed trait Target

  implicit object TargetGeneric extends Generic[Target] {
    override type Repr = Req.Id :+: ReqCodeGroup :+: CNil
    override def to  (t: Target): Repr = t match {
      case a: Req.Id       => Coproduct[Repr](a)
      case a: ReqCodeGroup => Coproduct[Repr](a)
    }
    override def from(co: Repr): Target = co match {
      case Inl(a)      => a
      case Inr(Inl(a)) => a
      case _ => ???
    }
  }

  implicit val targetEquality: UnivEq[Target] = deriveUnivEq

  // TODO Why the hell aren't IDs AnyVals?
  final case class Id(value: Long) extends TaggedLong

  /**
   * Data associated with a ReqCode in the case that the ReqCode exists in the current user-visible tree of ReqCodes.
   * (As opposed to a ReqCode that exists for technical reasons and doesn't exist as far as the user is concerned.)
   */
  @Lenses
  final case class ActiveData(id: Id, target: Target)

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
                        refsToGroup: Set[Id],
                        refsToReqs : Multimap[Req.Id, Set, Id]) {

    def ids: Stream[Id] =
      active.toStream.map(_.id) append
        refsToGroup.toStream append
        refsToReqs.allValues
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
final case class ReqCodeGroup(title: Text.RecCodeGroupTitle.OptionalText) extends ReqCode.Target
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
    cataA(UnivEq.emptyMultimap[Target, Set, Value])((q, c, d) =>
      q.add(d.target, c))

  //  lazy val reqCodeById: Map[Id, Value] =
//    trie.cataV(UnivEq.emptyMap[Id, Value])((q, c, d) =>
//      q.updated(d.id, c))

//  lazy val reqCodesByTarget: Multimap[Target, Set, Value] =
//    trie.cataV(UnivEq.emptyMultimap[Target, Set, Value])((q, c, d) =>
//      q.add(d.target, c))

//  lazy val targetToIds: Multimap[Target, Set, Id] =
//    trie.cataV(UnivEq.emptyMultimap[Target, Set, Id])((q, _, d) =>
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
final case class Pubid(reqTypeId: ReqType.Id, pos: ReqTypePos)

object Pubid {

  implicit val equality: UnivEq[Pubid] = deriveUnivEq

  /**
   * Once a (reqtype x position) is allocated, it is never removed.
   * Thus, the 0-based position in the vector corresponds with 1-based [[ReqTypePos]] values.
   */
  type Register = Multimap[ReqType.Id, Vector, Req.Id]

  val emptyRegister: Register = UnivEq.emptyMultimap

  def alloc(reqId: Req.Id, reqTypeId: ReqType.Id, register: Register): (Register, Pubid) = {
    val cur = register(reqTypeId)
    val i = cur.indexWhere(_ ≟ reqId)
    if (i >= 0)
      (register, Pubid(reqTypeId, ReqTypePos(i + 1)))
    else
      (register.add(reqTypeId, reqId), Pubid(reqTypeId, ReqTypePos(cur.size + 1)))
  }

  def lookup(register: Register, id: Pubid): Option[Req.Id] = {
    val v = register(id.reqTypeId)
    val i = id.pos.value - 1
    try {
      Some(v(i))
    } catch {
      case _: IndexOutOfBoundsException => None
    }
  }
}

// ===================================================================================================================
// Requirements

/** [[Req]] = [[GenericReq]] */
sealed abstract class Req {
  val id: Req.Id
  val pubid: Pubid
  val alive: Alive

  @inline final def reqTypeId = pubid.reqTypeId
}
object Req {

  /** type [[Id]] = [[Req.Id]] | [[GenericReq.Id]] */
  sealed trait Id extends TaggedLong with ReqCode.Target

  object IdAccess extends ObjDataId[Req.type, Req, Id] {
    override def id(d: Req) = d.id
    override val unapplyData: AnyRef => Option[Req] = {case r: Req => Some(r); case _ => None}
  }
}

final case class GenericReq(id         : GenericReq.Id,
                            pubid      : Pubid,
                            title      : Text.GenericReqTitle.OptionalText,
                            // TODO lastUpdated. Need JS-compat datetimeTZ
                            alive      : Alive) extends Req
object GenericReq {
  final case class Id(value: Long) extends TaggedLong with Req.Id
  implicit val equality: UnivEq[GenericReq] = deriveUnivEq
}


object ReqFieldData {
  type Text         = Map[CustomField.Text.Id, Map[Req.Id, Text.CustomTextField.NonEmptyText]]
  type Tags         = Multimap[Req.Id, Set, ApplicableTag.Id]


  /** Unidirectional implication data */
  type ImplicationsU = Multimap[Req.Id, Set, Req.Id]

  def implicationCycleDetector =
    CycleDetector.Directed.multimap[Set, Req.Id, Long](_.value, UnivEq.emptySet)

  case class Implications(srcToTgt: ImplicationsU) {
    lazy val tgtToSrc: ImplicationsU = srcToTgt.reverse

    def members: Set[Req.Id] =
      srcToTgt.m.toStream.foldLeft(UnivEq.emptySet[Req.Id]) {
        case (q, (k, vs)) => q + k ++ vs
      }
  }
}


case class ReqFieldData(text        : ReqFieldData.Text,
                        tags        : ReqFieldData.Tags,
                        implications: ReqFieldData.Implications)

case class Requirements(reqs: IMap[Req.Id, Req], pubids: Pubid.Register) {

  def req(id: Req.Id): Option[Req] =
    reqs.get(id)

  def reqByPubid(id: Pubid): Option[Req] =
    reqIdByPubid(id) flatMap req

  def reqIdByPubid(id: Pubid): Option[Req.Id] =
    Pubid.lookup(pubids, id)

  def reqM(id: Req.Id): Must[Req] =
    Must.fromOption(req(id), s"Req $id not found.")

  def reqByPubidM(id: Pubid): Must[Req] =
    Must.fromOption(reqByPubid(id), s"Req for $id not found.")

  def reqIdByPubidM(id: Pubid): Must[Req.Id] =
    Must.fromOption(reqIdByPubid(id), s"Req for $id not found.")

  def reqsByPubidM[M[X] <: TraversableOnce[X]: Monoidish](ids: M[Pubid]): Must[M[Req]] =
    Must.foldMapM(ids)(reqByPubidM)

  lazy val reqsByType: Multimap[ReqType.Id, Set, Req] =
    UnivEq.emptyMultimap[ReqType.Id, Set, Req]
      .addPairs(reqs.vstream(_.mapStrengthL(_.reqTypeId)): _*)
}
