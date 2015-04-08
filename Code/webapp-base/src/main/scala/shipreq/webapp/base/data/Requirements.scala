package shipreq.webapp.base.data

import japgolly.nyaya.CycleDetector
import japgolly.nyaya.util.Multimap
import scala.annotation.tailrec
import scalaz.{Equal, Order}
import scalaz.std.stream.streamEqual
import scalaz.std.string.stringInstance
import scalaz.std.tuple.tuple2Equal
import scalaz.syntax.equal._
import shapeless.{Generic, :+:, CNil, Coproduct, Inl, Inr}
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.TaggedTypes._
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.TypeclassDerivation._

// ===================================================================================================================
// ReqCodes: A hierarchy of semantic IDs

/**
 * A textual ID that refers to a requirement.
 *
 * Eg. "system.email.failure" would be `ReqCode("failure", "email" :: "system" :: Nil)`.
 *
 * Each [[ReqCode]] only refers to a single target, but requirements can have 0..n [[ReqCode]]s.
 */
final case class ReqCode(backwards: NonEmptyVector[ReqCode.Node]) {
  def forwards = backwards.reverse
  def txt: String = forwards.whole.mkString(".") // TODO rename. cache. Also should probably be in Presentation
  override def toString = s"ReqCode($txt)"
}

// TODO all req code text should be lowercase

/**
 * [[ReqCode.Trie]] contains the hierarchy of codes and their targets.
 * [[ReqCodes]] is a bundle of all req-codes in a project.
 */
object ReqCode {

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

  object Node {
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

  implicit val reqCodeOrder = UnivEq.withOrder[ReqCode](
    Order[NonEmptyVector[Node]].contramap(_.backwards))

  /**
   * Something to which a [[ReqCode]] can refer.
   *
   * type [[Target]] = [[ReqCodeGroup.Id]] | [[Req.Id]]
   */
  sealed trait Target extends TrieNode

  implicit object TargetGeneric extends Generic[Target] {
    override type Repr = ReqCodeGroup.Id :+: Req.Id :+: CNil
    override def to  (t: Target): Repr = t match {
      case a: ReqCodeGroup.Id => Coproduct[Repr](a)
      case a: Req.Id          => Coproduct[Repr](a)
    }
    override def from(co: Repr): Target = co match {
      case Inl(a)      => a
      case Inr(Inl(a)) => a
      case _           => ???
    }
  }

  implicit val targetEquality: UnivEq[Target] = deriveUnivEq

  /** [[TrieNode]] = [[TrieBranch]] | [[Target]] (terminal/leaf) */
  sealed trait TrieNode
  final case class TrieBranch(target: Option[Target], next: Trie) extends TrieNode

  type Trie = Map[Node, TrieNode]

  object Trie {
    val empty: Trie = Map.empty

    def simpleFold[A](trie: Trie, z: A)(f: (A, Node, TrieNode) => A): A =
      trie.foldLeft(z) { case (q, (cn, tn)) => tn match {
        case TrieBranch(_, next) => simpleFold(next, f(q, cn, tn))(f)
        case _: Target           => f(q, cn, tn)
      }}

    /** NonEmptyVector[Node] in f is backwards. */
    def fold[A](trie: Trie, z: A)(f: (A, NonEmptyVector[Node], Option[Target]) => A): A =
      foldP[A, Vector[Node], NonEmptyVector[Node]](trie, z, Vector.empty, _.whole)(
        (p, n) => NonEmptyVector(n, p), f)

    def foldP[A, P0, P1](trie: Trie, z: A, pz: => P0, p0: P1 => P0)(p1: (P0, Node) => P1, f: (A, P1, Option[Target]) => A): A = {
      def traverseT(q: A, path: P0, t: Trie): A =
        t.foldLeft(q) {
          case (q2, (cn, tn)) => traverseN(q2, p1(path, cn), tn)
        }

      @inline def traverseN(q: A, path: P1, node: TrieNode): A =
        node match {
          case TrieBranch(tgt, next) => traverseT(f(q, path, tgt), p0(path), next)
          case tgt: Target           => f(q, path, Some(tgt))
        }

      traverseT(z, pz, trie)
    }

    def flatStream(trie: Trie): Stream[(ReqCode, Target)] = {
      @inline def rc(h: Node, p: Vector[Node])= ReqCode(NonEmptyVector(h, p))
      def go(trie: Trie, p: Vector[Node]): Stream[(ReqCode, Target)] =
        trie.toStream.sortBy(_._1.value).flatMap {
          case (cn, TrieBranch(ot, next)) =>
            ot.map(t => (rc(cn, p), t)).toStream append go(next, cn +: p)
          case (cn, t: Target) =>
            Stream((rc(cn, p), t))
        }
      go(trie, Vector.empty)
    }

    def flatten(trie: Trie): Map[ReqCode, Target] =
      Trie.fold(trie, UnivEq.emptyMap[ReqCode, Target])((m, p, ot) =>
        ot.fold(m)(t => m.updated(ReqCode(p), t)))

    def putCF(trie: Trie, codeForwards: NonEmptyVector[Node])(target: Target): Trie = {
      @tailrec def go(t: Trie, codeH: Node, codeT: Vector[Node], unwind: Trie => Trie): Trie =
        if (codeT.isEmpty) {

          // At target-path's end
          val newTrieNode: TrieNode =
            t.get(codeH) match {
              case Some(TrieBranch(_, next)) => TrieBranch(Some(target), next)
              case Some(_: Target) => target
              case None => target
            }
          unwind(t.updated(codeH, newTrieNode))

        } else {

          // Still traversing target-path
          val a = codeT.head
          val b = codeT.tail
          t.get(codeH) match {
            case Some(TrieBranch(ot, onext)) => go(onext, a, b, n ⇒ unwind(t.updated(codeH, TrieBranch(ot, n))))
            case ot @ Some(_: Target)        => go(empty, a, b, n ⇒ unwind(t.updated(codeH, TrieBranch(ot.asInstanceOf[Option[Target]], n))))
            case None                        => go(empty, a, b, n ⇒ unwind(t.updated(codeH, TrieBranch(None, n))))
          }
        }

      go(trie, codeForwards.head, codeForwards.tail, identity)
    }

    def put(trie: Trie, code: ReqCode)(target: Target): Trie =
      putCF(trie, code.backwards.reverse)(target)
  }

  Equal[(ReqCode, Target)]
  implicit val trieEquality: Equal[Trie] =
    Equal[Stream[(ReqCode, Target)]] contramap Trie.flatStream

}

final case class ReqCodes(trie: ReqCode.Trie) { // TODO Needed? Also, rename?
  import ReqCode.{Node, Target, Trie}

  lazy val byTargetMap: Multimap[Target, Set, ReqCode] =
    Trie.fold(trie, UnivEq.emptyMultimap[Target, Set, ReqCode])((q, path, tgt) =>
      tgt.fold(q)(q.add(_, ReqCode(path))))

  @inline def byTarget(t: Target): Set[ReqCode] =
    byTargetMap(t)

  def codeSet: Set[ReqCode] =
    Trie.fold(trie, UnivEq.emptySet[ReqCode])((q, path, ot) =>
      ot.fold(q)(_ => q + ReqCode(path)))
}


/**
 * A row that exists just to provide a description or summary of its children in the code hierarchy.
 *
 * Previously called "Semantic Header Row" or "SHR" in the requirements.
 */
final case class ReqCodeGroup(id: ReqCodeGroup.Id, desc: String)
object ReqCodeGroup {
  final case class Id(value: Long) extends TaggedLong with ReqCode.Target

  implicit val equality: UnivEq[ReqCodeGroup] = deriveUnivEq
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

  object IdAccess extends ObjDataIdM[Req.type, Req, Id] {
    override def id(d: Req) = d.id
    override val unapplyData: AnyRef => Option[Req] = {case r: Req => Some(r); case _ => None}
    override def mkId(l: Long) = GenericReq.Id(l) // This is declared as being for testing only
    override def setId(cf: Req, i: Id) = cf match { // TODO Ideally this should be hidden from non-test code
        case r: GenericReq => r.copy(id = GenericReq.Id(i.value))
      }
  }
}

final case class GenericReq(id         : GenericReq.Id,
                            pubid      : Pubid,
                            desc       : Text.GenericReqDesc.OptionalText,
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
