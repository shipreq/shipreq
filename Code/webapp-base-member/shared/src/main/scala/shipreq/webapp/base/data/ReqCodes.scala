package shipreq.webapp.base.data

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.scalaz_ext.ScalazMacros
import japgolly.microlibs.utils.Memo
import monocle.macros.Lenses
import nyaya.util.Multimap
import scala.collection.immutable.ArraySeq
import scalaz.std.string.stringInstance
import scalaz.{Equal, Order}
import shipreq.base.util.TaggedTypes._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.util.Must._

sealed trait ReqCodeId extends TaggedInt

final case class ReqCodeGroupId(value: Int) extends ReqCodeId

/** Id of a req-code that can be applied to a subject (as opposed to req-code groups). */
final case class ApReqCodeId(value: Int) extends ReqCodeId

object ApReqCodeId {
  final case class AndValue(id: ApReqCodeId, value: ReqCode.Value) {
    def toTupleIV: (ApReqCodeId, ReqCode.Value) = (id, value)
    def toTupleVI: (ReqCode.Value, ApReqCodeId) = (value, id)
  }
  implicit def univEqAndValue: UnivEq[AndValue] = UnivEq.derive
}

/**
 * ReqCodes: A hierarchy of semantic IDs
 *
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

  object Value {

    /** For speed/mem efficiency */
    def toStr(v: Value, sep: Char): String = {
      val head = v.head.value
      if (v.tail.isEmpty)
        head
      else
        Util.quickSB(head, sb =>
          v.tail.foreach { n =>
            sb append sep
            sb append n.value
          }
        )
    }

    /** Unsafe because it assumes perfection, doesn't handle errors or additional whitespace etc */
    def unsafeFromStr(s: String, sep: Char): Value =
      NonEmptyVector.force(s.split(sep).iterator.map(Node.applyFn).toVector)
  }

  def debugShowCodes(codes: IterableOnce[Value]): String =
    codes.iterator.toList
      .map(Value.toStr(_, '.'))
      .sorted
      .map("\n  - " + _).mkString("")

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

    implicit val ordering: Ordering[Node] =
      order.toScalaOrdering

    val applyFn: String => Node =
      Memo(new Node(_))

    @inline def apply(value: String): Node =
      applyFn(value)
  }

  /** Inactive associations to a ReqCode by reqs.
    *
    * When a req is dead, all its ReqCodes move into this.
    *
    * When a req is live, it can also contain IDs referenced in rich text that have been renamed such that they now share
    * a ReqCode (i.e. Give a req two codes [a] & [b], create refs to both, change req's codes to just [c], [c] gets [a]'s
    * ID actively and [b]'s ID inactively here).
    */
  type ReqInactive = Multimap[ReqId, Set, ApReqCodeId]
  def emptyReqInactive: ReqInactive = UnivEq.emptySetMultimap

  /**
   * A [[CodeGroup]] previously assigned to a ReqCode, since deleted.
   */
  type DeadGroup = Option[DeadCodeGroup]

  /**
   * Data stored at each node in the ReqCode trie.
   */
  sealed abstract class Data {
    def nonEmpty: Boolean

    @inline final def isEmpty = !nonEmpty

    def isActive: Boolean

    def activeId: Option[ReqCodeId]

    val reqInactive: ReqInactive

    def modReqInactive(f: ReqInactive => ReqInactive): Data

    def deadGroup: DeadGroup

    /** Active & inactive */
    def ids: List[ReqCodeId]

    protected final def _deadIds: List[ReqCodeId] = {
      val a: List[ReqCodeId] = reqInactive.m.valuesIterator.flatMap(_.iterator).toList
      deadGroup.fold(a)(_.id :: a)
    }
  }

  @Lenses
  final case class Inactive(deadGroup: DeadGroup, reqInactive: ReqInactive) extends Data {
    override def nonEmpty = deadGroup.nonEmpty || reqInactive.nonEmpty
    override def isActive = false
    override def activeId = None
    override def ids      = _deadIds
    override def modReqInactive(f: ReqInactive => ReqInactive) =
      copy(reqInactive = f(reqInactive))
  }

  @Lenses
  final case class ActiveReq(id: ApReqCodeId, reqId: ReqId, deadGroup: DeadGroup, reqInactive: ReqInactive) extends Data {
    override def nonEmpty = true
    override def isActive = true
    override def activeId = Some(id)
    override def ids      = id :: _deadIds
    override def modReqInactive(f: ReqInactive => ReqInactive) =
      copy(reqInactive = f(reqInactive))
  }

  @Lenses
  final case class ActiveGroup(group: LiveCodeGroup, reqInactive: ReqInactive) extends Data {
    @inline  def id        = group.id
    override def nonEmpty  = true
    override def isActive  = true
    override def activeId  = Some(id)
    override def deadGroup = None
    override def ids       = id :: _deadIds
    override def modReqInactive(f: ReqInactive => ReqInactive) =
      copy(reqInactive = f(reqInactive))
  }

  object Data {
    val empty = Inactive(None, UnivEq.emptySetMultimap)
  }

  implicit def equalInactive   : UnivEq[Inactive]    = UnivEq.derive
  implicit def equalActiveReq  : UnivEq[ActiveReq]   = UnivEq.derive
  implicit def equalActiveGroup: UnivEq[ActiveGroup] = UnivEq.derive
  implicit def equalData       : UnivEq[Data]        = UnivEq.derive

  val  Trie = new MTrie.Types[Node, Data]
  type Trie = Trie.Trie

  val  CodeSet = new MTrie.Types[Node, Unit]
  type CodeSet = CodeSet.Trie
}

/**
 * A row that exists just to provide a description or summary of its children in the ReqCode hierarchy.
 *
 * Previously called "Semantic Header Row" or "SHR" in the requirements.
 */
sealed abstract class CodeGroup {
  val id: ReqCodeGroupId
  val title: Text.CodeGroupTitle.OptionalText
  def live: Live

  final def isEmpty : Boolean = title.isEmpty
  final def nonEmpty: Boolean = !isEmpty
}

@Lenses
final case class LiveCodeGroup(id: ReqCodeGroupId, title: Text.CodeGroupTitle.OptionalText) extends CodeGroup {
  override def live = Live
}

@Lenses
final case class DeadCodeGroup(id: ReqCodeGroupId, title: Text.CodeGroupTitle.OptionalText) extends CodeGroup {
  override def live = Dead
}

object CodeGroup {
  implicit def equalLive: UnivEq[LiveCodeGroup] = UnivEq.derive
  implicit def equalDead: UnivEq[DeadCodeGroup] = UnivEq.derive
  implicit def equalBase: UnivEq[CodeGroup]     = UnivEq.derive
}

// =====================================================================================================================

/** This is currently derived automatically and stored as a lazy val.
  * On one hand that means it's always correct, but on the other hand it's the biggest bottleneck (by far) of event
  * application. Eventually this should be manually maintained for speed. It should also be compared to its derivation
  * in [[DataProp]].
  */
final case class ReqCodeManifest(apReqCodesById       : Map[ApReqCodeId, ReqCode.Value],
                                 reqCodeGroupsById    : Map[ReqCodeGroupId, ReqCode.Value],
                                 activeReqCodesByReqId: Multimap[ReqId, Set, ReqCode.Value],
                                 inactiveIdsByReqId   : Multimap[ReqId, Set, ApReqCodeId])

object ReqCodeManifest {
  implicit def univEq: UnivEq[ReqCodeManifest] = UnivEq.derive
}

/**
 * All req code data for in a project.
 */
@Lenses
final case class ReqCodes(trie: ReqCode.Trie) {
  import ReqCode._

  private[data] lazy val scan =
    new derivation.ReqCodeTrieScan(trie)

  private[this] lazy val manifest =
    derivation.ReqCodeTrieScan.deriveManifest(trie)

  def isEmpty: Boolean =
    trie.isEmpty

  def get(code: Value): Option[Data] =
    trie.lookup(code)

  def need(code: Value): Data =
    get(code) mustExistElse ErrorMsg(s"No node at reqcode ${code.whole mkString "."}.")

  def apReqCodesById: Map[ApReqCodeId, Value] =
    manifest.apReqCodesById

  def reqCodeGroupsById: Map[ReqCodeGroupId, Value] =
    manifest.reqCodeGroupsById

  def activeReqCodesByReqId: Multimap[ReqId, Set, Value] =
    manifest.activeReqCodesByReqId

  /** Unlike the active case, the same code can have multiple inactive IDs. */
  def inactiveIdsByReqId: Multimap[ReqId, Set, ApReqCodeId] =
    manifest.inactiveIdsByReqId

  /** Active and inactive [[ReqCodeId]]s alike.
   *
   * This is needed in addition to [[idSet]] so that [[DataProp]] can detect duplicate IDs.
   */
  def idSeq: ArraySeq[ReqCodeId] =
    scan.idSeq

  /** Active and inactive [[ReqCodeId]]s alike. */
  lazy val idSet: Set[ReqCodeId] =
    idSeq.toSet

  def getReqCode(id: ReqCodeId): Option[Value] =
    id match {
      case i: ApReqCodeId    => apReqCodesById.get(i)
      case i: ReqCodeGroupId => reqCodeGroupsById.get(i)
    }

  def reqCode(id: ReqCodeId): Value =
    getReqCode(id) mustExistElse ErrorMsg(s"No req code associated with $id.")

  def getById(id: ReqCodeId): Option[Data] =
    getReqCode(id).flatMap(get)

  def needById(id: ReqCodeId): Data =
    getById(id) mustExistElse ErrorMsg(s"No reqcode with id #${id.value}.")

  def liveGroup(id: ReqCodeGroupId): Option[LiveCodeGroup] =
    scan.liveGroupsById.get(id)

  def liveGroups: List[LiveCodeGroup] =
    scan.liveGroups

  def liveGroupIds: Set[ReqCodeGroupId] =
    scan.liveGroupsById.keySet

  /** All groups, dead and live. */
  def groups: List[CodeGroup] =
    scan.groups
}

object ReqCodes {

  val empty: ReqCodes =
    ReqCodes(Map.empty)

  implicit lazy val equality: Equal[ReqCodes] = ScalazMacros.deriveEqual
}
