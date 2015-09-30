package shipreq.webapp.base.data

import nyaya.util.Multimap
import monocle.macros.Lenses
import scalaz.{Equal, Order}
import scalaz.std.string.stringInstance
import shipreq.base.util._
import shipreq.base.util.TaggedTypes._
import shipreq.webapp.base.text.Text, Text.Equality._
import shipreq.webapp.base.util.Must._

final case class ReqCodeId(value: Int) extends TaggedInt

/**
 * ReqCodes: A hierarchy of semantic IDs
 *
 * [[ReqCode.Trie]] contains the hierarchy of codes and their targets.
 * [[ReqCodes]] is a bundle of all req-codes in a project.
 */
object ReqCode {

  case class IdAndValue(id: ReqCodeId, value: Value) {
    @inline def toTupleIV: (ReqCodeId, Value) =
      (id, value)
    @inline def toTupleVI: (Value, ReqCodeId) =
      (value, id)
  }
  implicit def idAndValueEquality: UnivEq[IdAndValue] = UnivEq.derive

  /**
   * A textual ID that refers to a requirement.
   *
   * Eg. "system.email.failure" would be `NonEmptyVector(Node("system"), Node("email"), Node("failure"))`.
   */
  type Value = NonEmptyVector[Node]

  /** For speed/mem efficiency */
  def valueToStr(v: Value, sep: Char): String = {
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
      Memo(new Node(_))

    @inline def apply(value: String): Node =
      applyFn(value)
  }

  /**
   * Inactive associations to a ReqCode by reqs.
   *
   * When a req is dead, all its ReqCodes move into this.
   *
   * When a req is live, it can also contain IDs referenced in rich text that have been renamed such that they now share
   * a ReqCode (i.e. Give a req two codes [a] & [b], create refs to both, change req's codes to just [c], [c] gets [a]'s
   * ID actively and [b]'s ID inactively here).
   */
  type ReqInactive = Multimap[ReqId, Set, ReqCodeId]
  def emptyReqInactive: ReqInactive = UnivEq.emptySetMultimap

  /**
   * A [[ReqCodeGroup]] previously assigned to a ReqCode, since deleted.
   */
  type DeadGroup = Option[ReqCodeGroup.AndId]

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
    def ids: Stream[ReqCodeId]

    protected final def _inactiveIds: Stream[ReqCodeId] =
      deadGroup.toStream.map(_.id) append reqInactive.allValues
  }

  @Lenses
  case class Inactive(deadGroup: DeadGroup, reqInactive: ReqInactive) extends Data {
    override def nonEmpty = deadGroup.nonEmpty || reqInactive.nonEmpty
    override def isActive = false
    override def activeId = None
    override def ids      = _inactiveIds
    override def modReqInactive(f: ReqInactive => ReqInactive) =
      copy(reqInactive = f(reqInactive))
  }

  @Lenses
  case class ActiveReq(id: ReqCodeId, reqId: ReqId, deadGroup: DeadGroup, reqInactive: ReqInactive) extends Data {
    override def nonEmpty = true
    override def isActive = true
    override def activeId = Some(id)
    override def ids      = id #:: _inactiveIds
    override def modReqInactive(f: ReqInactive => ReqInactive) =
      copy(reqInactive = f(reqInactive))
  }

  @Lenses
  case class ActiveGroup(groupAndId: ReqCodeGroup.AndId, reqInactive: ReqInactive) extends Data {
    @inline  def id        = groupAndId.id
    @inline  def group     = groupAndId.group
    override def nonEmpty  = true
    override def isActive  = true
    override def activeId  = Some(id)
    override def deadGroup = None
    override def ids       = id #:: _inactiveIds
    override def modReqInactive(f: ReqInactive => ReqInactive) =
      copy(reqInactive = f(reqInactive))
  }
  object ActiveGroup {
    val group = groupAndId ^|-> ReqCodeGroup.AndId.group
    val id    = groupAndId ^|-> ReqCodeGroup.AndId.id
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
@Lenses
final case class ReqCodeGroup(title: Text.ReqCodeGroupTitle.OptionalText) {
  @inline def isEmpty : Boolean = title.isEmpty
  @inline def nonEmpty: Boolean = !isEmpty

  @inline def and(id: ReqCodeId): ReqCodeGroup.AndId =
    ReqCodeGroup.AndId(id, this)

  // TODO Not true anymore ↓
  def live = Live
  def recoverable = true
}

object ReqCodeGroup {
  val empty = ReqCodeGroup(Vector.empty)

  @Lenses
  final case class AndId(id: ReqCodeId, group: ReqCodeGroup)

  implicit def equality     : UnivEq[ReqCodeGroup] = UnivEq.derive
  implicit def andIdEquality: UnivEq[AndId]        = UnivEq.derive
}

/**
 * All req code data for in a project.
 */
@Lenses
final case class ReqCodes(trie: ReqCode.Trie) {
  import ReqCode._
  import MTrie.Ops

  def apply(code: Value): Data =
    get(code) mustExistElse s"No node at reqcode ${code.whole mkString "."}."

  def get(code: Value): Option[Data] =
    trie.lookup(code)

  def lookup(id: ReqCodeId): Data =
    apply(reqCode(id))

  def reqCode(id: ReqCodeId): Value =
    reqCodesById get id mustExistElse s"No req code associated with $id."

  private lazy val scan = new Scan
  private class Scan {
    private val _allIds         = Stream.newBuilder[ReqCodeId]
    private val _activeGroups   = List.newBuilder[ReqCodeGroup.AndId]
    private val _inactiveGroups = List.newBuilder[ReqCodeGroup.AndId]
    private val _reqCodesById   = Map.newBuilder[ReqCodeId, Value]
    var _activeReqCodesByReqId: Multimap[ReqId, Set, Value] = UnivEq.emptySetMultimap
    var _inactiveIdsByReqId: Multimap[ReqId, Set, ReqCodeId] = UnivEq.emptySetMultimap

    trie.foreachPathAndValue { (code, data) =>

      val ids = data.ids
      _allIds ++= ids
      _reqCodesById ++= ids.map((_, code))

      _inactiveIdsByReqId ++= data.reqInactive.m

      data.deadGroup.map(_inactiveGroups += _)

      data match {
        case d: ActiveReq   => _activeReqCodesByReqId = _activeReqCodesByReqId.add(d.reqId, code)
        case d: ActiveGroup => _activeGroups += d.groupAndId
        case _: Inactive    => ()
      }
    }

    val activeGroups          = _activeGroups.result()
    val inactiveGroups        = _inactiveGroups.result()
    val reqCodesById          = _reqCodesById.result()
    val allIds                = _allIds.result()
    val activeReqCodesByReqId = _activeReqCodesByReqId
    val inactiveIdsByReqId    = _inactiveIdsByReqId
  }

  // TODO Are {in,}activeGroups useful really?
  // 1) (RCG,id) likely isn't enough anymore, better would be (RCG,id,live, maybe code too?)
  // 2) Logic.gather doesn't use this
  @inline def activeGroups         : List[ReqCodeGroup.AndId]        = scan.activeGroups
  @inline def inactiveGroups       : List[ReqCodeGroup.AndId]        = scan.inactiveGroups
  @inline def reqCodesById         : Map[ReqCodeId, Value]           = scan.reqCodesById
  @inline def activeReqCodesByReqId: Multimap[ReqId, Set, Value]     = scan.activeReqCodesByReqId
  @inline def inactiveIdsByReqId   : Multimap[ReqId, Set, ReqCodeId] = scan.inactiveIdsByReqId
  @inline def idStream             : Stream[ReqCodeId]               = scan.allIds

  /** Active and inactive [[ReqCodeId]]s alike. */
  lazy val idSet = idStream.toSet
}

object ReqCodes {
  implicit lazy val equality: Equal[ReqCodes] = UtilMacros.deriveEqual
  def empty: ReqCodes = ReqCodes(Map.empty)
}
