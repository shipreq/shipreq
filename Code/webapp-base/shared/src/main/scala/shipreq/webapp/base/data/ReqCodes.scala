package shipreq.webapp.base.data

import nyaya.util.Multimap
import monocle.macros.Lenses
import scalaz.{Equal, Order}
import scalaz.std.string.stringInstance
import shipreq.base.util._
import shipreq.base.util.TaggedTypes._
import shipreq.base.util.univeq._
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

  def debugShowCodes(codes: TraversableOnce[Value]): String =
    codes.toList
      .map(valueToStr(_, '.'))
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

    implicit val ordering = order.toScalaOrdering

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
  type DeadGroup = Option[DeadReqCodeGroup]

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

    protected final def _inactiveIds: List[ReqCodeId] = {
      val a = reqInactive.m.valuesIterator.flatMap(_.iterator).toList
      deadGroup.fold(a)(_.id :: a)
    }
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
    override def ids      = id :: _inactiveIds
    override def modReqInactive(f: ReqInactive => ReqInactive) =
      copy(reqInactive = f(reqInactive))
  }

  @Lenses
  case class ActiveGroup(group: LiveReqCodeGroup, reqInactive: ReqInactive) extends Data {
    @inline  def id        = group.id
    override def nonEmpty  = true
    override def isActive  = true
    override def activeId  = Some(id)
    override def deadGroup = None
    override def ids       = id :: _inactiveIds
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
sealed abstract class ReqCodeGroup {
  val id: ReqCodeId
  val title: Text.ReqCodeGroupTitle.OptionalText
  def live: Live

  final def isEmpty : Boolean = title.isEmpty
  final def nonEmpty: Boolean = !isEmpty
}

@Lenses
final case class LiveReqCodeGroup(id: ReqCodeId, title: Text.ReqCodeGroupTitle.OptionalText) extends ReqCodeGroup {
  override def live = Live
}

@Lenses
final case class DeadReqCodeGroup(id: ReqCodeId, title: Text.ReqCodeGroupTitle.OptionalText) extends ReqCodeGroup {
  override def live = Dead
}

object ReqCodeGroup {
  implicit def equalLive: UnivEq[LiveReqCodeGroup] = UnivEq.derive
  implicit def equalDead: UnivEq[DeadReqCodeGroup] = UnivEq.derive
  implicit def equalBase: UnivEq[ReqCodeGroup]     = UnivEq.derive
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

  def getById(id: ReqCodeId): Option[Data] =
    reqCodesById.get(id).flatMap(get)

  def lookup(id: ReqCodeId): Data =
    apply(reqCode(id))

  def reqCode(id: ReqCodeId): Value =
    reqCodesById get id mustExistElse s"No req code associated with $id."

  private lazy val scan = new Scan
  private class Scan {
    private val _idList         = List.newBuilder[ReqCodeId]
    private val _idSet          = Set.newBuilder[ReqCodeId]
    private val _groups         = List.newBuilder[ReqCodeGroup]
    private val _reqCodesById   = Map.newBuilder[ReqCodeId, Value]
    var _activeReqCodesByReqId: Multimap[ReqId, Set, Value] = UnivEq.emptySetMultimap
    var _inactiveIdsByReqId: Multimap[ReqId, Set, ReqCodeId] = UnivEq.emptySetMultimap

    trie.foreachPathAndValue { (code, data) =>

      for (id <- data.ids) {
        _idList += id
        _idSet += id
        _reqCodesById += ((id, code))
      }

      _inactiveIdsByReqId ++= data.reqInactive.m

      data match {
        case d: ActiveReq   => _activeReqCodesByReqId = _activeReqCodesByReqId.add(d.reqId, code)
        case d: ActiveGroup => _groups += d.group
        case _: Inactive    => ()
      }

      data.deadGroup.map(_groups += _)
    }

    val groups                = _groups.result()
    val reqCodesById          = _reqCodesById.result()
    val idList                = _idList.result()
    val idSet                 = _idSet.result()
    val activeReqCodesByReqId = _activeReqCodesByReqId
    val inactiveIdsByReqId    = _inactiveIdsByReqId
  }

  /** All groups, dead and live. */
  @inline def groups: List[ReqCodeGroup] =
    scan.groups

  @inline def reqCodesById: Map[ReqCodeId, Value] =
    scan.reqCodesById

  @inline def activeReqCodesByReqId: Multimap[ReqId, Set, Value] =
    scan.activeReqCodesByReqId

  /** Unlike the active case, the same code can have multiple inactive IDs. */
  @inline def inactiveIdsByReqId: Multimap[ReqId, Set, ReqCodeId] =
    scan.inactiveIdsByReqId

  /**
   * Active and inactive [[ReqCodeId]]s alike.
   *
   * This is needed in addition to [[idSet]] so that [[DataProp]] can detect duplicate IDs.
   */
  @inline def idList: List[ReqCodeId] =
    scan.idList

  /** Active and inactive [[ReqCodeId]]s alike. */
  @inline def idSet: Set[ReqCodeId] =
    scan.idSet
}

object ReqCodes {
  implicit lazy val equality: Equal[ReqCodes] = UtilMacros.deriveEqual
  def empty: ReqCodes = ReqCodes(Map.empty)
}
