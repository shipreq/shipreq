package shipreq.webapp.base.data

import japgolly.nyaya.util.Multimap
import monocle.macros.Lenser
import shipreq.base.util.{Must, IMap, BiMap}
import shipreq.base.util.TaggedTypes._
import scalaz.NonEmptyList

// ===================================================================================================================
// ReqCodes: A hierarchy of semantic IDs

/**
 * A textual ID that refers to a requirement.
 *
 * Eg. "system.email.failure" would be TODO ???????????????? ReqCode("failure", "email" :: "system" :: Nil)
 *
 * Each [[ReqCode]] only refers to a single target, but requirements can have 0..n [[ReqCode]]s.
 */
final case class ReqCode(backwards: NonEmptyList[ReqCode.Node]) {
//    def asc = (last :: secondLastToRoot).reverse
  def txt: String = ???
}

// TODO all req code text should be lowercase

/**
 * [[ReqCode.Trie]] contains the hierarchy of codes and their targets.
 * [[ReqCode.Nodes]] contains the textual values of nodes in the trie.
 * [[ReqCodes]] is a bundle of all req-codes in a project.
 */
object ReqCode {

  /* TODO Make ReqCode.Node memory-efficient
  final class ReqCodeNode private (val value: String) {
    //override def equals(o: Any) = o match {case b:ReqCodeNode }
    override def hashCode = value.##
    override def toString = s"ReqCodeNode($value)"
  }
  object ReqCodeNode {
    implicit val equality: Equal[ReqCodeNode] = Equal.equalRef

    private[this] val cache = new java.util.HashMap[String, ReqCodeNode](128)

    def apply(value: String): ReqCodeNode = {
      println("yarrrrrrrr")
      var r = cache.get(value)
      if (null == r)
        synchronized {
          r = cache.get(value) // unnecessary in JS
          if (null == r)
            r = cache.put(value, new ReqCodeNode(value))
        }
      r
    }
  }
  */
  /**
   * Portion of a [[ReqCode]], separated by ".".
   *
   * Eg. "mail" in "system.mail.failure"
   */
  final case class Node(value: String) extends TaggedString

  final case class NodeId(value: Long) extends TaggedLong

  /**
   * Something to which a [[ReqCode]] can refer.
   *
   * [[Target]] = [[ReqCodeGroup.Id]] | [[Req.Id]]
   */
  sealed trait Target

  final case class TrieNode(target: Option[Target], next: Trie)

  type Trie = Map[NodeId, TrieNode]
  object Trie {
    val empty: Trie = Map.empty

    def fold[A](trie: Trie, z: => A)(f: (A, NonEmptyList[NodeId], Option[Target]) => A): A =
      foldP[A, List[NodeId], NonEmptyList[NodeId]](trie, z, Nil, _.list)(
        (p, i) => NonEmptyList.nel(i, p), f)

    def foldP[A, P0, P1](trie: Trie, z: => A, pz: => P0, p0: P1 => P0)(p1: (P0, NodeId) => P1, f: (A, P1, Option[Target]) => A): A = {
      def traverseT(q: A, path: P0, t: Trie): A =
        t.foldLeft(q) {
          case (q2, (id, node)) => traverseN(q2, p1(path, id), node)
        }

      @inline def traverseN(q: A, path: P1, node: TrieNode): A = {
        val q2 = f(q, path, node.target)
        traverseT(q2, p0(path), node.next)
      }

      traverseT(z, pz, trie)
    }
  }

  type Nodes = Map[NodeId, Node]
}

final case class ReqCodes(trie: ReqCode.Trie, nodes: ReqCode.Nodes) {
  import ReqCode.{NodeId, Node, Target, Trie}

  private def foldN[A](z: => A)(f: (A, NonEmptyList[Node], Option[Target]) => A): Must[A] = {
    val getNode: NodeId => Must[Node] = id => Must.fromOption(nodes get id, s"Node missing for $id\nNodes: $nodes")
    Trie.foldP[Must[A], Must[List[Node]], Must[NonEmptyList[Node]]](trie, z, Nil, _.map(_.list))(
        (mp, id)      => mp.flatMap(p => getNode(id).map(n => NonEmptyList.nel(n, p))),
        (ma, mp, tgt) => ma.flatMap(a => mp.map(p => f(a, p, tgt))))
  }

  lazy val byTargetMap: Must[Multimap[Target, Set, ReqCode]] =
    foldN[Multimap[Target, Set, ReqCode]](Multimap.empty)((q, path, tgt) =>
      tgt.fold(q)(q.add(_, ReqCode(path))))

  val byTarget: Target => Must[Set[ReqCode]] =
    byTargetMap.fold(e => Function const Must.Failed(e), _.apply)

  def nodeIdsInTrie: List[NodeId] =
    Trie.foldP[List[NodeId], Unit, NodeId](trie, Nil, (), _ => ())((_,i) => i, (q, i, _) => i :: q)
}


/**
 * A row that exists just to provide a description or summary of its children in the code hierarchy.
 *
 * Previously called "Semantic Header Row" or "SHR" in the requirements.
 */
final case class ReqCodeGroup(id: ReqCodeGroup.Id, desc: String)
object ReqCodeGroup {
  final case class Id(value: Long) extends TaggedLong with ReqCode.Target
}

// ===================================================================================================================
// Public IDs (like MF-3)

/**
 * A position (ordinal) in a req-type's ordered list of requirements.
 *
 * Eg. the "3" in "FR-3".
 *
 * @param value > 0.
 */
final case class ReqTypePos(value: Int) extends TaggedInt

/**
 * Public ID: A requirement's ID from the public's point-of-view.
 *
 * Eg. "FR-3"
 */
final case class Pubid(reqTypeId: ReqType.Id, pos: ReqTypePos)

object Pubid {

  /**
   * Once a (reqtype x position) is allocated, it is never removed.
   * Thus, the 0-based position in the vector corresponds with 1-based [[ReqTypePos]] values.
   */
  type Register = Multimap[ReqType.Id, Vector, Req.Id]

  val emptyRegister: Register = Multimap.empty
}

// ===================================================================================================================
// Requirements

/** [[Req]] = [[GenericReq]] */
sealed trait Req {
  val id: Req.Id
  val pubId: Pubid
}
object Req {

  /** [[Req.Id]] = [[GenericReq.Id]] */
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
                            pubId      : Pubid,
                            desc       : String,
                            // TODO lastUpdated. Need JS-compat datetimeTZ
                            alive      : Alive) extends Req {
  @inline def reqTypeId = pubId.reqTypeId
}
object GenericReq {
  final case class Id(value: Long) extends TaggedLong with Req.Id
}


object ReqFieldData {
  type Text         = Map[CustomField.Text.Id, Map[Req.Id, String]] // TODO String should be the rich text AST
  type Tags         = Map[Req.Id, Set[ApplicableTag.Id]]
  type Implications = BiMap[Req.Id, Req.Id]
}

case class ReqFieldData(text        : ReqFieldData.Text,
                        tags        : ReqFieldData.Tags,
                        implications: ReqFieldData.Implications)

case class Requirements(reqs: IMap[Req.Id, Req], pubids: Pubid.Register) {

  def req(id: Req.Id): Option[Req] =
    reqs.get(id)

  def reqByPubid(id: Pubid): Option[Req] =
    reqIdByPubid(id) flatMap req

  def reqIdByPubid(id: Pubid): Option[Req.Id] = {
    val v = pubids(id.reqTypeId)
    val i = id.pos.value - 1
    try {
      Some(v(i))
    } catch {
      case _: IndexOutOfBoundsException => None
    }
  }
}