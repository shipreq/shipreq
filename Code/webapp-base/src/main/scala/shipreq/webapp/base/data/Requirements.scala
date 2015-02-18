package shipreq.webapp.base.data

import japgolly.nyaya.util.Multimap
import monocle.macros.Lenser
import shipreq.base.util.{IMap, BiMap}
import shipreq.base.util.TaggedTypes._
import scalaz.{Memo, Equal, NonEmptyList}

// ===================================================================================================================
// ReqCodes: A hierarchy of semantic IDs

/**
 * A textual ID that refers to a requirement.
 *
 * Eg. "system.email.failure" would be TODO ???????????????? ReqCode("failure", "email" :: "system" :: Nil)
 *
 * Each [[ReqCode]] only refers to a single target, but requirements can have 0..n [[ReqCode]]s.
 */
final case class ReqCode(/*last: ReqCode.Node, secondLastToRoot: List[ReqCode.Node]*/) {
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

  final case class TrieNode(value: Option[Target], next: Trie)

  // ReqCodes are unique and refer to 0..1 (ReqCodeGroup | Req)
  type Trie = Map[NodeId, TrieNode]
  object Trie {
    val empty: Trie = Map.empty
  }

  type Nodes = Map[NodeId, Node]
}

final case class ReqCodes(trie: ReqCode.Trie, nodes: ReqCode.Nodes) {
  import ReqCode.Target

  private[this] lazy val mapByTarget: Map[Target, Set[ReqCode]] = ??? // TODO

  def byTarget(k: Target): Set[ReqCode] =
    mapByTarget.getOrElse(k, Set.empty[ReqCode])
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

case class Requirements(reqs: IMap[Req.Id, Req], pubids: Pubid.Register)