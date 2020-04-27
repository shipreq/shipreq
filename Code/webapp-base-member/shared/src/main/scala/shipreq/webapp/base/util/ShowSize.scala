package shipreq.webapp.base.util

import scala.runtime.AbstractFunction1
import scalaz.Semigroup
import scalaz.std.option._
import scalaz.syntax.semigroup._
import ShowSize.Node

class ShowSize[A](f: A => Node) extends AbstractFunction1[A, Node] {
  override def apply(a: A): Node = f(a)

  final def contramap[B](g: B => A): ShowSize[B] =
    ShowSize.lift(f compose g)
}

object ShowSize {

  @inline def apply[A](implicit s: ShowSize[A]): ShowSize[A] = s
  @inline def apply[A](a: A)(implicit s: ShowSize[A]): Node = s(a)

  @inline def lift[A](f: A => Node): ShowSize[A] = new ShowSize(f)


  def data2[X, A, B](parent: String, a: X => A, b: X => B)
                       (implicit A: ShowSize[A], B: ShowSize[B]): ShowSize[X] =
    ShowSize.lift(x => Node.sum(parent, A(a(x)), B(b(x)) ))

  def data3[X, A, B, C](parent: String, a: X => A, b: X => B, c: X => C)
                          (implicit A: ShowSize[A], B: ShowSize[B], C: ShowSize[C]): ShowSize[X] =
    ShowSize.lift(x => Node.sum(parent, A(a(x)), B(b(x)), C(c(x)) ))

  def data4[X, A, B, C, D](parent: String, a: X => A, b: X => B, c: X => C, d: X => D)
                                   (implicit A: ShowSize[A], B: ShowSize[B], C: ShowSize[C], D: ShowSize[D]): ShowSize[X] =
    ShowSize.lift(x => Node.sum(parent, A(a(x)), B(b(x)), C(c(x)), D(d(x)) ))

  def data5[X, A, B, C, D, E](parent: String, a: X => A, b: X => B, c: X => C, d: X => D, e: X => E)
                                   (implicit A: ShowSize[A], B: ShowSize[B], C: ShowSize[C], D: ShowSize[D], E: ShowSize[E]): ShowSize[X] =
    ShowSize.lift(x => Node.sum(parent, A(a(x)), B(b(x)), C(c(x)), D(d(x)), E(e(x)) ))

  def data6[X, A, B, C, D, E, F](parent: String, a: X => A, b: X => B, c: X => C, d: X => D, e: X => E, f: X => F)
                                   (implicit A: ShowSize[A], B: ShowSize[B], C: ShowSize[C], D: ShowSize[D], E: ShowSize[E], F: ShowSize[F]): ShowSize[X] =
    ShowSize.lift(x => Node.sum(parent, A(a(x)), B(b(x)), C(c(x)), D(d(x)), E(e(x)), F(f(x)) ))

  def data7[X, A, B, C, D, E, F, G](parent: String, a: X => A, b: X => B, c: X => C, d: X => D, e: X => E, f: X => F, g: X => G)
                                   (implicit A: ShowSize[A], B: ShowSize[B], C: ShowSize[C], D: ShowSize[D], E: ShowSize[E], F: ShowSize[F], G: ShowSize[G]): ShowSize[X] =
    ShowSize.lift(x => Node.sum(parent, A(a(x)), B(b(x)), C(c(x)), D(d(x)), E(e(x)), F(f(x)), G(g(x)) ))

  // ===================================================================================================================
  final case class Node(name: String, size: Int, children: Vector[Node]) {
    def <~(n: Node): Node =
      copy(children = children :+ n)

    def addChildren(ns: (String, Int)*): Node =
      copy(children = ns.foldLeft(children)((q, t) => q :+ Node(t._1, t._2)))

    def countChildren[A](as: IterableOnce[A])(f: A => String): Node = {
      val m = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
      as.iterator.foreach { a =>
        val k = f(a)
        m.update(k, m(k) + 1)
      }
      val n = m.iterator.map(t => Node(t._1, t._2)).toVector.sortBy(_.name)
      copy(children = children ++ n)
    }

    def +(b: Node): Node =
      Node.append(this, b)

    def sumChildren: Int =
      children.foldLeft(0)(_ + _.size)

    import nyaya.util.Util._

    def showTree: String =
      // asciiTree(this :: Nil)(_.children.filter(_.size != 0), n => s"${n.name}  ${n.size}")
      asciiTree(this :: Nil)(_.children.filter(_.size != 0), n => ">%6d  %s".format(n.size, n.name))
  }

  object Node {
    def apply(name: String, size: Int, children: Node*) =
      new Node(name, size, children.toVector)

    def empty(name: String) =
      new Node(name, 0, Vector.empty)

    def countChildren[A](parent: String, as: Iterable[A])(f: A => String): Node = {
      val x = empty(parent).countChildren(as)(f)
      x.copy(size = x.sumChildren)
    }

    def sum(parent: String, children: Node*) =
      Node(parent, children.foldLeft(0)(_ + _.size), children.toVector)

    def append(a: Node, b: Node): Node = {
      val newSize = a.size + b.size
      if (a.name == b.name) {
        def tomap(ns: Vector[Node]) = ns.foldLeft(Map.empty[String, Node]){(q, n) =>
          val v =
            q.get(n.name) match {
              case None    => n
              case Some(m) => append(n, m)
            }
          q.updated(n.name, v)
        }
        val am = tomap(a.children)
        val bm = tomap(b.children)
        var seen = Set.empty[String]
        var nn = Vector.empty[Node]
        (a.children ++ b.children).foreach {i =>
          val k = i.name
          if (!seen.contains(k)) {
            seen += k
            (am.get(k) |+| bm.get(k)).foreach(nn :+= _)
          }
        }
        Node(a.name, newSize, nn)
      } else
        Node(s"${a.name} & ${b.name}", newSize) <~ a <~ b
    }

    implicit val semigroup: Semigroup[Node] =
      new Semigroup[Node] { override def append(a: Node, b: => Node) = a + b }
  }

  // ===================================================================================================================
  import shipreq.webapp.base.data._

  implicit def customIssueTypes: ShowSize[CustomIssueTypeIMap] =
    ShowSize.lift(m => Node("Custom issue types", m.size))

  implicit def customReqTypes: ShowSize[ReqTypes.Custom] =
    ShowSize.lift(m => Node("Custom req types", m.size))

  implicit def fieldSet: ShowSize[FieldSet] =
    ShowSize.lift(fs => {
      val all = fs.order.size
      val cust = fs.customFields.size
      Node("Fields", all) <~
        Node("Static fields", all - cust) <~
        Node("Custom fields", cust).countChildren(fs.customFields.values) {
          case _: CustomField.Tag         => "Tag"
          case _: CustomField.Text        => "Text"
          case _: CustomField.Implication => "Implication"
        }
    })

  implicit def requirements: ShowSize[Requirements] =
    ShowSize.lift(r =>
      Node("Requirements", r.reqIterator().size)
        .countChildren(r.reqIterator()) {
          case _: GenericReq => "GenericReq"
          case _: UseCase    => "UseCase"
        }
    )
//      .addChildren(r.reqsByType.m.toList.map(x => (x._1, x._2.size))) )

  implicit def reqCodeTrie: ShowSize[ReqCode.Trie] =
    ShowSize.lift(trie =>
      Node("Req codes", trie.cataV(0)((q, _, _) => q + 1))
        .countChildren(trie.flatIterator().map(_._2)) {
          case _: ReqCode.ActiveReq   => "Codes @ reqs"
          case _: ReqCode.ActiveGroup => "Codes @ groups"
          case _: ReqCode.Inactive    => "Tombstones"
        })

  implicit def reqCodes: ShowSize[ReqCodes] =
    reqCodeTrie.contramap(_.trie)

  implicit def reqDataText: ShowSize[ReqData.Text] =
    ShowSize.lift(r => Node("Text", r.data.values.iterator.flatMap(_.values.iterator).size))

  implicit def reqDataTags: ShowSize[ReqData.Tags] =
    ShowSize.lift(r => Node("Tags", r.valuesIterator.map(_.size).sum))

  implicit def implications: ShowSize[Implications] =
    ShowSize.lift(r => Node("Implications", r.forwards.valuesIterator.map(_.size).sum))

  implicit def tagTree: ShowSize[TagTree] =
    ShowSize.lift(tt =>
      Node("Tags", tt.size).countChildren(tt.valuesIterator.map(_.tag)) {
        case _: TagGroup      => "TagGroup"
        case _: ApplicableTag => "ApplicableTag"
      })

  implicit def projectConfig: ShowSize[ProjectConfig] =
    ShowSize.data4("Project config", _.customIssueTypes, _.reqTypes.custom, _.fields, _.tags.tree)

  def projectContent: ShowSize[ProjectContent] =
    ShowSize.data5("Project content", _.reqs, _.reqCodes, _.reqText, _.reqTags, _.implications)

  implicit def project: ShowSize[Project] =
    ShowSize.data2("Project", (_: Project).config, (_: Project).content)(projectConfig, projectContent)
}