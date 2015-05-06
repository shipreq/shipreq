package shipreq.webapp.base.util

import scala.runtime.AbstractFunction1
import scalaz.Semigroup
import scalaz.std.option._
import scalaz.syntax.semigroup._
import shipreq.base.util.MTrie.Ops._
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


  def data7[X, A, B, C, D, E, F, G](parent: String, a: X => A, b: X => B, c: X => C, d: X => D, e: X => E, f: X => F, g: X => G)
                                   (implicit A: ShowSize[A], B: ShowSize[B], C: ShowSize[C], D: ShowSize[D], E: ShowSize[E], F: ShowSize[F], G: ShowSize[G]): ShowSize[X] =
    ShowSize.lift(x => Node.sum(parent, A(a(x)), B(b(x)), C(c(x)), D(d(x)), E(e(x)), F(f(x)), G(g(x))))

  // ===================================================================================================================
  final case class Node(name: String, size: Int, children: Vector[Node]) {
    def <~(n: Node): Node =
      copy(children = children :+ n)

    def addChildren(ns: (String, Int)*): Node =
      copy(children = ns.foldLeft(children)((q, t) => q :+ Node(t._1, t._2)))

    def countChildren[A](as: Iterable[A])(f: A => String): Node = {
      val m = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
      as.foreach{a =>
        val k = f(a)
        m.update(k, m(k) + 1)
      }
      val n = m.toStream.map(t => Node(t._1, t._2)).sortBy(_.name)
      copy(children = children ++ n)
    }

    def +(b: Node): Node =
      Node.append(this, b)

    def sumChildren: Int =
      children.foldLeft(0)(_ + _.size)

    import japgolly.nyaya.util.Util._

    def showTree: String =
      // asciiTree(this :: Nil)(_.children.filter(_.size != 0), n => s"${n.name}  ${n.size}")
      asciiTree(this :: Nil)(_.children.filter(_.size != 0), n => String.format(">%6d  %s", n.size: java.lang.Integer, n.name))
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

  implicit def customReqTypes: ShowSize[CustomReqTypeIMap] =
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
    ShowSize.lift(r => Node("Requirements", r.reqs.size))
//      .addChildren(r.reqsByType.m.toList.map(x => (x._1, x._2.size))) )

  implicit def reqCodes: ShowSize[ReqCodes] =
    ShowSize.lift(rc =>
      Node("Req codes", rc.trie.cataV(0)((q, _, _) => q + 1))
        .countChildren(rc.trie.flatStream.map(_._2.active.map(_.target))) {
          case Some(_: ReqId)        => "Req target"
          case Some(_: ReqCodeGroup) => "ReqCodeGroup"
          case None                  => "Tombstones"
        })

  implicit def reqFieldData: ShowSize[ReqFieldData] =
    ShowSize.lift(r => Node.sum("Req field-data",
      Node("Implications", r.implications.srcToTgt.vstream(_.size).sum),
      Node("Tags", r.tags.vstream(_.size).sum),
      Node("Text", r.text.values.toStream.flatMap(_.values.toStream).size)))

  implicit def tagTree: ShowSize[TagTree] =
    ShowSize.lift(tt =>
      Node("Tags", tt.size).countChildren(tt.vstream(_.tag)) {
        case _: TagGroup      => "TagGroup"
        case _: ApplicableTag => "ApplicableTag"
      })

  def rev[D](name: String): ShowSize[RevAnd[D]] =
    ShowSize.lift(r => Node(name, r.rev.value.toInt))

  implicit def revData[D](implicit d: ShowSize[D]): ShowSize[RevAnd[D]] =
    d.contramap(_.data)

  implicit def project: ShowSize[Project] =
    ShowSize.data7("Project",
      _.customIssueTypes, _.customReqTypes, _.fields, _.tags, _.reqs, _.reqCodes, _.reqFieldData)
}