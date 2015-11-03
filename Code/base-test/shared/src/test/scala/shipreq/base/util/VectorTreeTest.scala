package shipreq.base.util

import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import utest._
import shipreq.base.test.BaseUtilGen._
import UnivEq.Implicits._
import VectorTree.{apply => _, _}

object VectorTreeTest extends TestSuite {

  val genIntTree = genVectorTree(Gen.int, 4)(0 to 4)

  def allNodes[A](n: Node[A]): Vector[A] =
    n.children.flatMap(allNodes) :+ n.value

  type PV = Prop[VectorTree[Int]]

  def values: PV =
    Prop.equal("values")(
      _.valueIterator.toVector.sorted,
      _.children.flatMap(allNodes).sorted)

  def locAndValueIterator: PV = {
    def values: PV =
      Prop.equal("values")(
        t => t.locAndValueIterator((_, i) => i).toVector.sorted,
        _.children.flatMap(allNodes).sorted)

    def locations: PV =
      Prop.atom("locs", t => {
        val results = t.locAndValueIterator((_, _))
        val bad = results.filter{ case(l, i) => t.getAtLocation(l) != Option(i) }.toList
        bad.headOption.map{ case(l, i) => s"Bad result: (${l.whole mkString "."}) = ${t.getAtLocation(l)} not $i}" }
      })

    (values ∧ locations) rename "locAndValueIterator"
  }

  def dims: PV =
    Prop.equal("Dims")(
      _.dims,
      t => {
        var ml = 0
        var md = 0
        for (loc <- t.locAndValueIterator((loc, _) => loc)) {
          md = md max loc.length
          ml = ml max (loc.whole.max + 1)
        }
        Dims(maxLength = ml, maxDepth = md)
      })

  def props: PV =
    (values ∧ locAndValueIterator ∧ dims) rename "VectorTree props"

  override def tests = TestSuite {
    props mustBeSatisfiedBy genIntTree
  }
}
