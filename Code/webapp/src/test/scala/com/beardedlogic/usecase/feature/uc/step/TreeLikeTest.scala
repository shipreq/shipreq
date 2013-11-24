package com.beardedlogic.usecase.feature.uc.step

import org.scalatest.FunSuite
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, Checkers}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Matchers

class TreeLikeTest extends FunSuite with GeneratorDrivenPropertyChecks with Checkers with Matchers {

  case class N(n: Int, children: List[N]) extends TreeNodeLike[N]
  implicit lazy val ord: Ordering[N] = Ordering.by(_.n)

  def genN: Gen[N] = for {
    num <- arbitrary[Int]
    cs <- Gen.choose(0, 1)
    children <- Gen.listOfN(cs, genN)
  } yield N(num, children)

  implicit lazy val arb = Arbitrary(genN)

  test("iteratorRecursive == flattenRecursive") {
    check((n: N) => {
      n.iteratorRecursive.toList.sorted == n.flattenRecursive.sorted
    })
  }
}
