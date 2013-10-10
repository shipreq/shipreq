package com.beardedlogic.usecase.lib

import org.scalatest.FunSuite
import scalaz.syntax.equal._
import com.beardedlogic.usecase.test.TestData
import change.Changes.StepAdded
import Lenses._
import Types.{@@, Validated}
import UseCaseEquality._

class UseCaseEqualityTest extends FunSuite with TestData {

  def assertEqual(a: UseCase, b: UseCase): Unit = {
    assert(a ≟ b, "UseCases should be equal.\nA:\n$a\nB:\n$b")
  }
  def assertNotEqual(a: UseCase, b: UseCase): Unit = {
    assert(a ≠ b, s"UseCases should not be equal.\nA:\n$a\nB:\n$b")
  }

  def testStringChange(f: UseCase => (String => String) => UseCase): Unit = {
    val a = MockUc4.UC
    val b = f(a)(_ + "!")
    val c = f(b)(_.dropRight(1))
    assertNotEqual(a, b)
    assertNotEqual(b, c)
    assertEqual(a, c)
  }

  test("No change") {
    val a = MockUc4.UC
    assertEqual(a, a)
  }

  test("Changes in header") {
    testStringChange(uc => f => ucTitleL.mod(f.asInstanceOf[String @@ Validated => String @@ Validated], uc))
  }

  test("Changes in text field text") {
    testStringChange(uc => f => ucTextFieldTextL.mod(s => (f(s), uc.stepsAndLabels), (uc, TF1)))
  }

  test("Changes in step text") {
    testStringChange(uc => f => ucStepTextTextL.mod(s => (f(s), uc.stepsAndLabels), (uc, (NCF, X5))))
  }

  test("Changes in step tree") {
    val a = MockUc4.UC
    val b = NCF.addTailStep(a).gimme
    assertNotEqual(a, b)

    val (c, cl) = NCF.addStep(X2)(b).openChange
    assertNotEqual(b, c)
    val cn = cl.collectFirst {case (_, StepAdded(_, n)) => n}.get

    val d = NCF.removeStep(cn.id)(c).gimme
    assertNotEqual(c, d)
    assertEqual(b, d)

    val e = NCF.increaseIndent(X5)(c).gimme
    assertNotEqual(d, e)
  }
}
