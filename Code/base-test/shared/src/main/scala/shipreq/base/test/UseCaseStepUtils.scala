package shipreq.base.test

import cats.Eq
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.IndexLabel._
import shipreq.base.util.VectorTree.{Children, Node}
import shipreq.base.util._

object UseCaseStepUtils {

  // (UC-8.)0.1.a.i.4
  val Labels = Vector[IndexLabel](
    NumericFrom0,
    NumericFrom1,
    Alpha,
    Roman,
    NumericFrom1)

  private[this] val lineRegex = """^\s*(\S+?)\. (\S[^\r\n]*?)\s*$""".r
  private[this] val manualIdRegex = "^(.+)(?:\\|id=(.+))\\s*$".r
  //private[this] val topLevelLabel = """^(\S+\.)(\d+)$""".r

  case class FakeStep(id: String, value: String)
  implicit def univEqFakeStep: UnivEq[FakeStep] = UnivEq.derive

  def fakeStepEqByValue: Eq[FakeStep] =
    Eq.instance[FakeStep](_.value ==* _.value)

  val TreeEqByFakeStepValue: Eq[VectorTree[FakeStep]] =
    VectorTree equalityForRoot fakeStepEqByValue

  def assertTreeValues(actual: VectorTree[FakeStep], expect: VectorTree[FakeStep]): Unit =
    if (!TreeEqByFakeStepValue.eqv(actual, expect)) {
      def removeIds(t: VectorTree[FakeStep]) = t.map(_.value)
      val a = removeIds(actual)
      val e = removeIds(expect)
      //assertEq(a, e)
      println(compareTrees("Expected", e, "Actual", a))
      fail("Trees differ.")
    }

  /**
    * Builds a side-by-side, textual representation of two trees.
    */
  def compareTrees[A](title1: String, nodes1: VectorTree[A], title2: String, nodes2: VectorTree[A]): String = {
    def pp(t: VectorTree[A]) = t.prettyPrintIndented().split('\n').toVector
    val sb = new StringBuilder(1024, "")
    val t1 = pp(nodes1)
    val t2 = pp(nodes2)
    val t1Size = (title1 +: t1) map (_.length) max
    val t2Size = (title2 +: t2) map (_.length) max
    val fmt = s"%-${t1Size}s | %-${t2Size}s | %s\n"
    val size = Vector(t1.size, t2.size).max
    def x(l: IndexedSeq[String], i: Int) = if (i >= l.size) "" else l(i)
    sb ++= String.format(fmt, title1, title2, "") +
      ("-" * t1Size + "-+-" + "-" * t2Size + "-+\n")
    for (i <- 0 until size) {
      val (a, b) = (x(t1, i), x(t2, i))
      val c = if (a == b) "" else "#"
      sb ++= String.format(fmt, a, b, c)
    }
    sb.toString
  }

  /**
   * Parses a textual representation of a tree.
   *
   * Each line must match the format "<indent><label>. <step text>"
   *
   * Indents must be spaces in multiples of 2.
   */
  def parseStepTree(txt: String, useTextAsId: Boolean = false): VectorTree[FakeStep] = {
    import scala.collection.mutable.{Map => MutableMap}
    type B = collection.mutable.Builder[FakeStep, Vector[FakeStep]]
    def newB: B = Vector.newBuilder[FakeStep]

    val rootSteps = newB
    val parents   = MutableMap.empty[Int, FakeStep]
    val children  = MutableMap.empty[FakeStep, B]

    val lines = txt.split("""\s*[\r\n]+""").iterator.map(_.replaceFirst("\\s+$", "")).filter(_.nonEmpty).toList
    val commonIndentSize   = lines.map(_.replaceFirst("\\S.+", "").length).min
    val linesWithoutIndent = lines.map(_.substring(commonIndentSize))

    for (line <- linesWithoutIndent) {

      // Parse line
      val indentSize = line.replaceFirst("\\S.+", "").length
      if (indentSize % 2 != 0) throw new RuntimeException("Odd indent size: " + line)
      val indent = indentSize >> 1
      var lineRegex(label, stepText) = line
      if (stepText == "_") stepText = ""

      // Parse manual id, eg. "1.0. Root|id=6"
      val manualIdMatcher = manualIdRegex.pattern.matcher(stepText)
      val idOverride = if (manualIdMatcher.matches) {
        stepText = manualIdMatcher.group(1)
        Some(manualIdMatcher.group(2))
      } else if (useTextAsId) Some(stepText)
      else None

      // Create node
      val n: FakeStep =
        if (indent == 0) {
          val n = FakeStep(idOverride getOrElse label, stepText)
          rootSteps += n
          n
        } else {
          val p = parents(indent - 1)
          val n = FakeStep(idOverride getOrElse s"${p.id}.$label", stepText)
          children(p) += n
          n
        }

      parents(indent) = n
      children(n) = newB
    }

    def addChildren(b: B): Children[FakeStep] =
      b.result().map(s => Node(s, addChildren(children(s))))

    VectorTree(addChildren(rootSteps))
  }

}
