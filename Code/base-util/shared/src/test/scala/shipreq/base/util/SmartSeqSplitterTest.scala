package shipreq.base.util

import scala.collection.immutable.ArraySeq
import sourcecode.Line
import japgolly.microlibs.testutil.TestUtil._
import utest._
import shipreq.base.util.algorithm.GeneticEvolution.Binary.Config

object SmartSeqSplitterTest extends TestSuite {

  private val lineHeight: Double =
    1.0

  private val getLineWidth: ArraySeq[String] => Double =
    _.mkString(" ").length.toDouble

  override def tests = Tests {

    "fillOval" - {

      def run(str: String)(implicit splitter: SmartSeqSplitter[String]): String = {
        val result = SmartSeqSplitter.fillOval[String](
          input        = ArraySeq.unsafeWrapArray(str.split(' ')),
          getLineWidth = getLineWidth,
          lineHeight   = lineHeight,
          idealWidth   = 20,
          tolerance    = 0,
          splitter     = splitter,
        )
        SmartSeqSplitter.draw(result)
      }

      val input = ("* " * 32).trim

      // IDA* doesn't work cos I don't know how to define an "acceptable" goal.
      // Where as with GA you can just define perfection, let it run for a while and take its best result.

      "genetic" - println(run(input)(SmartSeqSplitter.genetic[String](_ => Config(128, 64, 100, 0.05, true))))

      "ovalWHRatio" - {
        def test(str: String, expected: Double)(implicit l: Line): Unit = {
          val data = ArraySeq.unsafeWrapArray(str.split('\n').map(line => ArraySeq.unsafeWrapArray(line.split(' '))))
//          val debugger = new SmartSeqSplitter.OvalDebugger.ToSvg
          val ref = new SmartSeqSplitter.WHRef
          SmartSeqSplitter.ovalWH(data, getLineWidth, lineHeight, ref)
//          println("\n\n" + debugger.result() + "\n\n")
//          japgolly.microlibs.utils.FileUtils.write("/home/golly/BeardedLogic/projects/shipreq/Code/tmp.svg", debugger.result())
          assertEqWithTolerance(ref.whRatio(), expected, 0.0000001)
        }
        "a" - test("a", 1.0)
        "abcd" - test("abcd", 4.0)
        "a|b|c|d" - test("a\nb\nc\nd", .25)
      }
    }
  }
}
