package shipreq.utils

import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.webapp.base.data._
import shipreq.webapp.sampledata.SampleData
import shipreq.base.util.FxModule._

object Profile {

  def main(args: Array[String]): Unit = {
    val sd = loadSampleData()

//    import shipreq.webapp.base.data.derivation._
//    val project = sd.project
//    profile(1)(Array.fill(100)(AtomScan(project)))

//    val trie = sd.project.content.reqCodes.trie
//    profile(2)(Array.fill(100)(ReqCodes.benchmarkScan(trie)))

    import shipreq.webapp.base.event._
    val trusted = ApplyEvent.trusted
    val verifiedEvents = sd.verifiedEvents
    profile(1)(trusted.applyVerified(verifiedEvents)(Project.empty))
  }

  private def loadSampleData(): SampleData = {
    println("Loading sample data...")
    val sd = SampleData.noReqCodes.`10000`
    sd.assertValid()
    sd
  }

  private def waitForInput(): Unit = {
    println(".")
    println("Press enter to start...")
    scala.io.StdIn.readLine()
    println("Starting...")
    println(".")
  }

  private def profile(times: Int)(body: => Any): Unit = {
    val run = Fx(body).measureDuration_
    waitForInput()
    for (i <- 1 to times) {
      val dur = run.unsafeRun()
      println(s"Run #$i - ${dur.conciseDesc}")
    }
    println("Done.")
  }
}
