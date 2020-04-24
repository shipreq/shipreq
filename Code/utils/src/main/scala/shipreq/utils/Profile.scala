package shipreq.utils

import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation._
import shipreq.webapp.sampledata.SampleData
import shipreq.base.util.FxModule._

object Profile {

  def main(args: Array[String]): Unit = {
    println("Loading sample data...")

    val project = SampleData.`10000`.project
    profile(1)(Array.fill(100)(AtomScan(project)))

//    import shipreq.webapp.base.event._
//    val trusted = ApplyEvent.trusted
//    val verifiedEvents = SampleData.`10000`.verifiedEvents
//    profile(1)(trusted.applyVerified(verifiedEvents)(Project.empty))
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
    CC.clear()
    waitForInput()
    for (i <- 1 to times) {
      val dur = run.unsafeRun()
      println(s"Run #$i - ${dur.conciseDesc}")
    }
    CC.printReport()
    println("Done.")
  }

}
