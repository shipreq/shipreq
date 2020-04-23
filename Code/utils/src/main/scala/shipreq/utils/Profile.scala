package shipreq.utils

import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.ApplyEvent
import shipreq.webapp.sampledata.SampleData
import shipreq.base.util.FxModule._

object Profile {

  def main(args: Array[String]): Unit = {
    println("Loading sample data...")
    val sd = SampleData.`10000`
    val verifiedEvents = sd.verifiedEvents

    val run = Fx(ApplyEvent.trusted.applyVerified(verifiedEvents)(Project.empty)).measureDuration_

    waitForInput()

    for (i <- 1 to 1) {
      val dur = run.unsafeRun()
      println(s"Run #$i - ${dur.conciseDesc}")
    }

    println("Done.")
  }

  private def waitForInput(): Unit = {
    println(".")
    println("Press enter to start...")
    scala.io.StdIn.readLine()
    println("Starting...")
    println(".")
  }
}
