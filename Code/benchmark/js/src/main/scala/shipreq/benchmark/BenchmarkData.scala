package shipreq.benchmark

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.react._
import java.nio.ByteBuffer
import scala.collection.immutable.BitSet
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.sampledata.SampleData

final case class BenchmarkData(data: Vector[SampleData]) {

  val guiParam: GuiParam[SampleData, BitSet] =
    GuiParam.`enum`[SampleData](
      header        = "Sample data",
      values        = data: _*)(
      resultTxt     = _.name)
}

object BenchmarkData {

  def load: AsyncCallback[BenchmarkData] =
    for {
      data <- AsyncCallback.sequence(SampleData.all)
    } yield
      BenchmarkData(data)

  val verifiedEvents       = Benchmark.setup[SampleData, VerifiedEvent.Seq](_.verifiedEvents)
  val verifiedEventsBinary = Benchmark.setup[SampleData, ByteBuffer       ](_.verifiedEventsBinary.toNewByteBuffer)
  val project              = Benchmark.setup[SampleData, Project          ](_.project)
  val projectBinary        = Benchmark.setup[SampleData, ByteBuffer       ](_.projectBinary.toNewByteBuffer)
}