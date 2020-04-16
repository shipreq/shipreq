package shipreq.benchmark

import java.nio.ByteBuffer
import java.io.ByteArrayOutputStream
import java.util.zip.GZIPOutputStream
import shipreq.base.util.BinaryData

object SizeCompare {

  case class NData(name: String, data: Data)
  implicit def dataFromNData(n: NData) = n.data
  implicit def dataFromBinaryData(n: BinaryData) = Data(n.unsafeArray)

  final case class Data(bytes: Array[Byte]) {

    val size = bytes.length

    val gzipSize: Long = {
      val bs = new ByteArrayOutputStream()
      val gs = new GZIPOutputStream(bs)
      gs.write(bytes)
      gs.finish()
      bs.flush()
      bs.toByteArray.length
    }

    val gzipReduction = gzipSize.toDouble / size.toDouble * 100
  }

  implicit def bbSize(bb: ByteBuffer) = Data(bb.array)
  implicit def strSize(s: String) = Data(s.getBytes("UTF-8"))

  def bench(name: String, d: Data) = {
    printf("| %-20s | %,10d bytes | %,10d bytes | %3.0f%% |\n", name, d.size, d.gzipSize, d.gzipReduction)
    NData(name, d)
  }

  def main(args: Array[String]): Unit = {

    val sep = "|----------------------+------------------+------------------|------|"
    println(sep)
    println("| Data                 |       Size       |     Zip Size     | Zip% |")
    println(sep)

//    implicit val projectJsonCodec = DataCodecs.project
//    val j100  = bench("json P100",  upickle.Fns write SampleData.project_100)
//    val j1000 = bench("json P1000", upickle.Fns write SampleData.project_1000)

    val b1000   = bench("bin P1000",  SampleData.`1000`.projectBinary)
    val b10000  = bench("bin P10000", SampleData.`10000`.projectBinary)

    println(sep)
    println()

//    compare(b100, j100)
//    compare(b1000, j1000)

    println()
  }

  def compare(a: NData, b: NData): Unit = {
    val (x,y) = (a.size, b.size)
    val sym = if (x < y) "<" else if (x > y) ">" else "="
    val d = Math.abs(x - y)
    val p1 = x.toDouble / y.toDouble * 100
    val p2 = y.toDouble / x.toDouble * 100
    //  (%3.0f%% : %3.0f%%)
    val dp = d.toDouble / y.toDouble * 100
//    printf("%-20s %s %-20s by %3.0f%% (%,9d bytes).\n", a.name, sym, b.name, dp, delta)
    printf("%-20s %s %-20s by %3.0f%%. | %,10d %s %,10d by %,10d.\n", a.name, sym, b.name, dp, x, sym, y, d)
  }
}
