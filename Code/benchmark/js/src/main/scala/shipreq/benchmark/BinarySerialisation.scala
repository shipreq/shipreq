package shipreq.benchmark

import boopickle.UnpickleImpl
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.react.vdom.html_<^._

object BinarySerialisation {

  type BM = Benchmark[SampleData]

  private implicit val picklerProject          = shipreq.webapp.base.protocol.binary.v1.Rev1.picklerProject
  private implicit val picklerVerifiedEventSeq = shipreq.webapp.base.protocol.binary.v1.Rev1.picklerVerifiedEventSeq

  sealed trait Method {
    val bm: BM
    def name = toString
  }

  object Method {

    case object ReadEvents extends Method {
      override val bm: BM =
        BenchmarkData.verifiedEventsBinary("Read events") { bb =>
          bb.rewind()
          UnpickleImpl(picklerVerifiedEventSeq) fromBytes bb
        }
    }

    case object ReadProject extends Method {
      override val bm: BM =
        BenchmarkData.projectBinary("Read project") { bb =>
          bb.rewind()
          UnpickleImpl(picklerProject) fromBytes bb
        }
    }

    val all      = AdtMacros.adtValues[Method]
    val guiParam = GuiParam.`enum`("Method", all.whole.sortBy(_.name): _*)(_.name)
  }
}

final case class BinarySerialisation(data: BenchmarkData) {
  val suite    = Suite("Binary Serialisation")(BinarySerialisation.Method.all.whole.map(_.bm): _*)
  val guiSuite = GuiSuite(suite, data.guiParam)
}
