package shipreq.webapp.client.project.app.cfg.reqtypes

import japgolly.microlibs.stdlib_ext.ParseInt
import japgolly.scalajs.react.test.Simulation
import org.scalajs.dom.html
import shipreq.webapp.base.test.TestState._

object CfgReqTypesObs {

  final case class Row(row: DomZipperJs) {

    val mnemonic: String = {
      val mnemonicCell = row(">td:first")
      mnemonicCell.collect01("input").domsAs[html.Input] match {
        case Some(i) => i.value
        case None    => mnemonicCell.innerText
      }
    }

    private val usageCell = row(">td:nth-child(4)")

    val usageNum: Int =
      usageCell.innerText match {
        case ParseInt(i) => i
      }

    val usageLink: Option[html.Anchor] =
      usageCell.collect01("a").domsAs[html.Anchor]
  }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final class CfgReqTypesObs($: DomZipperJs) {
  import CfgReqTypesObs._

  val table: DomZipperJs =
    $("table:contains(Usage)")

  val rowByMnemonic: Map[String, Row] =
    table
      .collect1n(">tbody>tr")
      .zippers
      .iterator
      .map(Row)
      .map(r => (r.mnemonic, r))
      .toMap

}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object CfgReqTypesDsl {

  val dsl = Dsl[Unit, CfgReqTypesObs, Unit]

  def clickUsageLink(mnemonic: String): dsl.Actions =
    dsl.action(s"Click $mnemonic usage")(Simulation.click run _.obs.rowByMnemonic(mnemonic).usageLink.get)
}
