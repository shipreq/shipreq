package shipreq.webapp.sampledata

object VerifySampleData {

  def main(args: Array[String]): Unit = {
    import Console._

    val errors = SampleData.errors
    if (errors.isEmpty)
      println(s"$GREEN_B${BLACK}Sample data ok.$RESET")
    else {
      println(s"$RED_B${WHITE}Invalid sample data detected.$RESET")
      println(errors.map("  - " + _).mkString("\n"))
    }
  }
}
