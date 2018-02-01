package shipreq.webapp.gen

object Main {
  def main(args: Array[String]): Unit = {
    Manifest.All.foreach(_.gen.printFileContent())
    println()
  }
}
