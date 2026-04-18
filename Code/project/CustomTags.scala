import sbt._

object CustomTags {
  val MemoryMB = Tags.Tag("MemoryMB")
  val Node     = Tags.Tag("Node")

  val WebappClientProjectTest = Tags.Tag("WebappClientProjectTest")
}
