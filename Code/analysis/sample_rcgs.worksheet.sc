import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.io.{Codec, Source}

def writeFile(filename: String, content: String): Unit = {
  Files.write(Paths.get(filename), content.getBytes(StandardCharsets.UTF_8))
}

val root = "analysis/sample_rcgs"
val lv = Source.fromFile(s"$root-tree.txt").getLines.toVector
val ls = lv.toArray

var empties = 0
var maxLevels = 0

class PerLevel {
  val uniq = collection.mutable.Set.empty[String]
  var total = 0
  def add(s: String) = {
    total += 1
    uniq += s
  }
}

var countsPerLevel = Vector.fill(20)(new PerLevel)

for (i <- ls.indices) {
  var l = ls(i)

  if (l.trim.startsWith(".")) {
    val indent = l.takeWhile(_ == ' ').length >> 1

    ls(i) =
      if (indent == 0) {
        val prev = lv.take(i).reverseIterator.filter(_.headOption.exists(c => c != '.' && c != ' ')).next()
        prev + l
      } else {
        val needExtraIndent = lv.take(i).reverseIterator.filterNot(_ startsWith " ").next().startsWith(".")
        val prev = ls.take(i).reverseIterator.filter(_.nonEmpty).next()
        val ps = prev.split('.')
        val pi = indent + (if (needExtraIndent) 1 else 0)
        ps.take(pi).mkString(".") + l.trim
      }

    l = ls(i)
  }

  if (l.trim.isEmpty)
    empties += 1
  else {
    val parts = l.split('.')
    maxLevels = maxLevels.max(parts.length)
    for ((p, j) <- parts.zipWithIndex) {
      countsPerLevel(j).add(p)
    }
  }
}

countsPerLevel = countsPerLevel.take(maxLevels)

val perLevel =
  countsPerLevel.zipWithIndex.map { case (x, lvl) =>
    s"Level ${lvl+1}: ${x.total} total, ${x.uniq.size} unique"
  }.mkString("\n")

val all =
s"""|Reqs        : ${lv.length}
    |Empties     : $empties   (${empties.toDouble / lv.length * 100}%)
    |With Codes  : ${lv.length - empties} (${100 - 100 * (empties.toDouble / lv.length)}%)
    |All levels  : ${countsPerLevel.map(_.total).sum} total, ${countsPerLevel.map(_.uniq.size).sum} unique
    |
    |$perLevel
    |""".stripMargin

writeFile(s"$root-list.txt", ls.mkString("\n"))
writeFile(s"$root-summary.txt", all)
