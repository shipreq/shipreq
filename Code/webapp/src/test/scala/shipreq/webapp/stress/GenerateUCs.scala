package shipreq.webapp.stress

import shipreq.webapp.lib.ScalazSubset._
import shipreq.webapp.lib.Types._
import scala.annotation.tailrec
import shipreq.webapp.test.DataGenerators._
import shipreq.webapp.test.TestDB
import shipreq.webapp.app.Defaults
import shipreq.webapp.feature.uc.UseCase
import org.apache.commons.io.FileUtils
import java.io.File
import collection.parallel.immutable.ParSeq
import org.scalacheck.Gen

object GenerateUCs /*extends App */{

  // -------------------------------------------------------------------------------------------------------------------

  lazy val fieldList = {
    TestDB.init()
    Defaults.fieldList.value
  }

  def generate(count: Int, ucn: UseCaseNumber): ParSeq[UseCase] = {
    val prms = Gen.Parameters.default
    val gen = useCaseGen(fieldList, ucn)
    (1 to count).toList.par.map(_ => nextUseCase(gen, prms))
  }

  def generate(count: Int, ucn: UseCaseNumber, seed: Long): List[UseCase] = {
    val prms = new Gen.Parameters.Default{ override val rng = new scala.util.Random(seed) }
    val gen = useCaseGen(fieldList, ucn)
    (1 to count).toList.map(_ => nextUseCase(gen, prms))
  }

  @tailrec
  def nextUseCase(gen: Gen[UseCase], prms: Gen.Parameters, tries: Int = 20): UseCase =
    gen(prms) match {
      case Some(uc)           => uc
      case None if tries == 0 => throw new RuntimeException("Failed to generate UC.")
      case None if tries > 0  => nextUseCase(gen, prms, tries - 1)
    }

  /*
  // -------------------------------------------------------------------------------------------------------------------

  // TODO This failed. Compilation crashes scalac and takes ages anyway

  val count = 100
  val ucn: UseCaseNumber = (1:Short).tag
  val outpath = "/tmp/Data.scala"

  import shipreq.webapp.feature.Inspection._

  def inspect(vars: Map[String, String])(uc: UseCase): String = {
    var x = uc.shows
    for ((name,body) <- vars) x = x.replace(body, name)
//    x.substring(0, 200)
    x
  }

  val vars: Map[String, String] = Map(
    "ucn" -> ucn.shows
  ) ++ Defaults.fieldList.value.fields.zipWithIndex.map{ case (f,i) => (s"f$i" -> f.shows) }

  val ucs = generate(count, ucn).map(inspect(vars))

  val output = {
    var lines = List.empty[String]
    for ((name,body) <- vars)
      lines ::= s"val $name = $body"
    lines = lines.sorted.reverse

    for ((uc, i) <- ucs.toList.zipWithIndex)
      lines = s"val uc$i = $uc" :: "" :: lines
    lines ::= ""
    lines ::= s"val ucs = List(${0.until(count).map(i => s"uc$i") mkString ","})"

    s"""
      |object Data {
      |  import scalaz.Name, shipreq.webapp._, db._, lib.Types._, feature.uc, uc._, uc.field._, uc.step._, uc.text._, FreeTextTerms._, util._
      |  ${lines.reverse.mkString("\n").replace("\n", "\n  ")}
      |}
    """.stripMargin
  }

  println(s"Writing to $outpath")
  FileUtils.writeStringToFile(new File(outpath), output)
  */
}
