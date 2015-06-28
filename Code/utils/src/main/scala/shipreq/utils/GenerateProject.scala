package shipreq.utils

import japgolly.nyaya.test._
import scala.annotation.tailrec
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.util.ShowSize
import shipreq.webapp.base.RandomData
import shipreq.utils.lib._
import ShowSrcDataImp._

object GenerateProject {

  object Size10 {
    val CfgTags              = 10
    val CfgCustomIssueTypes  = 10
    val CfgCustomReqTypes    = 10
    val CfgFields            = 10
    val Reqs                 = 10
    val ReqCodeDepth         = 2
    val ReqCodeSize          = 2
    val Tags                 = Reqs
    val Implications         = Reqs
  }

  object Size100 {
    val CfgTags              = 30
    val CfgCustomIssueTypes  = 15
    val CfgCustomReqTypes    = 30
    val CfgFields            = 25
    val Reqs                 = 100
    val ReqCodeDepth         = 6
    val ReqCodeSize          = 5
    val Tags                 = Reqs * 2
    val Implications         = Reqs * 2
  }

  object Size1000 {
    val CfgTags              = 100
    val CfgCustomIssueTypes  =  20
    val CfgCustomReqTypes    =  50
    val CfgFields            =  30
    val Reqs                 = 1000
    val ReqCodeDepth         = 8
    val ReqCodeSize          = 6
    val Tags                 = Reqs * 2
    val Implications         = Reqs * 2
  }

  val Size = Size100

  def main(args: Array[String]): Unit = {
//    autoSelect = true

    val tags0           = sample(RandomData.revAndTagTree, Size.CfgTags)
    val issues0         = sample(RandomData.customIssueTypes, Size.CfgCustomIssueTypes)
    val (issues, tags)  = RandomData.distinctHashRefKeys.run((issues0, tags0))
    val reqtypes        = sample(RandomData.customReqTypes, Size.CfgCustomReqTypes)
    val reqTypeIds      = StaticReqType.values ++ reqtypes.data.keys
    val reqTypeIdSet    = reqTypeIds.whole.toSet
    val fields1         = sample(RandomData.fieldSet2(reqTypeIdSet, tags.data.keySet, reqtypes.data.keySet), Size.CfgFields)
    val fields          = firstSample(RandomData.revAnd(fields1), 500)
    val cfg             = ProjectConfig(issues, reqtypes, fields, tags)
    val atagIds         = cfg.tags.data.vstream(_.tag).filterT[ApplicableTag].map(_.id).toSet
    val reqsWithoutText = firstSample(RandomData.reqsWithoutText(Size.Reqs, cfg), 0)
    val reqIds          = reqsWithoutText.reqs.keys
    val reqIdG          = Gen oneofO reqIds.toSeq
    val reqIdSet        = reqIds.toSet
    val liveReqIds      = reqsWithoutText.reqs.values.toStream.filter(_.live :: Live).map(_.id)
    val liveReqIdG      = Gen oneofO liveReqIds
    val reqCodeDataG    = RandomData.reqCode.data(liveReqIdG, reqIdG, RandomData.reqCode.gEmptyReqCodeGroup)
    val reqCodesG       = RandomData.reqCodes(RandomData.reqCode.trie(Size.ReqCodeDepth, reqCodeDataG.sup))
    val reqCodes        = sample(reqCodesG, Size.ReqCodeSize)
    val reqTags         = sample(RandomData.reqFieldDataTags(reqIdSet, atagIds), Size.Tags)
    val reqImps         = sample(RandomData.reqFieldDataImplications(reqIdSet), Size.Implications)
    val p               = sample(RandomData.genProject(cfg, reqsWithoutText, reqCodes, reqTags, reqImps), Size.Reqs)

    val objName = s"Project_${Size.Reqs}"
    val code    = ShowSrc.generateObject("shipreq.benchmark.data", objName, "project")(p)
    val fout    = s"/tmp/$objName.scala"

    println()
    println(s"Writing ${String.format("%,d", java.lang.Integer valueOf code.length)} bytes to $fout ...")
    writeFile(fout, code)
    println("Done.")
  }

  // ===================================================================================================================

  def firstSample[A](gen: Gen[A], size: Int): A =
    gen.f(GenSize(size)).run.unsafePerformIO()

  var autoSelect         = false
  var lastPromptResponse = '?'

  def sample[A: ShowSize](gen: Gen[A], size: Int): A = {
    println("_" * 100)
    @tailrec
    def go(): A = {
      val a = firstSample(gen, size)
      val sz = ShowSize(a).showTree
      println()
      println(sz)
      @tailrec def prompt(): Option[A] = {
        println("This ok?")
        System.out.flush()
        val ch = Option(io.StdIn.readLine()).flatMap(_.trim.toLowerCase.headOption).getOrElse(lastPromptResponse)
        lastPromptResponse = ch
        ch match {
          case 'y' => Some(a)
          case 'n' => None
          case 'q' => sys.error("Bye!")
          case _   => prompt()
        }
      }
      if (autoSelect) {
        println()
        a
      } else
        prompt() match {
          case Some(a) => a
          case None    => go()
        }
    }
    go()
  }

  def time[R](name: String, f: => R): R = {
    print(s"Starting $name... ")
    val start = System.nanoTime()
    val r = f
    val end = System.nanoTime()
    val time = end - start // 10⁹
    val sec = 1000000000
    val t = time.toDouble / sec.toDouble
    printf("Time: %,f sec\n", t)
    System.out.flush()
    r
  }

  def writeFile(filename: String, content: String): Unit = {
    import java.nio.file.{Paths, Files}
    import java.nio.charset.StandardCharsets
    Files.write(Paths get filename, content getBytes StandardCharsets.UTF_8)
  }
}
