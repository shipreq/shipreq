package shipreq.utils

import japgolly.nyaya.test._
import scala.annotation.tailrec
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.util.ShowSize
import shipreq.webapp.base.{RandomData => $}
import shipreq.utils.lib._
import DataImplicits._
import ShowSrcDataImp._

object GenerateProject {

  object Size10 {
    val CfgTags              = 10
    val CfgCustomIssueTypes  =  2
    val CfgCustomReqTypes    =  8
    val CfgFields            = 10
    val Reqs                 = 10
    val ReqCodeDepth         =  2
    val ReqCodeSize          =  2
    val Tags                 = 10
    val ImplicationsPerSrc   =  4
    val Implications         = 10
  }

  object Size100 {
    val CfgTags              =  30
    val CfgCustomIssueTypes  =  10
    val CfgCustomReqTypes    =  12
    val CfgFields            =  25
    val Reqs                 = 100
    val ReqCodeDepth         =   6
    val ReqCodeSize          =   5
    val Tags                 = 200
    val ImplicationsPerSrc   =   8
    val Implications         = 100
  }

  object Size1000 {
    val CfgTags              =  120
    val CfgCustomIssueTypes  =   20
    val CfgCustomReqTypes    =   24
    val CfgFields            =   30
    val Reqs                 = 1000
    val ReqCodeDepth         =   10
    val ReqCodeSize          =    6
    val Tags                 = 1600
    val ImplicationsPerSrc   =   12
    val Implications         = 1000
  }

  val Size = Size1000

  def main(args: Array[String]): Unit = {
//    autoSelect = true

    val tags0           = sample($.tagTree, Size.CfgTags)
    val issues0         = firstSample($.revAndIMap($.customIssueType list Size.CfgCustomIssueTypes), 50)
    val (issues, tags)  = $.distinctHashRefKeys.run((issues0, tags0))
    val reqtypes        = firstSample($.genCustomReqTypes($.customReqType list Size.CfgCustomReqTypes), 50)
    val reqTypeIds      = StaticReqType.values ++ reqtypes.keys
    val reqTypeIdSet    = reqTypeIds.whole.toSet
    val fields          = sample($.fieldSet2(reqTypeIdSet, tags.keySet, reqtypes.keySet), Size.CfgFields)
    val cfg             = ProjectConfig(issues, reqtypes, fields, tags)
    val atagIds         = cfg.tags.vstream(_.tag).filterT[ApplicableTag].map(_.id).toSet
    val reqsWithoutText = firstSample($.reqsWithoutText(Size.Reqs, cfg), 0)
    val reqIds          = reqsWithoutText.reqs.keys
    val reqIdG          = Gen tryGenChoose reqIds.toIndexedSeq
    val reqIdSet        = reqIds.toSet
    val liveReqIds      = reqsWithoutText.reqs.values.toStream.filter(_.live(cfg.customReqTypes) :: Live).map(_.id)
    val liveReqIdG      = Gen tryGenChoose liveReqIds
    val reqCodeDataG    = $.reqCode.data(liveReqIdG, reqIdG, $.reqCode.gEmptyReqCodeGroup)
    val reqCodesG       = $.reqCodes($.reqCode.trie(Size.ReqCodeDepth, reqCodeDataG))
    val reqCodes        = sample(reqCodesG, Size.ReqCodeSize)
    val reqTags         = sample($.reqFieldDataTags(reqIdSet, atagIds), Size.Tags)
    val impMethod       = $.implicationsMethod2(Size.ImplicationsPerSrc, Size.Implications)
    val reqImps         = sample($.reqFieldDataImplications(reqIdSet, impMethod), Size.Implications)
    val p               = sample($.genProject(cfg, reqsWithoutText, reqCodes, reqTags, reqImps), Size.Reqs)

    val objName = s"Project_${Size.Reqs}"
    val code    = ShowSrc.generateObject("shipreq.benchmark", objName, "project")(p)
    val fout    = s"/tmp/$objName.scala"

    println()
    println(s"Writing ${String.format("%,d", java.lang.Integer valueOf code.length)} bytes to $fout ...")
    writeFile(fout, code)
    println("Done.")
  }

  // ===================================================================================================================

  def firstSample[A](gen: Gen[A], size: Int): A =
    gen.samples(GenCtx(GenSize(size)), 1).next()

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
