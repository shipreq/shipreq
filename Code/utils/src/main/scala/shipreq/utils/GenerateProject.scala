package shipreq.utils

import boopickle.PickleImpl
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.nio.file.{Files, Paths}
import java.time.Instant
import nyaya.gen._
import scala.annotation.tailrec
import shipreq.base.test.BaseUtilGen._
import shipreq.base.util.BinaryData
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.binary.v1.Latest.picklerProject
import shipreq.webapp.base.util.ShowSize
import shipreq.webapp.base.{RandomData => $, RandomDataSettings}

object GenerateProject {

  val UseCasePercentage = 0.2

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

  val Size = Size100

  def main(args: Array[String]): Unit = {
    autoSelect = false

    RandomDataSettings.disableUnicode = true

    val useCaseCount    = (UseCasePercentage * Size.Reqs).toInt
    val genericReqCount = Size.Reqs - useCaseCount

    val reqtypes        = firstSample($.genCustomReqTypes($.customReqType list Size.CfgCustomReqTypes), 50)
    val reqTypeIds      = StaticReqType.values ++ reqtypes.keys
    val reqTypeIdSet    = reqTypeIds.whole.toSet
    val genReqTypeId    = Gen.chooseNE(reqTypeIds)
    val tags0           = sample($.tagTree(genReqTypeId.set(0 to 4)), Size.CfgTags)
    val issues0         = firstSample($.revAndIMap($.customIssueType list Size.CfgCustomIssueTypes), 50)
    val (issues, tags)  = $.distinctHashRefKeys.run((issues0, tags0))
    val fields          = sample($.fieldSet(reqTypeIdSet, tags.keySet), Size.CfgFields)
    val cfg             = ProjectConfig(issues, ReqTypes(reqtypes), fields, Tags(tags))
    val atagIds         = cfg.tags.tree.valuesIterator.map(_.tag).filterSubType[ApplicableTag].map(_.id).toSet
    val reqsWithoutText = firstSample($.reqsWithoutText(cfg, genericReqCount, useCaseCount), 20)
    val reqIdSet        = reqsWithoutText.idIterator().toSet
    val reqIdG          = Gen tryGenChoose reqIdSet.toIndexedSeq
    val liveReqIds      = reqsWithoutText.reqIterator().filter(_.live(cfg.reqTypes) is Live).map(_.id)
    val liveReqIdG      = Gen tryGenChoose liveReqIds.toIndexedSeq
    val reqCodeDataG    = $.reqCode.data(liveReqIdG, reqIdG)
    val reqCodesG       = $.reqCodes($.reqCode.trie(reqCodeDataG, Size.ReqCodeDepth))
    val reqCodes        = sample(reqCodesG, Size.ReqCodeSize)
    val reqTags         = sample($.reqFieldDataTags(reqIdSet, atagIds), Size.Tags)
    val impMethod       = $.implicationsMethod2(Size.ImplicationsPerSrc, Size.Implications)
    val reqImps         = sample($.reqFieldDataImplications(reqIdSet, impMethod), Size.Implications)
    val p               = sample($.genProject(cfg, reqsWithoutText, reqCodes, reqTags, reqImps), Size.Reqs)
    val filename        = s"/tmp/project-${Size.Reqs}-${Instant.now().toString.filter(_.isDigit)}.bin"
    val bin             = BinaryData.unsafeFromByteBuffer(PickleImpl intoBytes p)

    println()
    println(s"Writing ${"%,d" format bin.length} bytes to $filename ...")
    Files.write(Paths.get(filename), bin.unsafeArray)
    println("Done.")
  }

  // ===================================================================================================================

  def firstSample[A](gen: Gen[A], size: Int): A =
    gen.samples(GenSize(size)).next()

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
        val ch = Option(scala.io.StdIn.readLine()).flatMap(_.trim.toLowerCase.headOption).getOrElse(lastPromptResponse)
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

}
