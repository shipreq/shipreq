package shipreq.webapp.base.data

import monocle.Lens
import monocle.macros.Lenses
import scalaz.{-\/, Equal, \/, \/-}
import scalaz.std.anyVal.intInstance
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.webapp.base.text.{Atom, Text}
import shipreq.webapp.base.util.ShowSize
import DataImplicits._

object Project {
  val customIssueTypes    : Lens[Project, CustomIssueTypeIMap] = config ^|-> ProjectConfig.customIssueTypes
  val reqTypes            : Lens[Project, ReqTypes           ] = config ^|-> ProjectConfig.reqTypes
  val fields              : Lens[Project, FieldSet           ] = config ^|-> ProjectConfig.fields
  val tags                : Lens[Project, TagTree            ] = config ^|-> ProjectConfig.tags
  val genericReqs         : Lens[Project, GenericReqIMap     ] = reqs ^|-> Requirements.genericReqs
  val useCases            : Lens[Project, UseCases           ] = reqs ^|-> Requirements.useCases
  val useCaseIMap         : Lens[Project, UseCaseIMap        ] = useCases ^|-> UseCases.imap
  val useCaseStepIndex    : Lens[Project, UseCases.StepIndex ] = useCases ^|-> UseCases.stepIndex
  val pubidRegister       : Lens[Project, PubidRegister      ] = reqs ^|-> Requirements.pubids
  val implicationsSrcToTgt: Lens[Project, Implications.UniDir] = implications ^<-> Implications.biToUni
  val reqCodeTrie         : Lens[Project, ReqCode.Trie       ] = reqCodes ^|-> ReqCodes.trie

  import ReqData._ // for equality
  implicit lazy val equality: Equal[Project] = UtilMacros.deriveEqual

  val empty: Project =
    Project(
      ProjectConfig.empty,
      Requirements.empty,
      ReqCodes.empty,
      ReqData.emptyText,
      ReqData.emptyTags,
      Implications.emptyBiDir,
      DeletionReasons.empty,
      IdCeilings.zero)
}

sealed abstract class PubidQueryError
object PubidQueryError {
  case object InvalidReqType extends PubidQueryError
  case class InvalidPos(reqType: ReqType, maxLegalPos: Int) extends PubidQueryError
}

@Lenses
final case class Project(config         : ProjectConfig,
                         reqs           : Requirements,
                         reqCodes       : ReqCodes,
                         reqText        : ReqData.Text,
                         reqTags        : ReqData.Tags,
                         implications   : Implications.BiDir,
                         deletionReasons: DeletionReasons,
                         idCeilings     : IdCeilings) {

  override def toString =
    s"Project($idCeilings)"
    //ShowSize(this).showTree

  lazy val deadReqIds: Set[ReqId] =
    reqs.reqs.filterV(_.live(config.reqTypes) :: Dead).keySet

  lazy val deadReqCount: Int =
    deadReqIds.size

  lazy val reqTypeCount: LDStats[ReqTypeId, Int] = {
    val b = new LDStats.Builder[ReqTypeId, Int]
    for (r <- reqs.reqs.values) {
      val live = r.live(config.reqTypes)
      b(r.reqTypeId).mod(live)(_ + 1)
    }
    b.result()
  }

  /** Dead or alive */
  def allRichText: List[(String, Iterator[Text.AnyOptional])] =
    ("Deletion reasons",  deletionReasons.reasons.iterator.map(_.whole))                 ::
    ("ReqCodeGroups",     reqCodes.groups.iterator.map(_.title))                         ::
    ("GenericReq titles", reqs.genericReqs.valuesIterator.map(_.title))                  ::
    ("UseCase titles",    reqs.useCases.imap.valuesIterator.map(_.title))                ::
    ("UseCase steps",     reqs.useCases.stepIterator.map(_.titleExplicitly))             ::
    ("Text fields",       reqText.valuesIterator.flatMap(_.valuesIterator).map(_.whole)) ::
    Nil

  def countAtoms: ShowSize.Node = {
    val counted =
      allRichText.map {
        case (name, txts) =>
          ShowSize.Node.countChildren(name, txts.toStream.flatMap(_.toStream))(Atom.Type.of(_).toString)
      }
    ShowSize.Node.sum("Atoms", counted: _*)
  }

  lazy val atomScan = AtomScan(this)

  /**
   * Transitive closure of implications going source → target.
   *
   * Note: Dead reqs are included (reflexively and when direct implications) but are not followed.
   */
  lazy val implicationSrcToTgtTC: TransitiveClosure[ReqId] =
    implicationTransitiveClosure(Forwards)

  /**
   * Transitive closure of implications going target → source.
   *
   * Note: Dead reqs are included (reflexively and when direct implications) but are not followed.
   */
  lazy val implicationTgtToSrcTC: TransitiveClosure[ReqId] =
    implicationTransitiveClosure(Backwards)

  private def implicationTransitiveClosure(dir: Direction): TransitiveClosure[ReqId] =
    implications.transitiveClosure(dir, reqs.reqs.keys, TransitiveClosure.Filter terminalSet deadReqIds)

  def findReq(externalPubid: ExternalPubid): PubidQueryError \/ Req =
    findReq(externalPubid.mnemonic, externalPubid.pos)

  def findReq(mnemonic: ReqType.Mnemonic, pos: ReqTypePos): PubidQueryError \/ Req =
    config.reqTypes.allByMnemonic.get(mnemonic) match {
      case None =>
        -\/(PubidQueryError.InvalidReqType)
      case Some(rt) =>
        val i = pos.value - 1
        val register = reqs.pubids.value(rt.reqTypeId)
        if (register.isIndexValid(i))
          \/-(reqs req register(i))
        else
          -\/(PubidQueryError.InvalidPos(rt, register.length))
    }
}
