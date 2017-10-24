package shipreq.webapp.base.data

import japgolly.microlibs.scalaz_ext.ScalazMacros
import monocle.{Lens, Optional}
import monocle.macros.Lenses
import monocle.std.option.pSome
import scalaz.{-\/, Equal, \/, \/-}
import scalaz.std.anyVal.intInstance
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.text.{Atom, Text}
import shipreq.webapp.base.util.ShowSize
import DataImplicits._

object Project {
  type Name = String

  val customIssueTypes    : Lens[Project, CustomIssueTypeIMap] = config ^|-> ProjectConfig.customIssueTypes
  val reqTypes            : Lens[Project, ReqTypes           ] = config ^|-> ProjectConfig.reqTypes
  val fields              : Lens[Project, FieldSet           ] = config ^|-> ProjectConfig.fields
  val tags                : Lens[Project, TagTree            ] = config ^|-> ProjectConfig.tags
  val genericReqs         : Lens[Project, GenericReqIMap     ] = reqs ^|-> Requirements.genericReqs
  val useCases            : Lens[Project, UseCases           ] = reqs ^|-> Requirements.useCases
  val pubidRegister       : Lens[Project, PubidRegister      ] = reqs ^|-> Requirements.pubids
  val reqCodeTrie         : Lens[Project, ReqCode.Trie       ] = reqCodes ^|-> ReqCodes.trie
  val implicationsSrcToTgt: Lens[Project, Implications.UniDir] = implications ^<-> Implications.biToUni
  val useCaseIMap         : Lens[Project, UseCaseIMap        ] = useCases ^|-> UseCases.imap
  val useCaseStepIndex    : Lens[Project, UseCases.StepIndex ] = useCases ^|-> UseCases.stepIndex

  val reqtableViewsNE: Optional[Project, reqtable.SavedViews.NonEmpty] =
    reqtableViews ^<-? pSome

  def reqtableView(id: reqtable.SavedView.Id): Optional[Project, reqtable.SavedView] =
    reqtableViewsNE ^|-? reqtable.SavedViews.NonEmpty.at(id)

  import ReqData._ // for equality
  implicit lazy val equality: Equal[Project] = ScalazMacros.deriveEqual

  // Not allowed by validator.
  // This ensures that initial ProjectNameSet events (generated on project creation) apply instead of being discarded
  // as NO-OPs due to the hashcodes being unchanged before and after.
  final val emptyProjectName: Name =
    ""

  val empty: Project =
    Project(
      emptyProjectName,
      ProjectConfig.empty,
      Requirements.empty,
      ReqCodes.empty,
      ReqData.emptyText,
      ReqData.emptyTags,
      Implications.emptyBiDir,
      DeletionReasons.empty,
      reqtable.SavedViews.empty,
      IdCeilings.zero)
}

@Lenses
final case class Project(name           : Project.Name,
                         config         : ProjectConfig,
                         reqs           : Requirements,
                         reqCodes       : ReqCodes,
                         reqText        : ReqData.Text,
                         reqTags        : ReqData.Tags,
                         implications   : Implications.BiDir,
                         deletionReasons: DeletionReasons,
                         reqtableViews  : reqtable.SavedViews.Optional,
                         idCeilings     : IdCeilings) {

  override def toString =
    s"Project($idCeilings)"
    //ShowSize(this).showTree

  lazy val deadReqIds: Set[ReqId] =
    reqs.reqIterator.filter(_.live(config.reqTypes) is Dead).map(_.id).toSet

  lazy val deadReqCount: Int =
    deadReqIds.size

  lazy val reqTypeCount: LiveDeadStatMap[ReqTypeId, Int] = {
    val b = new LiveDeadStatMap.Builder[ReqTypeId, Int]
    for (r <- reqs.reqIterator) {
      val live = r.live(config.reqTypes)
      b(r.reqTypeId).mod(live)(_ + 1)
    }
    b.result()
  }

  /** Dead or alive */
  def allRichText: List[(String, Iterator[Text.AnyOptional])] =
    ("Deletion reasons",  deletionReasons.reasons.iterator.map(_.whole))                 ::
    ("CodeGroups",        reqCodes.groups.iterator.map(_.title))                         ::
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
    implications.transitiveClosure(dir, reqs.idIterator, TransitiveClosure.Filter terminalSet deadReqIds)

  def reqtableViewIterator: Iterator[reqtable.SavedView] =
    reqtableViews.fold[Iterator[reqtable.SavedView]](Iterator.empty)(_.iterator)
}
