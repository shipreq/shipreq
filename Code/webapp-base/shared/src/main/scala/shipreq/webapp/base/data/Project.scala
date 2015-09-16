package shipreq.webapp.base.data

import monocle.Lens
import monocle.macros.Lenses
import scalaz.Equal
import shipreq.base.util.ScalaExt._
import shipreq.base.util.UtilMacros
import shipreq.webapp.base.text.{Atom, Text}
import shipreq.webapp.base.util.{TransitiveClosure, ShowSize}
import DataImplicits._

object Project {
  val customIssueTypes: Lens[Project, CustomIssueTypeIMap] = config ^|-> ProjectConfig.customIssueTypes
  val customReqTypes  : Lens[Project, CustomReqTypeIMap  ] = config ^|-> ProjectConfig.customReqTypes
  val fields          : Lens[Project, FieldSet           ] = config ^|-> ProjectConfig.fields
  val tags            : Lens[Project, TagTree            ] = config ^|-> ProjectConfig.tags

  import ReqData._ // for equality
  implicit lazy val equality: Equal[Project] = UtilMacros.deriveEqual

  val empty: Project = {
    val cfg      = ProjectConfig.empty
    val reqs     = Requirements.empty
    val reqCodes = ReqCodes.empty
    val reqText  = ReqData.emptyText
    val reqTags  = ReqData.emptyTags
    val reqImps  = Implications.empty
    val ids      = IdCeilings.zero
    Project(cfg, reqs, reqCodes, reqText, reqTags, reqImps, ids)
  }
}

@Lenses
final case class Project(config      : ProjectConfig,
                         reqs        : Requirements,
                         reqCodes    : ReqCodes,
                         reqText     : ReqData.Text,
                         reqTags     : ReqData.Tags,
                         implications: Implications,
                         idCeilings  : IdCeilings) {

  override def toString =
    s"Project($idCeilings)"
    //ShowSize(this).showTree

  lazy val deadReqIds: Set[ReqId] =
    reqs.reqs.filterV(_.live(config.customReqTypes) :: Dead).keySet

  lazy val deadReqCount: Int =
    deadReqIds.size

  def allRichText: Stream[(String, Stream[Text.AnyOptional])] =
    Stream(
      ("ReqCodeGroups", reqCodes.activeGroups.map(_.group.title)),
      ("GenericReq titles", reqs.reqs.values.filterT[GenericReq].map(_.title)),
      ("Text fields", reqText.values.toStream.flatMap(_.values.toStream).map(_.whole)))

  def countAtoms: ShowSize.Node = {
    val counted =
      allRichText.map {
        case (name, txts) =>
          ShowSize.Node.countChildren(name, txts.flatMap(_.toStream))(Atom.Type.of(_).toString)
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
    implicationTransitiveClosure(_.srcToTgt)

  /**
   * Transitive closure of implications going target → source.
   *
   * Note: Dead reqs are included (reflexively and when direct implications) but are not followed.
   */
  lazy val implicationTgtToSrcTC: TransitiveClosure[ReqId] =
    implicationTransitiveClosure(_.tgtToSrc)

  private def implicationTransitiveClosure(f: Implications => Implications.Uni): TransitiveClosure[ReqId] =
    Implications.transitiveClosure(
      reqs.reqs.keys,
      deadReqIds,
      f(implications))

  // Finally, ensure validity
//  import japgolly.nyaya._
//  this assertSatisfies DataProp.project.all
//  TODO Delete ↑ once confirmed that Project tightly confirmed in events etc
}

