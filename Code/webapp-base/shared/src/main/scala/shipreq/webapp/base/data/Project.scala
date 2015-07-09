package shipreq.webapp.base.data

import monocle.Lens
import monocle.macros.Lenses
import scalaz.Equal
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.text.{Atom, Text}
import shipreq.webapp.base.util.{TransitiveClosure, ShowSize}
import shipreq.webapp.base.util.TypeclassDerivation._
import DataImplicits._

object Project {
  val customIssueTypes: Lens[Project, RevAnd[CustomIssueTypeIMap]] = config ^|-> ProjectConfig.customIssueTypes
  val customReqTypes  : Lens[Project, RevAnd[CustomReqTypeIMap]  ] = config ^|-> ProjectConfig.customReqTypes
  val fields          : Lens[Project, RevAnd[FieldSet]           ] = config ^|-> ProjectConfig.fields
  val tags            : Lens[Project, RevAnd[TagTree]            ] = config ^|-> ProjectConfig.tags

  import ReqData._ // for equality
  implicit def equality: Equal[Project] = deriveEqual
}

@Lenses
final case class Project(config      : ProjectConfig,
                         reqs        : RevAnd[Requirements],
                         reqCodes    : RevAnd[ReqCodes],
                         reqText     : RevAnd[ReqData.Text],
                         reqTags     : RevAnd[ReqData.Tags],
                         implications: RevAnd[Implications]) {


  def contentRev: Rev =
    reqs        .rev +
    reqCodes    .rev +
    reqText     .rev +
    reqTags     .rev +
    implications.rev

  val rev: Rev =
    config.rev + contentRev

  override def toString =
    s"Project(config: ${config.rev}, content: $contentRev)"
    //ShowSize(this).showTree

  def allRichText: Stream[(String, Stream[Text.AnyOptional])] =
    Stream(
      ("ReqCodeGroups", reqCodes.data.activeGroups.map(_.group.title)),
      ("GenericReq titles", reqs.data.reqs.values.filterT[GenericReq].map(_.title)),
      ("Text fields", reqText.data.values.toStream.flatMap(_.values.toStream).map(_.whole)))

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
      reqs.data.reqs.keys,
      reqs.data.dead,
      f(implications.data))

  // Finally, ensure validity
//  import japgolly.nyaya._
//  this assertSatisfies DataProp.project.all
//  TODO Delete ↑ once confirmed that Project tightly confirmed in events etc
}

