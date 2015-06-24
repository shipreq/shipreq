package shipreq.webapp.base.data

import japgolly.nyaya.util.Multimap
import monocle.macros.Lenses
import scalaz.{Equal, -\/, \/-}
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{UnivEq, Monoidish, Must}
import shipreq.webapp.base.text.{Atom, Text}
import shipreq.webapp.base.util.{TransitiveClosure, ShowSize}
import shipreq.webapp.base.util.TypeclassDerivation._
import DataImplicits._
import ReqFieldData.{Implications, ImplicationsU}

@Lenses
final case class Project(customIssueTypes: RevAnd[CustomIssueTypeIMap],
                         customReqTypes  : RevAnd[CustomReqTypeIMap],
                         fields          : RevAnd[FieldSet],
                         tags            : RevAnd[TagTree],
                         reqs            : RevAnd[Requirements],
                         reqCodes        : RevAnd[ReqCodes],
                         reqFieldData    : RevAnd[ReqFieldData]) {

  def configRev: Rev =
    customIssueTypes.rev +
    customReqTypes  .rev +
    fields          .rev +
    tags            .rev

  def contentRev: Rev =
    reqs            .rev +
    reqCodes        .rev +
    reqFieldData    .rev

  val rev: Rev =
    configRev + contentRev

  override def toString =
    s"Project(config: $configRev, content: $contentRev)"
    //ShowSize(this).showTree

  def allRichText: Stream[(String, Stream[Text.AnyOptional])] =
    Stream(
      ("Generic Req descs", reqs.data.reqs.values.filterT[GenericReq].map(_.title)),
      ("Text fields", reqFieldData.data.text.values.toStream.flatMap(_.values.toStream).map(_.whole)))

  def countAtoms: ShowSize.Node = {
    val counted =
      allRichText.map {
        case (name, txts) =>
          ShowSize.Node.countChildren(name, txts.flatMap(_.toStream))(Atom.Type.of(_).toString)
      }
    ShowSize.Node.sum("Atoms", counted: _*)
  }

  def atag(id: ApplicableTagId): Must[ApplicableTag] =
    Must.fromOption(tags.data.get(id), s"No tag found with $id")
      .flatMap(t => t.tag match {
        case a: ApplicableTag => Must(a)
        case _                => Must.Failed(s"$t is not an ApplicableTag")
      })

  def atags[M[X] <: TraversableOnce[X]: Monoidish](ids: M[ApplicableTagId]): Must[M[ApplicableTag]] =
    Must.foldMapM(ids)(atag)

  def atags: Stream[ApplicableTag] =
    tags.data.vstream(_.tag).filterT[ApplicableTag]

  def customField[I <: CustomFieldId, D <: CustomField](id: I)(implicit d: DataIdAux[D, I]): Must[D] =
    fields.data.customFields(id).flatMap(f =>
      Must.fromOption(d.unapplyData(f), s"$id associated with wrong type: $f"))

  def customIssueType(id: CustomIssueTypeId): Must[CustomIssueType] =
    Must.fromOption(customIssueTypes.data.get(id), s"No CustomIssueType found with $id")

  lazy val customTagFields =
    fields.data.customFields.values.filterT[CustomField.Tag]

  lazy val customTextFields =
    fields.data.customFields.values.filterT[CustomField.Text]

  lazy val liveCustomTextFields =
    customTextFields.filter(_.live :: Live)

  def reqType(i: ReqTypeId): Must[ReqType] =
    i.foldId[Must[ReqType]](Must.apply, customReqTypes.data.apply)

  def reqTypeC(i: CustomReqTypeId): Must[CustomReqType] =
    reqType(i).flatMap {
      case c: CustomReqType => Must(c)
      case f                => Must.Failed(s"$f must be a CustomReqType")
    }

  lazy val reqTypes: Stream[ReqType] =
    (customReqTypes.data.values.toStream: Stream[ReqType]) append
      (StaticReqType.valueStream        : Stream[ReqType])

  lazy val reqTypesByMnemonic: Map[ReqType.Mnemonic, ReqType] =
    reqTypes.flatMap(t => t.allMnemonics.toStream.map((_, t))).toMap

  lazy val liveTagColumnDistribution =
    TagColumnDistribution(this, _.live :: Live)

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

  private def implicationTransitiveClosure(f: Implications => ImplicationsU): TransitiveClosure[ReqId] =
    ReqFieldData.implicationTransitiveClosure(
      reqs.data.reqs.keys,
      reqs.data.dead,
      f(reqFieldData.data.implications))

  /** Keys are lowercase */
  lazy val hashRefLookupM: Map[String, HashRefTarget] = (
      atags.map(t => (t.key.value.toLowerCase, -\/(t))) append
      customIssueTypes.data.vstream(t => (t.key.value.toLowerCase, \/-(t)))
    ).toMap

  def hashRefLookup(key: String): Option[HashRefTarget] =
    hashRefLookupM.get(key.toLowerCase)

  lazy val tagsInTextR  : Multimap[ReqId,     Set,  ApplicableTagId] = Multimap(scanAllLiveTextR(Text.findTags(_),   Text.findTags))
  lazy val issuesInTextR: Multimap[ReqId,     Vector, Atom.AnyIssue] = Multimap(scanAllLiveTextR(Text.findIssues(_), Text.findIssues))
  lazy val issuesInTextG: Multimap[ReqCodeId, Vector, Atom.AnyIssue] = Multimap(scanAllLiveTextG(Text.findIssues(_)))

  private def scanAllLiveTextR[R](f1: Text.GenericReqTitle.OptionalText => R,
                                  f2: (Text.CustomTextField.OptionalText, R) => R): Map[ReqId, R] = {
    val textData   = reqFieldData.data.text
    val textFields = liveCustomTextFields.map(_.id)

    def searchCustomTextFields(id: ReqId, into: R): R =
      textFields.foldLeft(into)((q, f) =>
        textData.get(f).flatMap(_ get id).fold(q)(txt => f2(txt.whole, q)))

    reqs.data.reqs.mapValues {
      case r: GenericReq => searchCustomTextFields(r.id, f1(r.title))
    }
  }

  private def scanAllLiveTextG[R](f: Text.ReqCodeGroupTitle.OptionalText => R): Map[ReqCodeId, R] =
    reqCodes.data.activeGroups
      .foldLeft(Map.empty[ReqCodeId, R])((m, g) =>
        m.updated(g.id, f(g.group.title)))

  // Finally, ensure validity
  import japgolly.nyaya._
  this assertSatisfies DataProp.project.all
}

object Project {
  implicit def equality: Equal[Project] = deriveEqual
}