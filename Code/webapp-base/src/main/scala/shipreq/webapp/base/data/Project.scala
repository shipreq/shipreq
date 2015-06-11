package shipreq.webapp.base.data

import japgolly.nyaya.util.Multimap
import monocle.macros.{GenLens, Lenses}
import scalaz.{-\/, \/-}
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{UnivEq, Monoidish, Must}
import shipreq.webapp.base.text.{Atom, Text}
import shipreq.webapp.base.util.{TransitiveClosure, ShowSize}
import DataImplicits._
import ReqFieldData.{Implications, ImplicationsU}

final case class RevAnd[D](rev: Rev, data: D)

object RevAnd {
  def rev[D] = GenLens[RevAnd[D]](_.rev)
  def data[D] = GenLens[RevAnd[D]](_.data)
}

@Lenses
final case class Project(customIssueTypes: RevAnd[CustomIssueTypeIMap],
                         customReqTypes  : RevAnd[CustomReqTypeIMap],
                         fields          : RevAnd[FieldSet],
                         tags            : RevAnd[TagTree],
                         reqs            : RevAnd[Requirements],
                         reqCodes        : RevAnd[ReqCodes],
                         reqFieldData    : RevAnd[ReqFieldData]) {

  val rev: Rev =
    customIssueTypes.rev +
    customReqTypes  .rev +
    fields          .rev +
    tags            .rev +
    reqs            .rev +
    reqCodes        .rev +
    reqFieldData    .rev

  override def toString =
    s"Project($rev)"
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

  def reqType(i: ReqTypeId): Must[ReqType] =
    i.foldId[Must[ReqType]](Must.apply, customReqTypes.data.apply)

  def reqTypeC(i: CustomReqTypeId): Must[CustomReqType] =
    reqType(i).flatMap {
      case c: CustomReqType => Must(c)
      case f                => Must.Failed(s"$f must be a CustomReqType")
    }

  lazy val reqTypes: Stream[ReqType] =
    (customReqTypes.data.values.toStream: Stream[ReqType]) #:::
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

  /**
   * Searches all (live) text fields in each reqs for TagRefs.
   */
  lazy val tagsInText: Multimap[ReqId, Set, ApplicableTagId] = {
    type Tags      = Set[ApplicableTagId]
    val textData   = reqFieldData.data.text
    val textFields = customTextFields.filter(_.live :: Live).map(_.id)

    def searchCustomTextFields(id: ReqId, into: Tags): Tags =
      textFields.foldLeft(into)((q, f) =>
        textData.get(f).flatMap(_ get id) match {
          case None      => q
          case Some(txt) => Text.findTags(txt.whole, q)
        }
      )

    def findAll(req: Req): Tags =
      req match {
        case r: GenericReq => searchCustomTextFields(r.id, Text.findTags(r.title))
      }

    val m = reqs.data.reqs.mapValues(findAll)
    Multimap(m)
  }

  // Finally, ensure validity
  import japgolly.nyaya._
  this assertSatisfies DataProp.project.all
}
