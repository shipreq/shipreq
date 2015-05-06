package shipreq.webapp.base.data

import monocle.macros.GenLens
import scalaz.{-\/, \/-}
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{Monoidish, Must}
import shipreq.base.util.UnivEq.{immutableHashMapMemo => memo}
import shipreq.webapp.base.TransitiveClosure
import shipreq.webapp.base.text.{Atom, Text}
import shipreq.webapp.base.util.ShowSize
import DataImplicits._

final case class RevAnd[D](rev: Rev, data: D)

object RevAnd {
  def data[D] = GenLens[RevAnd[D]](_.data)
}

object Project {
  val customIssueTypes = GenLens[Project](_.customIssueTypes)
  val customReqTypes   = GenLens[Project](_.customReqTypes)
  val fields           = GenLens[Project](_.fields)
  val tags             = GenLens[Project](_.tags)
  val reqs             = GenLens[Project](_.reqs)
  val reqCodes         = GenLens[Project](_.reqCodes)
  val reqFieldData     = GenLens[Project](_.reqFieldData)
}

final case class Project(customIssueTypes: RevAnd[CustomIssueTypeIMap],
                         customReqTypes  : RevAnd[CustomReqTypeIMap],
                         fields          : RevAnd[FieldSet],
                         tags            : RevAnd[TagTree],
                         reqs            : RevAnd[Requirements],
                         reqCodes        : RevAnd[ReqCodes],
                         reqFieldData    : RevAnd[ReqFieldData]) {

  import japgolly.nyaya.{Atom => _, _}
  this assertSatisfies DataProp.project.all

  def rev =
    customIssueTypes.rev +
    customReqTypes  .rev +
    fields          .rev +
    tags            .rev +
    reqs            .rev +
    reqCodes        .rev +
    reqFieldData    .rev

  override def toString =
    ShowSize(this).showTree

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

  def atag(id: ApplicableTag.Id): Must[ApplicableTag] =
    Must.fromOption(tags.data.get(id), s"No tag found with $id")
      .flatMap(t => t.tag match {
        case a: ApplicableTag => Must(a)
        case _                => Must.Failed(s"$t is not an ApplicableTag")
      })

  def atags[M[X] <: TraversableOnce[X]: Monoidish](ids: M[ApplicableTag.Id]): Must[M[ApplicableTag]] =
    Must.foldMapM(ids)(atag)

  def customField[I <: CustomField.Id, D <: CustomField](id: I)(implicit d: DataIdAux[D, I]): Must[D] =
    fields.data.customFields(id).flatMap(f =>
      Must.fromOption(d.unapplyData(f), s"$id associated with wrong type: $f"))

  def customIssueType(id: CustomIssueType.Id): Must[CustomIssueType] =
    Must.fromOption(customIssueTypes.data.get(id), s"No CustomIssueType found with $id")

  lazy val customTagFields =
    fields.data.customFields.keys.filterT[CustomField.Tag.Id]

  lazy val customTextFields =
    fields.data.customFields.keys.filterT[CustomField.Text.Id]

  def reqType(i: ReqTypeId): Must[ReqType] =
    i.foldId[Must[ReqType]](Must.Exists(_), customReqTypes.data.apply)

  lazy val reqTypes: Stream[ReqType] =
    (customReqTypes.data.values.toStream: Stream[ReqType]) #:::
      (StaticReqType.valueStream        : Stream[ReqType])

  lazy val reqTypesByMnemonic: Map[ReqType.Mnemonic, ReqType] =
    reqTypes.flatMap(t => t.allMnemonics.toStream.map((_, t))).toMap

  lazy val tagColumnDistribution = new TagColumnDistribution(this)

  /** Transitive closure of implications going source → target. */
  lazy val implicationSrcToTgtTC: TransitiveClosure[ReqId] =
    TransitiveClosure.auto[ReqId](reqs.data.reqs.keys)(reqFieldData.data.implications.srcToTgt.apply)

  /** Transitive closure of implications going target → source. */
  lazy val implicationTgtToSrcTC: TransitiveClosure[ReqId] =
    TransitiveClosure.auto[ReqId](reqs.data.reqs.keys)(reqFieldData.data.implications.tgtToSrc.apply)

  /** Keys are lowercase */
  lazy val hashRefLookupM: Map[String, HashRefTarget] = (
      tags.data.vstream(_.tag).filterT[ApplicableTag].map(t => (t.key.value.toLowerCase, -\/(t))) append
      customIssueTypes.data.vstream(t => (t.key.value.toLowerCase, \/-(t)))
    ).toMap

  def hashRefLookup(key: String): Option[HashRefTarget] =
    hashRefLookupM.get(key.toLowerCase)
}


final class TagColumnDistribution(p: Project) {
  // Traversing the tag tree for used columns is better than calculating the full
  // transitive closure at O(V²) space and O(V²+VE) time.
  private[this] implicit val tagTree = p.tags.data

  type TagIds = Must[Set[ApplicableTag.Id]]

  val tagIdsForColumn: CustomField.Tag.Id => TagIds =
    memo(fid =>
      p.customField(fid).flatMap(field =>
        tagTree(field.tagId)
          .flatMap(_.transitiveChildren)
          .map(_.filterT[ApplicableTag.Id].toSet)))

  lazy val tagIdsUsedInColumns: TagIds =
    Must.foldMapMF(p.customTagFields)(tagIdsForColumn)

  lazy val tagIdsNotUsedInColumns: TagIds =
    tagIdsUsedInColumns.map(s =>
      tagTree.vstream(_.tag.id)
        .filterT[ApplicableTag.Id]
        .filterNot(s.contains)
        .toSet)

  type Tags = Must[Set[ApplicableTag]]

  val tagsForColumn: CustomField.Tag.Id => Tags =
    tagIdsForColumn(_) flatMap p.atags[Set]

  lazy val tagsUsedInColumns: Tags =
    tagIdsUsedInColumns flatMap p.atags[Set]

  lazy val tagsNotUsedInColumns: Tags =
    tagIdsNotUsedInColumns flatMap p.atags[Set]
}
