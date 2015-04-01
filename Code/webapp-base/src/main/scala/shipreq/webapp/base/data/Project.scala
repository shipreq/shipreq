package shipreq.webapp.base.data

import monocle.macros.GenLens
import scalaz.Memo
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{Monoidish, Must}
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

  import japgolly.nyaya._
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
    Stream(customIssueTypes, customReqTypes, fields, tags, reqs, reqCodes, reqFieldData)
      .map("\n    " + _.toString.replace(" -> ", " → "))
      .mkString("Project(", "", "\n)")


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

  def reqType(i: ReqType.Id): Must[ReqType] =
    i.foldId[Must[ReqType]](Must.Exists(_), customReqTypes.data.apply)

  lazy val reqTypes: Stream[ReqType] =
    (customReqTypes.data.values.toStream: Stream[ReqType]) #:::
      (StaticReqType.valueStream        : Stream[ReqType])

  lazy val tagColumnDistribution = new TagColumnDistribution(this)
}

final class TagColumnDistribution(p: Project) {
  // Traversing the tag tree for used columns is better than calculating the full
  // transitive closure at O(V²) space and O(V²+VE) time.
  private[this] implicit val tagTree = p.tags.data

  type TagIds = Must[Set[ApplicableTag.Id]]

  val tagIdsForColumn: CustomField.Tag.Id => TagIds =
    Memo.mutableHashMapMemo(fid =>
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
