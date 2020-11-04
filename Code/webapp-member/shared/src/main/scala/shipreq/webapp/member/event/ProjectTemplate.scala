package shipreq.webapp.member.event

import shipreq.base.util._
import shipreq.webapp.member.data._
import shipreq.webapp.member.event.Event._
import shipreq.webapp.member.event.RetiredGenericData._
import shipreq.webapp.member.filter.Filter
import shipreq.webapp.member.sort.SortMethod
import shipreq.webapp.member.util.GenericDataMacros._

/**
 * Once a [[ProjectTemplate]] has been used (i.e. an Event exists in the database which refers to it),
 * it must never change, else it would break the hash integrity of all subsequent events.
 */
sealed trait ProjectTemplate {
  def events: Vector[Event]
}

object ProjectTemplate {

  private class QuickBuilder {
    class IdCounter[A](f: Int => A) {
      var ceil = 0
      def inc(): Unit =
        ceil += 1
      def next(): A = {
        inc()
        f(ceil)
      }
    }

    implicit def mkDesc(s: String): Option[String] =
      if (s.isEmpty) None else Some(s)

    var events = Vector.empty[Event]
    def add(e: Event): Unit =
      events :+= e

    val reqTypeId = new IdCounter(CustomReqTypeId)
    def reqTypeV1(mnemonicStr: String, name: String, implication: Mandatory) = {
      val id = reqTypeId.next()
      add(CustomReqTypeCreateV1(id, CustomReqTypeGDv1(
        mnemonic    = ReqType.Mnemonic(mnemonicStr),
        name        = name,
        implication = implication,
      )))
      id
    }

    val issueTypeId = new IdCounter(CustomIssueTypeId)
    def issueType(keyStr: String, desc: Option[String]): Unit = {
      add(CustomIssueTypeCreate(issueTypeId.next(), CustomIssueTypeGD(
        key  = HashRefKey(keyStr),
        desc = desc,
      )))
    }

    val tagId = new IdCounter(identity)

    @nowarn("cat=unused")
    def tagGroup(name       : String,
                 desc       : Option[String],
                 exclusivity: Exclusivity,
                 parents    : TagInTree.Parents  = Map.empty,
                 children   : TagInTree.Children = Vector.empty) = {
      val id = TagGroupId(tagId.next())
      add(TagGroupCreate(id, gdAllValues(TagGroupGD, "")))
      id
    }

    @nowarn("cat=unused")
    def applicableTagV1(name    : String,
                        desc    : Option[String],
                        key     : HashRefKey,
                        parents : TagInTree.Parents  = Map.empty,
                        children: TagInTree.Children = Vector.empty) = {
      val id = ApplicableTagId(tagId.next())
      add(ApplicableTagCreateV1(id, gdAllValues(RetiredGenericData.ApplicableTagGDv1, "")))
      id
    }

    def allReqTypes = ApplicableReqTypes.empty
    val customFieldId = new IdCounter(identity)

    @nowarn("cat=unused")
    def customTextField(name: String, key: String, mandatory: Mandatory, applicableReqTypes: ApplicableReqTypes): Unit = {
      val id = CustomField.Text.Id(customFieldId.next())
      add(FieldCustomTextCreateV1(id, gdAllValues(CustomTextFieldGDv1, "")))
    }

    @nowarn("cat=unused")
    def customTagField(tagId: TagId, mandatory: Mandatory, applicableReqTypes: ApplicableReqTypes): Unit = {
      val id = CustomField.Tag.Id(customFieldId.next())
      add(FieldCustomTagCreateV1(id, gdAllValues(CustomTagFieldGDv1, "")))
    }

    @nowarn("cat=unused")
    def customImpField(reqTypeId: ReqTypeId, mandatory: Mandatory, applicableReqTypes: ApplicableReqTypes): Unit = {
      val id = CustomField.Implication.Id(customFieldId.next())
      add(FieldCustomImpCreateV1(id, gdAllValues(CustomImpFieldGDv1, "")))
    }

    val savedViewId = new IdCounter(savedview.SavedView.Id)
    def savedView(name      : String,
                  columns   : NonEmptyVector[savedview.Column],
                  order     : savedview.SortCriteria,
                  filterDead: FilterDead                      = HideDead,
                  filter    : Option[Filter.Valid]            = None): Unit =
      add(SavedViewCreateV1(savedViewId.next(), savedview.SavedView.Name(name), columns, order, filterDead, filter))
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  case object V1 extends ProjectTemplate {
    override lazy val events: Vector[Event] = {
      val qb = new QuickBuilder
      import qb._

    /*val co */reqTypeV1("CO", "Constraint",             Optional)
    /*val fr */reqTypeV1("FR", "Functional Requirement", Mandatory)
      val mf = reqTypeV1("MF", "Major Feature",          Optional)
      val oe = reqTypeV1("OE", "Operating Environment",  Optional)
    /*val qa */reqTypeV1("QA", "Quality Attribute",      Mandatory)

      issueType("TO"+"DO", "Work needs to be done.")
      issueType("PENDING", "Waiting on external information, or an external event.")

      tagGroup("Actors", None, NonExclusive)

      val must   = applicableTagV1("Must",   "Requirement is critical to the current delivery timebox in order for it to be a success. If even one MUST requirement is not included, the project delivery should be considered a failure", HashRefKey("must"))
      val should = applicableTagV1("Should", "Requirement is important but not necessary for delivery in the current delivery timebox.", HashRefKey("should"))
      val could  = applicableTagV1("Could",  "Requirement is desirable but not necessary, and could improve user experience or customer satisfaction for little development cost. These will typically be included if time and resources permit.", HashRefKey("could"))
      val pri    = tagGroup("Priority", None, Exclusive, children = Vector(must, should, could))

      val v10  = applicableTagV1("Version 1.0", None, HashRefKey("v1.0"))
      val urel = tagGroup("Unreleased", "Product version in which requirements are planned for implementation.", NonExclusive, children = Vector(v10))
      val rel  = tagGroup("Released", "Product version in which requirements were implemented.", NonExclusive)
      val ver  = tagGroup("Version", "Target product version.", NonExclusive, children = Vector(rel, urel))

      customTextField("Detail", "detail", Optional,  allReqTypes)
      customImpField(mf,                  Mandatory, ApplicableReqTypes.blacklist(mf, oe))
      customTagField(pri,                 Optional,  allReqTypes)
      customTagField(ver,                 Optional,  allReqTypes)

      import shipreq.webapp.member.data.savedview._, SortCriterion.SyntaxHelpers._

      savedView("Default",
        NonEmptyVector(Column.Pubid, Column.Title, Column.OtherTags),
        SortCriteria(Vector.empty, Column.Pubid / SortMethod.Asc))

      savedView("By Code",
        NonEmptyVector(Column.Code, Column.Pubid, Column.Title, Column.OtherTags),
        SortCriteria(Vector(Column.Code / SortMethod.AscThenBlanks), Column.Pubid / SortMethod.Asc))

      qb.events
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /*
   * Note to future-me: Eventually this may become 2D:
   * 1) different templates
   * 2) different versions of each template (as direct template modification breaks hashes)
   */
  val values: NonEmptyVector[ProjectTemplate] =
    NonEmptyVector(V1)

  def default: ProjectTemplate =
    values.last

  implicit def equality: UnivEq[ProjectTemplate] =
    UnivEq.derive
}
