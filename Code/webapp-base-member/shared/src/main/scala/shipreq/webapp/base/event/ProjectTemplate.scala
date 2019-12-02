package shipreq.webapp.base.event

import japgolly.microlibs.nonempty._
import shipreq.base.util.ISubset
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.sort.SortMethod
import shipreq.webapp.base.util.GenericDataMacros._
import Event._
import Field.ApplicableReqTypes

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
    def reqType(mnemonicStr: String, name: String, imp: ImplicationRequired) = {
      val mnemonic = ReqType.Mnemonic(mnemonicStr)
      val id = reqTypeId.next()
      add(CustomReqTypeCreate(id, gdAllValues(CustomReqTypeGD, "")))
      id
    }

    val issueTypeId = new IdCounter(CustomIssueTypeId)
    def issueType(keyStr: String, desc: Option[String]): Unit = {
      val key = HashRefKey(keyStr)
      add(CustomIssueTypeCreate(issueTypeId.next(), gdAllValues(CustomIssueTypeGD, "")))
    }

    val tagId = new IdCounter(identity)
    def tagGroup(name         : String,
                 desc         : Option[String],
                 mutexChildren: MutexChildren,
                 parents      : TagInTree.Parents  = Map.empty,
                 children     : TagInTree.Children = Vector.empty) = {
      val id = TagGroupId(tagId.next())
      add(TagGroupCreate(id, gdAllValues(TagGroupGD, "")))
      id
    }
    def applicableTag(name    : String,
                      desc    : Option[String],
                      key     : HashRefKey,
                      parents : TagInTree.Parents  = Map.empty,
                      children: TagInTree.Children = Vector.empty) = {
      val id = ApplicableTagId(tagId.next())
      add(ApplicableTagCreate(id, gdAllValues(ApplicableTagGD, "")))
      id
    }

    val allReqTypes: ApplicableReqTypes = ISubset.All()
    val customFieldId = new IdCounter(identity)
    def customTextField(name: String, key: FieldRefKey, mandatory: Mandatory, reqTypes: ApplicableReqTypes): Unit = {
      val id = CustomField.Text.Id(customFieldId.next())
      add(FieldCustomTextCreate(id, gdAllValues(CustomTextFieldGD, "")))
    }
    def customTagField(tagId: TagId, mandatory: Mandatory, reqTypes: ApplicableReqTypes): Unit = {
      val id = CustomField.Tag.Id(customFieldId.next())
      add(FieldCustomTagCreate(id, gdAllValues(CustomTagFieldGD, "")))
    }
    def customImpField(reqTypeId: ReqTypeId, mandatory: Mandatory, reqTypes: ApplicableReqTypes): Unit = {
      val id = CustomField.Implication.Id(customFieldId.next())
      add(FieldCustomImpCreate(id, gdAllValues(CustomImpFieldGD, "")))
    }

    val savedViewId = new IdCounter(reqtable.SavedView.Id)
    def savedView(name      : String,
                  columns   : NonEmptyVector[reqtable.Column],
                  order     : reqtable.SortCriteria,
                  filterDead: FilterDead                      = HideDead,
                  filter    : Option[Filter.Valid]            = None): Unit =
      add(SavedViewCreate(savedViewId.next(), reqtable.SavedView.Name(name), columns, order, filterDead, filter))
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  case object V1 extends ProjectTemplate {
    override lazy val events: Vector[Event] = {
      val qb = new QuickBuilder
      import qb._

      val co = reqType("CO", "Constraint",                     ImplicationRequired.Not)
      val fr = reqType("FR", "Functional Requirement",         ImplicationRequired)
      val mf = reqType("MF", "Major Feature",                  ImplicationRequired.Not)
      val oe = reqType("OE", "Operating Environment",          ImplicationRequired.Not)
      val qa = reqType("QA", "Quality Attribute",              ImplicationRequired)

      issueType("TO"+"DO", "Work needs to be done.")
      issueType("PENDING", "Waiting on external information, or an external event.")

      tagGroup("Actors", None, MutexChildren.Not)

      val must   = applicableTag("Must",   "Requirement is critical to the current delivery timebox in order for it to be a success. If even one MUST requirement is not included, the project delivery should be considered a failure", HashRefKey("must"))
      val should = applicableTag("Should", "Requirement is important but not necessary for delivery in the current delivery timebox.", HashRefKey("should"))
      val could  = applicableTag("Could",  "Requirement is desirable but not necessary, and could improve user experience or customer satisfaction for little development cost. These will typically be included if time and resources permit.", HashRefKey("could"))
      val pri    = tagGroup("Priority", None, MutexChildren, children = Vector(must, should, could))

      val v10  = applicableTag("Version 1.0", None, HashRefKey("v1.0"))
      val urel = tagGroup("Unreleased", "Product version in which requirements are planned for implementation.", MutexChildren.Not, children = Vector(v10))
      val rel  = tagGroup("Released", "Product version in which requirements were implemented.", MutexChildren.Not)
      val ver  = tagGroup("Version", "Target product version.", MutexChildren.Not, children = Vector(rel, urel))

      customTextField("Detail", FieldRefKey("detail"), Mandatory.Not, allReqTypes)
      customImpField(mf,     Mandatory    , ISubset.Not(NonEmptySet(mf, oe)))
      customTagField(pri,    Mandatory.Not, allReqTypes)
      customTagField(ver,    Mandatory.Not, allReqTypes)

      import reqtable._, SortCriterion.SyntaxHelpers._

      savedView("Default",
        NonEmptyVector(Column.Pubid, Column.Title, Column.Tags),
        SortCriteria(Vector.empty, Column.Pubid / SortMethod.Asc))

      savedView("By Code",
        NonEmptyVector(Column.Code, Column.Pubid, Column.Title, Column.Tags),
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
