package shipreq.webapp.base

import scala.collection.compat.IterableOnce
import shipreq.base.util.{Backwards, Direction, Forwards, LeftRight}
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.data.{Dead, HashRefKey, Live, StaticField, StaticFieldType}
import shipreq.webapp.base.issue.IssueCategory
import shipreq.webapp.base.text.{PlainText, Text}

object UiText {

  def mathFailed = "{Invalid expression}"

  @inline implicit class EnglishStringExt(private val self: String) extends AnyVal {
    def pluralise(i: Int, pluralised: String = null): String =
      if (i == 1)
        self
      else if (pluralised ne null)
        pluralised
      else
        self + "s"
  }

  @inline implicit class EnglishIntExt(private val self: Int) extends AnyVal {
    def unitsOf(name: String, pluralised: String = null): String =
      self + " " + name.pluralise(self, pluralised)
  }

  def sortedOrClause(cases: IterableOnce[String], limit: Int = Int.MaxValue) = {
    var a = cases.toArray
    scala.util.Sorting.quickSort(a)
    val len = a.length
    if (len > limit) {
      a = a.take(limit + 1)
      a(limit) = s"one of ${len - limit} others"
    }
    val last = a.length - 1
    a(last) = "or " + a(last)
    a.mkString(", ")
  }

  def unsavedChanges(i: Int): String =
    i.unitsOf("unsaved change")

  object ColumnNames {
    def reqType          = FieldNames.reqType
    def pubid            = "ID"
    def id               = "ID"
    def code             = FieldNames.reqCode
    def title            = "Title"
    def tags             = FieldNames.tags
    def deletionReason   = FieldNames.deletionReason
    def issueCategory    = "Issue Type"
    def issueClass       = "Issue"
    def issueFieldName   = "Field"
    def issueFieldEditor = "Field Editor"
    def issueActions     = "Actions"

    val implications: Direction => String = {
      case Backwards => "Implied By"
      case Forwards => "Implies"
    }
  }

  object FieldNames {
    def hashRefKey           = "Key"
    def fieldRefKey          = hashRefKey
    def reqCode              = "Code"
    def reqCodes             = "Codes"
    def reqCodeNode          = "Code node"
    def name                 = "Name"
    def colour               = "Colour"
    def desc                 = "Description"
    def mnemonic             = "Mnemonic"
    def exclusivity          = "Exclusive"
    def fieldType            = "Type"
    def mandatory            = "Mandatory"
    def applicableReqTypes   = "Applicable Req Types"
    def dndDragHandleHeader  = ""
    def usage                = "Usage"
    def implicationRequired  = "Implication Required"
    def reqType              = "Type"
    def tags                 = "Tags"
    def implications         = "Implications"
    def implicationGraph     = StaticFieldType.ImplicationGraph.name
    def useCaseStepTreeN     = "Normal Course"
    def useCaseStepTreeA     = "Alternative Courses"
    def useCaseStepTreeE     = StaticField.ExceptionStepTree.name
    def useCaseStepFlowGraph = StaticFieldType.UseCaseStepGraph.name
    def deletionReason       = "Deletion Reason"
    def pastPubids           = "Past IDs"
    def savedViewName        = "Name"
    def impFieldSource       = "Req Type"
  }

  object RichText {
    val descPlural: Text.Base => String = {
      case _: Text.ReqTitle     => "req titles"
      case Text.CodeGroupTitle  => s"${UiText.codeGroup.toLowerCase} titles"
      case Text.InlineIssueDesc => "issue info blocks"
      case Text.CustomTextField => "text fields"
      case Text.UseCaseStep     => "use case steps"
      case Text.DeletionReason  => FieldNames.deletionReason.toLowerCase.pluralise(2)
      case Text.ManualIssue     => Issues.looseIssues
    }
  }

  object Cfg {
    def startNewButton    = "Create"
    def abortNewButton    = "Cancel"
    def retryFailedButton = buttonRetry
  }

  object Life {
    def status(l: Live): String =
      l match {
        case Live => live
        case Dead => dead
      }

    def field             = "Live Status"
    def live              = "Live"
    def dead              = "Deleted"
    def delete            = "Delete"
    def deleteReqs        = delete + " Reqs"
    def restoreReqs       = restore + " Reqs"
    def deleteCodeGroups  = delete + " " + codeGroups
    def restoreCodeGroups = restore + " " + codeGroups
    def restore           = "Restore"
    def showDead          = "Show deleted content."
  }

  def codeGroup = "Code Group"
  def codeGroups = "Code Groups"
  def useCase = "Use Case"

  def buttonCommitChange = "OK"
  def buttonAbortChange = "Cancel"

  def buttonRetry = "Retry"

  def doubleClickToEdit = "double-click to edit"

  // An empty string cannot be used as a hover, nothing appears which leaves the impression (as a user) that the hover
  // isn't popping up properly or they haven't hovered long enough.
  def hoverText(t: String): String =
    if (t.isEmpty) "<blank>" else t

  val useCaseStepShift: LeftRight => String = {
    case LeftRight.Left  => "Unindent"
    case LeftRight.Right => "Indent"
  }

  object Issues {
    final val loose = "manual"
    final val Loose = "Manual"

    final val looseIssues = loose + " issues"

    val category: IssueCategory => String = {
      case IssueCategory.BadData     => "Bad data"
      case IssueCategory.Futility    => "Futility"
      case IssueCategory.MissingData => "Missing data"
      case IssueCategory.UserDefined => "User-defined"
    }

    def descBlankCustomField(field: String): String =
      "Mandatory field is blank: " + field

    def descBlankTitle: String =
      "Title is blank"

    def descBlankUseCaseStep: String =
      "Use case step is blank"

    def descConflictingTags(tag: String): String =
      "Conflicting " + tag + " tags"

    def descDeadIssueTag(tag: HashRefKey): String =
      "Deleted issue tag in use: " + PlainText.hashtag(tag)

    def descDeadRef: String =
      "Reference to deleted data"

    def descDeadTag(tag: HashRefKey): String =
      "Deleted tag in use: " + PlainText.hashtag(tag)

    def descEmptyCodeGroup: String =
      "Code group has nothing to group"

    def descFieldDefaultTagDead(field: String, tag: String): String =
      s"$field field using deleted tag $tag as a default"

    def descFieldDefaultTagNotApplicable(field: String, tag: String, reqType: String): String =
      s"$field field defaults to $tag for $reqType, but $tag isn't applicable to $reqType"

    def descFieldDefaultTagUnrelated(field: String, tag: String): String =
      s"$field field using unrelated tag $tag as a default"

    def descImplicationRequired(reqType: Mnemonic): String =
      "Implication required for req type: " + reqType.value

    def descIssueTag(tag: HashRefKey): String =
      PlainText.hashtag(tag)

    def descNonApplicableField(field: String): String =
      field + " field not applicable to any req types"

    def descUninhabitableTagField(field: String): String =
      field + " field has no tags"

    def descManualIssue: String =
      "Manual"
  }

}
