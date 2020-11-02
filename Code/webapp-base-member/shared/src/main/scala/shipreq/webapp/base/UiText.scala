package shipreq.webapp.base

import scala.collection.compat.IterableOnce
import shipreq.base.util.LeftRight
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.data.{Dead, HashRefKey, Live, SpecialBuiltInField}
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
      self.toString + " " + name.pluralise(self, pluralised)
  }

  private def sortedSeqClause(cases: IterableOnce[String], conj: String, limit: Int): String = {
    var a = cases.iterator.toArray
    a.length match {
      case 0 => ""
      case 1 => a(0)
      case len =>
        scala.util.Sorting.quickSort(a)
        if (len > limit) {
          a = a.take(limit + 1)
          a(limit) = s"one of ${len - limit} others"
        }
        a.dropRight(1).mkString("", ", ", s" $conj ${a.last}")
    }
  }

  def sortedAndClause(cases: IterableOnce[String], limit: Int = Int.MaxValue): String =
    sortedSeqClause(cases, "and", limit)

  def sortedOrClause(cases: IterableOnce[String], limit: Int = Int.MaxValue): String =
    sortedSeqClause(cases, "or", limit)

  def unsavedChanges(i: Int): String =
    i.unitsOf("unsaved change")

  object ColumnNames {
    def issueCategory    = "Issue Type"
    def issueClass       = "Issue"
    def issueFieldName   = "Field"
    def issueFieldEditor = "Field Editor"
    def issueActions     = "Actions"
  }

  object FieldNames {
    def hashRefKey           = "Key"
    def fieldRefKey          = hashRefKey
    def reqCodeNode          = "Code part"
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
    def implication          = "Implication"
    def implications         = "Implications"
    def useCaseStepTreeN     = "Normal Course"
    def useCaseStepTreeA     = "Alternative Courses"
    def pastPubids           = "Past IDs"
    def pastMnemonics        = "Past Mnemonics"
    def savedViewName        = "Name"
    def impFieldSource       = "Req Type"
    def tagFieldSource       = "Tag Group"
  }

  object RichText {
    val descPlural: Text.Base => Option[String] = {
      case _: Text.ReqTitle          => Some("req titles" )
      case Text.CodeGroupTitle       => Some(s"${UiText.codeGroup.toLowerCase} titles" )
      case Text.InlineIssueDesc      => Some("issue info blocks" )
      case Text.CustomTextField      => Some("text fields" )
      case Text.UseCaseStep          => Some("use case steps" )
      case Text.DeletionReason       => Some(SpecialBuiltInField.DeletionReason.name.toLowerCase.pluralise(2) )
      case Text.ManualIssue          => Some(Issues.looseIssues )
      case Text.HeadingTitleFull
         | Text.HeadingTitleNoIssues
         | Text.StyledInnerFull
         | Text.StyledInnerContentRef
         | Text.StyledInnerNoIssues
         | Text.StyledInnerNoTags    => None
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

    def descDerivativeTagResultDead(field: String, key1: String, key2: String, tag: String): String =
      s"$field field derivative tag rule for [$key1 + $key2] results in $tag which is deleted"

    def descDerivativeTagResultUnrelated(field: String, key1: String, key2: String, tag: String, tagGroup: String): String =
      s"$field field derivative tag rule for [$key1 + $key2] results in $tag which isn't a $tagGroup anymore"

    def descDuplicateTitle: String =
      "Duplicate title"

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

    def descNonApplicableTag(tag: String): String =
      s"Non-applicable tag in use: $tag"

    def descUninhabitableTagField(field: String): String =
      field + " field has no tags"

    def descManualIssue: String =
      "Manual"
  }

}
