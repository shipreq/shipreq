package shipreq.webapp.base

import shipreq.base.util.{Backwards, Direction, Forwards, LeftRight}
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.data.{Dead, HashRefKey, Live, StaticField, StaticFieldType}
import shipreq.webapp.base.issue.IssueCategory
import shipreq.webapp.base.text.PlainText

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
    def desc                 = "Description"
    def mnemonic             = "Mnemonic"
    def mutexChildren        = "Exclusive"
    def fieldType            = "Type"
    def mandatory            = "Mandatory"
    def applicableReqTypes   = "Req Types"
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
    def useCaseStepFlowGraph = StaticFieldType.StepGraph.name
    def deletionReason       = "Deletion Reason"
    def pastPubids           = "Past IDs"
    def savedViewName        = "Name"
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

    val category: IssueCategory => String = {
      case IssueCategory.BadData     => "Bad data"
      case IssueCategory.Futility    => "Futility"
      case IssueCategory.MissingData => "Missing data"
      case IssueCategory.UserDefined => "User-defined"
    }

    def descBlankCustomField     (field: String)    : String = "Mandatory field is blank: " + field
    def descBlankTitle                              : String = "Title is blank"
    def descBlankUseCaseStep                        : String = "Use case step is blank"
    def descConflictingTags      (tag: String)      : String = "Conflicting " + tag + " tags"
    def descDeadIssueTag         (tag: HashRefKey)  : String = "Deleted issue tag in use: " + PlainText.hashtag(tag)
    def descDeadRef                                 : String = "Reference to deleted data"
    def descDeadTag              (tag: HashRefKey)  : String = "Deleted tag in use: " + PlainText.hashtag(tag)
    def descEmptyCodeGroup                          : String = "Code group has nothing to group"
    def descImplicationRequired  (reqType: Mnemonic): String = "Implication required for req type: " + reqType.value
    def descIssueTag             (tag: HashRefKey)  : String = PlainText.hashtag(tag)
    def descUninhabitableTagField(field: String)    : String = field + " field has no tags"
    def descManualIssue                             : String = "Manual"
  }
}
