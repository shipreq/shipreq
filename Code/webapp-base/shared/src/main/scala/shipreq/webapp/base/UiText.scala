package shipreq.webapp.base

import shipreq.webapp.base.data.{Dead, Live, StaticField, StaticFieldType}

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
    def reqType        = FieldNames.reqType
    def pubid          = "ID"
    def code           = FieldNames.reqCode
    def title          = "Title"
    def tags           = FieldNames.tags
    def implicationSrc = "Implied By"
    def implicationTgt = "Implies"
    def deletionReason = FieldNames.deletionReason
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
    def mutexChildren        = "Choice"
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

    def field    = "Live Status"
    def live     = "Live"
    def dead     = "Deleted"
    def delete   = "Delete"
    def restore  = "Restore"
    def showDead = "Show deleted content."
  }

  def reqCodeGroup = "Code Group"
  def reqCodeGroups = "Code Groups"
  def useCase = "Use Case"

  def buttonCommitChange = "OK"
  def buttonAbortChange = "Cancel"

  def buttonRetry = "Retry"
}
