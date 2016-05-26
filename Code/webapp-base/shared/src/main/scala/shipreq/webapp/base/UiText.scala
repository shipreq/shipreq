package shipreq.webapp.base

import shipreq.webapp.base.data.{StaticField, StaticFieldType}

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
      name.pluralise(self, pluralised)
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
    def retryFailedButton = "Retry"
  }

  object Life {
    /*
    def field    = "Active Status"
    def live     = "Active"
    def dead     = "Deleted"
    def delete   = "Delete"
    def restore  = "Restore"
    def showDead = "Show deleted content."
    */

    def field    = "Life Status"
    def live     = "Alive"
    def dead     = "Dead"
    def delete   = "Kill"
    def restore  = "Resurrect"
    def showDead = "Show dead content."
  }

  def reqCodeGroup = "Code Group"
  def reqCodeGroups = "Code Groups"
  def useCase = "Use Case"

  def buttonCommitChange = "OK"
  def buttonAbortChange = "Cancel"
}
