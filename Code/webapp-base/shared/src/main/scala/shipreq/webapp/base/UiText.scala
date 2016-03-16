package shipreq.webapp.base

object UiText {

  def mathFailed = "{Invalid expression}"

  @inline implicit class EnglishIntExt(val _i: Int) extends AnyVal {
    def unitsOf(name: String, pluralised: String = null): String = {
      val units =
        if (_i == 1)
          name
        else if (pluralised ne null)
          pluralised
        else
          name + "s"
      s"${_i} $units"
    }
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
    def hashRefKey          = "Key"
    def fieldRefKey         = hashRefKey
    def reqCode             = "Code"
    def reqCodes            = "Codes"
    def reqCodeNode         = "Code node"
    def name                = "Name"
    def desc                = "Description"
    def mnemonic            = "Mnemonic"
    def mutexChildren       = "Choice"
    def fieldType           = "Type"
    def mandatory           = "Mandatory"
    def applicableReqTypes  = "Req Types"
    def dndDragHandleHeader = ""
    def usage               = "Usage"
    def implicationRequired = "Implication Required"
    def reqType             = "Type"
    def tags                = "Tags"
    def implications        = "Implications"
    def useCaseStepTreeN    = "Normal Course"
    def useCaseStepTreeA    = "Alternative Courses"
    def useCaseStepTreeE    = "Exceptions"
    def deletionReason      = "Deletion Reason"
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
