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
    def reqType        = "Type"
    def pubid          = "ID"
    def code           = "Code"
    def title          = "Title"
    def tags           = "Tags"
    def implicationSrc = "Implied By"
    def implicationTgt = "Implies"
    def deletionReason = "Deletion Reason"
  }

  object FieldNames {
    def hashRefKey          = "Key"
    def fieldRefKey         = hashRefKey
    def reqCode             = "Code"
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
    def reqType             = ColumnNames.reqType
    def implications        = "Implications"
  }

  object Cfg {
    def startNewButton    = "Create"
    def abortNewButton    = "Cancel"
    def retryFailedButton = "Retry"
  }

  def reqCodeGroup = "Code Group"
  def reqCodeGroups = "Code Groups"

  def buttonCommitChange = "OK"
  def buttonAbortChange = "Cancel"
}
