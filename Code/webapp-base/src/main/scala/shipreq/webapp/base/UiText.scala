package shipreq.webapp.base

object UiText {

  def entityNameNotFound = "?"

  object ColumnNames {
    def reqType = "Type"
    def pubId   = "ID"
    def code    = "Code"
    def desc    = "Desc"
  }

  object FieldNames {
    def hashRefKey          = "Key"
    def fieldRefKey         = hashRefKey
    def name                = "Name"
    def desc                = "Description"
    def mnemonic            = "Mnemonic"
    def mutexChildren       = "Choice"
    def fieldType           = "Type"
    def mandatory           = "Mandatory"
    def applicableReqTypes  = "Req Types"
    def dndDragHandleHeader = ""
  }

  object Cfg {
    def startNewButton    = "Create"
    def abortNewButton    = "Cancel"
    def retryFailedButton = "Retry"
  }
}
