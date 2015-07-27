package shipreq.webapp.base

import shipreq.base.util.Must

object UiText {

  def mustFailed = "¿ERR"

  def mathFailed = "{Invalid expression}"

  def mustA(m: Must[String], outputOnFailure: String = mustFailed): String = {
    m.fold(e => {
      System.err.println(e) // side-effect!
      outputOnFailure
    }, identity)
  }

  @inline implicit class Unmust[A](val _m: Must[String]) extends AnyVal {
    @inline def unmust: String = UiText.mustA(_m)
  }

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
  }

  object Cfg {
    def startNewButton    = "Create"
    def abortNewButton    = "Cancel"
    def retryFailedButton = "Retry"
  }

  def reqCodeGroup = "Code Group"

  def buttonCommitChange = "OK"
  def buttonAbortChange = "Cancel"
}
