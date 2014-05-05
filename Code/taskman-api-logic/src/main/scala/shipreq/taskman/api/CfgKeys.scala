package shipreq.taskman.api

/**
 * Keys of data in the `cfg` table.
 */
object CfgKeys {
  object Webapp {
    private def p: String => String = "shipreq.webapp." + _
    def appName  = p("appName")
    def homeUrl  = p("url.home")
    def loginUrl = p("url.login")
  }
}
