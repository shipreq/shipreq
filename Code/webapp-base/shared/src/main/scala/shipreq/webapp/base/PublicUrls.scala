package shipreq.webapp.base

object PublicUrls {
  def home : String = "/"
  def login: String = "/login"

  /** This is for Lift in webapp-server and will be DCE'd from JS */
  object ForLift {
    private def toLift(s: String): String = {
      val ss = s.replaceFirst("^/", "")
      if (ss.isEmpty) "index" else ss
    }
    def home : String = toLift(PublicUrls.home)
    def login: String = toLift(PublicUrls.login)
  }
}
