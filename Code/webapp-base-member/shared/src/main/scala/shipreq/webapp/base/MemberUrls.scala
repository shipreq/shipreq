package shipreq.webapp.base

import shipreq.webapp.base.data.{ExternalId, Project}

object MemberUrls {
  def home                    : String = "/"
  def project(id: Project.XId): String = "/project/" + id.value
  def logout                  : String = "/logout"

  /** This is for Lift in webapp-server and will be DCE'd from JS */
  object ForLift {
    private def toLift(s: String): String = {
      val ss = s.replaceFirst("^/", "")
      if (ss.isEmpty) "index" else ss
    }
    def home   : String = toLift(MemberUrls.home)
    def project: String = toLift(MemberUrls.project(ExternalId(""))).replaceFirst("/.*", "")
    def logout : String = toLift(MemberUrls.logout)
  }
}
