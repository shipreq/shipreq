package shipreq.webapp.server.test

import shipreq.base.test.db.DbTable

object DbTables {
  val Event                = DbTable("event")
  val Project              = DbTable("project")
  val ProjectAccessPerHour = DbTable("project_access_per_hour")
  val Usr                  = DbTable("usr")
  val UsrLoginLog          = DbTable("usr_login_log")
  val UsrLoginsPerHour     = DbTable("usr_logins_per_hour")
  val Usrd                 = DbTable("usrd")
  val UsrhName             = DbTable("usrh_name")
  val VisitorStatsPerHour  = DbTable("visitor_stats_per_hour")
}
