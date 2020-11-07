package shipreq.webapp.server.test

import shipreq.base.test.db.DbTable

object DbTables {
  val GlobalEvent          = DbTable("global_event")
  val ProjectEvent         = DbTable("project_event")
  val Project              = DbTable("project")
  val ProjectAccessPerHour = DbTable("project_access_per_hour")
  val Usr                  = DbTable("usr")
  val UsrLoginLog          = DbTable("usr_login_log")
  val UsrLoginsPerHour     = DbTable("usr_logins_per_hour")
  val Usrd                 = DbTable("usrd")
  val VisitorStatsPerHour  = DbTable("visitor_stats_per_hour")
}
