package shipreq.webapp.server.test

import japgolly.univeq.UnivEq
import shipreq.base.util.{NonEmptyVector, UtilMacros}

sealed abstract class DbTable(val name: String) {
  override def toString = name
}

object DbTable {
  case object Project     extends DbTable("project")
  case object Usr         extends DbTable("usr")
  case object UsrLoginLog extends DbTable("usr_login_log")
  case object Usrd        extends DbTable("usrd")
  case object UsrhName    extends DbTable("usrh_name")

  //val All = UtilMacros.adtValues[DbTable]
  val All = NonEmptyVector[DbTable](Project, Usr, UsrLoginLog, Usrd, UsrhName)

  implicit def univEq: UnivEq[DbTable] = UnivEq.derive
}
