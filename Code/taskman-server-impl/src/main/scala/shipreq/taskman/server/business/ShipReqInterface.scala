package shipreq.taskman.server.business

import scala.slick.jdbc.JdbcBackend.Session
import shipreq.taskman.api.Types._

object ShipReqInterface {

  class Sql(schema: Option[String]) {

    import scala.slick.jdbc.GetResult
    import scala.slick.jdbc.StaticQuery.{query, queryNA}
    import shipreq.base.db.SqlHelpers._

    implicit val GR_UserId = GR_TaggedLong[UserId]
    implicit val SP_UserId = SP_TaggedLong[UserId]

    implicit val GR_EmailAddr = GR_TaggedString[EmailAddr]
    implicit val SP_EmailAddr = SP_TaggedString[EmailAddr]

    implicit val GR_ShipReqUser = GetResult(r => ShipReqUser(r.<<, r.<<, r.<<, r.<<, r.<<))

    // ---------------------------------------------------------------------------------------------

    val prefix = schema.map(_ + ".") getOrElse ""

    val findUsersSql = s"select id, username, email, name, newsletter from ${prefix}taskman_users_v01"

    val findUserById = query[UserId, ShipReqUser](findUsersSql + " where id=?")

    val findUserByEmail = query[EmailAddr, ShipReqUser](findUsersSql + " where email=?")

    val findAllUsers = queryNA[ShipReqUser](findUsersSql)

    def findAllUsersW(whereClause: String) = queryNA[ShipReqUser](s"$findUsersSql where ($whereClause)")
  }

  // ===================================================================================================================

  class Dao(sql: Sql)(implicit session: Session) {

    def findUser(id: UserId): Option[ShipReqUser] = sql.findUserById.firstOption(id)

    def findUser(e: EmailAddr): Option[ShipReqUser] = sql.findUserByEmail.firstOption(e)

    def findAllUsers(): List[ShipReqUser] = sql.findAllUsers.list()

    def findAllUsers(cond: String): List[ShipReqUser] = sql.findAllUsersW(cond).list()
  }
}