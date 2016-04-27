package shipreq.taskman.server.business

import scala.slick.jdbc.JdbcBackend.Session
import shipreq.taskman.api.{EmailAddr, UserId}

object ShipReqInterface {

  class Sql(schema: Option[String]) {
    import scala.slick.jdbc.GetResult
    import scala.slick.jdbc.StaticQuery.{query, queryNA}
    import shipreq.base.db.SqlHelpers._

    implicit val dbCodecUserId    = DbCodec.WithOption.caseClass[UserId]
    implicit val dbCodecEmailAddr = DbCodec.WithOption.caseClass[EmailAddr]

    implicit val GR_ShipReqUser: GetResult[ShipReqUser] =
      GetResult(r => ShipReqUser(r.<<, r.<<, r.<<, r.<<, r.<<))

    // ---------------------------------------------------------------------------------------------

    val prefix = schema.fold("")(_ + ".")

    val findUsersSql = s"select id, username, email, name, newsletter from ${prefix}taskman_users_v01"

    val findUserById = query[UserId, ShipReqUser](findUsersSql + " where id=?")

    val findUserByEmail = query[EmailAddr, ShipReqUser](findUsersSql + " where email=?")

    val findAllUsers = queryNA[ShipReqUser](findUsersSql)

    def findAllUsersW(whereClause: String) = queryNA[ShipReqUser](s"$findUsersSql where ($whereClause)")
  }

  // ===================================================================================================================

  class Dao(sql: Sql)(implicit session: Session) {

    def findUser(id: UserId): Option[ShipReqUser] = sql.findUserById(id).firstOption

    def findUser(e: EmailAddr): Option[ShipReqUser] = sql.findUserByEmail(e).firstOption

    def findAllUsers(): List[ShipReqUser] = sql.findAllUsers.list

    def findAllUsers(cond: String): List[ShipReqUser] = sql.findAllUsersW(cond).list
  }
}