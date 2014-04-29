package shipreq.taskman.server.business

import scala.slick.session.Session
import shipreq.taskman.api.Types._

object ShipReqInterface {

  class Sql(schema: Option[String]) {

    import scala.slick.jdbc.GetResult
    import scala.slick.jdbc.StaticQuery.query
    import shipreq.base.db.SqlHelpers._

    implicit val GR_UserId = GR_TaggedLong[UserId]
    implicit val SP_UserId = SP_TaggedLong[UserId]

    implicit val GR_EmailAddr = GR_TaggedString[EmailAddr]
    implicit val SP_EmailAddr = SP_TaggedString[EmailAddr]

    implicit val GR_ShipReqUser = GetResult(r => ShipReqUser(r.<<, r.<<, r.<<, r.<<, r.<<))

    // ---------------------------------------------------------------------------------------------

    val prefix = schema.map(_ + ".") getOrElse ""

    val userQuery = s"select id, username, email, name, newsletter from ${prefix}taskman_users_v01"

    val userQueryById = query[UserId, ShipReqUser](userQuery + " where id=?")
    val userQueryByEmail = query[EmailAddr, ShipReqUser](userQuery + " where email=?")
  }

  // ===================================================================================================================

  class Dao(sql: Sql)(implicit session: Session) {

    def userQueryById(id: UserId): Option[ShipReqUser] =
      sql.userQueryById.firstOption(id)

    def userQueryByEmail(e: EmailAddr): Option[ShipReqUser] =
      sql.userQueryByEmail.firstOption(e)
  }
}