package shipreq.taskman.server.business

import doobie.imports._
import shipreq.base.db.SqlHelpers._
import shipreq.taskman.api.{EmailAddr, UserId}
import shipreq.taskman.server.logic.business.ShipReqUser

final case class ShipReqInterface(schema: Option[String]) {

  private val prefix = schema.fold("")(_ + ".")

  private implicit val doobieMetaUserId    = doobieMetaCaseClass[UserId]
  private implicit val doobieMetaEmailAddr = doobieMetaCaseClass[EmailAddr]

  private implicit val compositeShipReqUser: Composite[ShipReqUser] = Composite.generic

  private val find = s"select id, username, email, name, newsletter from ${prefix}taskman_users_v01"

  val findUserById: UserId => ConnectionIO[Option[ShipReqUser]] = {
    val q = Query[UserId, ShipReqUser](s"$find where id=?")
    q.toQuery0(_).option
  }

  val findUserByEmail: EmailAddr => ConnectionIO[Option[ShipReqUser]] = {
    val q = Query[EmailAddr, ShipReqUser](s"$find where email=?")
    q.toQuery0(_).option
  }

  val findAllUsers: ConnectionIO[List[ShipReqUser]] =
    Query0[ShipReqUser](find).list

  def findAllUsersW(whereClause: String): ConnectionIO[List[ShipReqUser]] =
    Query0[ShipReqUser](s"$find where ($whereClause)").list
}