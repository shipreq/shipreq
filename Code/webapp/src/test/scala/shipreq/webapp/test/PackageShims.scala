package shipreq.webapp

package db {
  import scala.slick.jdbc.JdbcBackend.Session
  object Shim {
    def newDaoT(s: Session): DaoT = new Dao(s)
    def newAdminDao(s: Session): AdminDao = new AdminDao(s)

    def InsertUsrd = Sql.InsertUsrd
  }
}