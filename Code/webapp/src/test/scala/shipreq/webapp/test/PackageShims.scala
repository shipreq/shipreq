package shipreq.webapp

package db {
  import slick.session.Session
  object Shim {
    def newDaoT(s: Session): DaoT = new Dao(s)
    def newAdminDao(s: Session): AdminDao = new AdminDao(s)
  }
}