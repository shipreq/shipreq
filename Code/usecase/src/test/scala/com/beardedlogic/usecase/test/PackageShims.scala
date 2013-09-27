package com.beardedlogic.usecase

package db {
  import slick.session.Session
  object Shim {
    def newDaoT(s: Session): DaoT = new Dao(s)
  }
}