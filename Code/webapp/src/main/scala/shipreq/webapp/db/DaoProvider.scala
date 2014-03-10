package shipreq.webapp
package db

import scalaz.Need
import slick.session.Session
import util.ResourceLeaseMonad1

// Trait would be nice but https://issues.scala-lang.org/browse/SI-4767
abstract class DaoProvider {

  def withRawSession[T](f: Session => T): T
  protected def rawSession(): Session
  protected def inTransaction[T](s: Session)(f: Session => T): T = s.withTransaction(f(s))

  protected def newDaoS    (s: Session): DaoS     = new Dao(s)
  protected def newDaoT    (s: Session): DaoT     = new Dao(s)
  protected def newAdminDao(s: Session): AdminDao = new AdminDao(s)

  @inline final def withSession    [T](f: DaoS     => T): T = withRawSession(s => f(newDaoS(s)))
  @inline final def withTransaction[T](f: DaoT     => T): T = withRawSession(inTransaction(_)(s => f(newDaoT(s))))
  @inline final def withAdminDao   [T](f: AdminDao => T): T = withRawSession(s => f(newAdminDao(s)))

  @inline final def forSession    [M[_]] = new ResourceLeaseMonad1[DaoS,     M] {protected override def exec[T](f: DaoS     => T): T = withSession(f(_))}
  @inline final def forTransaction[M[_]] = new ResourceLeaseMonad1[DaoT,     M] {protected override def exec[T](f: DaoT     => T): T = withTransaction(f(_))}
  @inline final def forAdmin      [M[_]] = new ResourceLeaseMonad1[AdminDao, M] {protected override def exec[T](f: AdminDao => T): T = withAdminDao(f(_))}

  def withLazySession[T](f: Need[DaoS] => T): T = {
    var used = false
    val n = Need {used = true; newDaoS(rawSession)}
    try
      f(n)
    finally
      if (used) n.value.session.close()
  }

  /**
   * Starts a transaction with a specified transaction isolation level.
   * @param level See java.sql.Connection
   */
  def withTransactionLevel[R](level: Int)(f: DaoT => R): R =
    withRawSession(s => {
      val conn = s.conn
      val orig = conn.getTransactionIsolation
      try {
        conn setTransactionIsolation level
        inTransaction(s)(ss => f(newDaoT(ss)))
      } finally
        conn setTransactionIsolation orig
    })
}
