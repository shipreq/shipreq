package com.beardedlogic.usecase
package util

import java.lang.{Long => JLong}
import java.util.concurrent.locks.ReentrantReadWriteLock
import org.scalatest.FunSuite
import org.scalatest.Matchers
import LockProvider._

class LockingTest extends FunSuite with Matchers {

  trait X extends LockToken
  trait A extends LockToken
  trait B extends A
  trait C extends B

  class TestLockManager extends DefaultLockProvider[JLong, B, Ø, Ø, B] {
    def getLock_(id: Long): ReentrantReadWriteLock = super.getRealLock(id)
  }

  test("Same lock used for same ID") {
    val t = new TestLockManager
    val a = t.getLock_(123)
    val b = runInAnotherThread(t.getLock_(123))
    a should be theSameInstanceAs(b)
  }

  test("Different locks used for different IDs") {
    val t = new TestLockManager
    val a = t.getLock_(123)
    val b = t.getLock_(456)
    a should not be theSameInstanceAs(b)
  }

  test("Write locks should be exclusive") {
    val t = new TestLockManager
    val a = t.getLock_(123)
    val b = t.getLock_(123)
    a should be theSameInstanceAs(b)
    a.writeLock.lock
    runInAnotherThread(b.writeLock.tryLock) should be(false)
    a.writeLock.unlock
    runInAnotherThread(b.writeLock.tryLock) should be(true)
  }

  def runInAnotherThread[U](fn: => U): U = {
    var u: Option[U] = None
    val t = new Thread() {
      override def run() { u = Some(fn)}
    }
    t.start
    t.join
    u.get
  }

  test("withWriteLock") {
    val t = new TestLockManager
    val x = t.write(123){_ =>
      runInAnotherThread(t.getLock_(123).writeLock.tryLock) should be(false)
      666
    }
    x should be(666)
  }

  /*
  test_a(lockInstance[A, Ø])
  test_a(lockInstance[B, Ø])
  test_a(lockInstance[C, Ø])

  test_a(lockInstance[A with X, Ø])
  test_a(lockInstance[B with X, Ø])
  test_a(lockInstance[C with X, Ø])

  test_c(lockInstance[A, Ø])
  test_c(lockInstance[B, Ø])
  test_c(lockInstance[C, Ø])

  test_c(lockInstance[A with X, Ø])
  test_c(lockInstance[B with X, Ø])
  test_c(lockInstance[C with X, Ø])

  def test_a(l: Lock.Read[A]) = true
  def test_c(l: Lock.Read[C]) = true
  */
}
