package com.beardedlogic.usecase
package lib

import com.google.common.collect.MapMaker
import java.util.concurrent.locks.{Lock, ReentrantReadWriteLock}
import java.util.concurrent.{TimeoutException, TimeUnit}
import net.liftweb.common.Logger
import model.DAO

object Locks {
  /** R/W locks keyed by use case data id. */
  val UseCase = new LockManager
}

class LockManager extends Logger {

  private class Fn extends com.google.common.base.Function[Long, ReentrantReadWriteLock] {
    override def apply(key: Long) = new ReentrantReadWriteLock
    override def equals(that: Any) = false
  }

  private val lockMap = new MapMaker()
                        .concurrencyLevel(32)
                        .initialCapacity(0x1000)
                        .weakKeys()
                        .weakValues()
                        .makeComputingMap[Long, ReentrantReadWriteLock](new Fn)

  @inline final protected def getLock(id: Long): ReentrantReadWriteLock = lockMap.get(id)

  //  @inline private def withLock[U](lock: Lock)(block: => U): U = {
  //    lock.lock
  //    try block finally lock.unlock
  //  }

  @inline private def withLock[U](lock: Lock)(block: => U): U = {
    if (!lock.tryLock(30, TimeUnit.SECONDS)) throw new TimeoutException()
    try block finally lock.unlock
  }

  final def withReadLock[U](id: Long)(block: => U): U = withLock(getLock(id).readLock)(block)
  final def withWriteLock[U](id: Long)(block: => U): U = withLock(getLock(id).writeLock)(block)

  @inline final def withReadLockAndTransaction[U](id: Long, dao: DAO)(block: => U): U = withReadLock(id)(dao.withTransaction(block))
  @inline final def withWriteLockAndTransaction[U](id: Long, dao: DAO)(block: => U): U = withWriteLock(id)(dao.withTransaction(block))
}
