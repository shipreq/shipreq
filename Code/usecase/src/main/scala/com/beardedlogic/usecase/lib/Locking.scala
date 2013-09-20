package com.beardedlogic.usecase
package lib

import com.google.common.cache.{CacheBuilder, CacheLoader}
import java.util.concurrent.locks.{Lock, ReentrantReadWriteLock}
import java.util.concurrent.{TimeoutException, TimeUnit}
import java.lang.{Long => JLong}
import net.liftweb.common.Logger
import Locks._
import Types._
import util.{ResourceLeaseMonadL, ResourceLeaseMonadR, ResourceLeaseMonad1}

object Locks {
  /** R/W locks keyed by use case data id. */
  val UseCase = new LockManager[UseCaseIdentId]

  sealed trait ReadLockToken
  sealed trait WriteLockToken extends ReadLockToken
}

class LockManager[LockKey <: JLong] extends Logger {

  private val ReadLockToken = new ReadLockToken{}
  private val WriteLockToken = new WriteLockToken{}

  private class Fn extends CacheLoader[JLong, ReentrantReadWriteLock] {
    override def load(key: JLong) = new ReentrantReadWriteLock
  }

  private val lockCache = CacheBuilder.newBuilder()
                          .concurrencyLevel(32)
                          .initialCapacity(0x1000)
                          .weakValues()
                          .build(new Fn)

  @inline final protected def getLock(id: LockKey): ReentrantReadWriteLock = lockCache.get(id)

  @inline private def withLock[U](lock: Lock)(block: => U): U = {
    if (!lock.tryLock(30, TimeUnit.SECONDS)) throw new TimeoutException()
    try block finally lock.unlock
  }

  final def withReadLock[U](id: LockKey)(block: => U): U = withLock(getLock(id).readLock)(block)
  final def withWriteLock[U](id: LockKey)(block: => U): U = withLock(getLock(id).writeLock)(block)

  final def withReadLockToken[U](id: LockKey)(block:ReadLockToken => U): U = withLock(getLock(id).readLock)(block(ReadLockToken))
  final def withWriteLockToken[U](id: LockKey)(block:WriteLockToken => U): U = withLock(getLock(id).writeLock)(block(WriteLockToken))

//  @inline final def withReadLockAndTransaction[U](id: LockKey, dao: Dao)(block: ReadLockToken => U): U = withReadLock(id)(dao.withTransaction(block))
//  @inline final def withWriteLockAndTransaction[U](id: LockKey, dao: Dao)(block: WriteLockToken => U): U = withWriteLock(id)(dao.withTransaction(block))

  def forRead[M[_]](id: LockKey) = new ResourceLeaseMonad1[ReadLockToken, M] {protected override def exec[T](f: ReadLockToken => T): T = withReadLockToken(id)(f(_))}
  def forReadLeft[R, M[_, R]](id: LockKey) = new ResourceLeaseMonadL[ReadLockToken, R, M] {protected override def exec[T](f: ReadLockToken => T): T = withReadLockToken(id)(f(_))}
  def forReadRight[L, M[L, _]](id: LockKey) = new ResourceLeaseMonadR[ReadLockToken, L, M] {protected override def exec[T](f: ReadLockToken => T): T = withReadLockToken(id)(f(_))}

  def forWrite[M[_]](id: LockKey) = new ResourceLeaseMonad1[WriteLockToken, M] {protected override def exec[T](f: WriteLockToken => T): T = withWriteLockToken(id)(f(_))}
  def forWriteLeft[R, M[_, R]](id: LockKey) = new ResourceLeaseMonadL[WriteLockToken, R, M] {protected override def exec[T](f: WriteLockToken => T): T = withWriteLockToken(id)(f(_))}
  def forWriteRight[L, M[L, _]](id: LockKey) = new ResourceLeaseMonadR[WriteLockToken, L, M] {protected override def exec[T](f: WriteLockToken => T): T = withWriteLockToken(id)(f(_))}
}
