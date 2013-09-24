package com.beardedlogic.usecase
package lib

import com.google.common.cache.{CacheBuilder, CacheLoader}
import java.util.concurrent.locks.{Lock, ReentrantReadWriteLock}
import java.util.concurrent.{TimeoutException, TimeUnit}
import java.lang.{Long => JLong}
import net.liftweb.common.Logger

import Types._
import util.{LockToken, LockTokenR, LockTokenW, ResourceLeaseMonad1}

object Locks {
  /** R/W locks keyed by use case data id. */
  val useCase = new RWLockManager[UseCaseIdentId, UseCase]
}

class RWLockManager[LockKey <: JLong, S <: LockToken.Subject] private[lib]() extends LockToken.Factory with Logger {

  private class LockCreator extends CacheLoader[JLong, ReentrantReadWriteLock] {
    override def load(key: JLong) = new ReentrantReadWriteLock
  }

  private val lockCache = CacheBuilder.newBuilder()
                          .concurrencyLevel(32)
                          .initialCapacity(0x1000)
                          .weakValues()
                          .build(new LockCreator)

  @inline protected def getLock(id: LockKey): ReentrantReadWriteLock =
    lockCache.get(id)

  @inline private def useLock[R, Token <: LockTokenR[S]](lock: Lock, token: Token)(block: Token => R): R = {
    if (!lock.tryLock(30, TimeUnit.SECONDS)) throw new TimeoutException()
    try block(token) finally lock.unlock
  }

  def read[U](id: LockKey)(block: LockTokenR[S] => U): U =
    useLock(getLock(id).readLock, newLockR)(block)

  def write[U](id: LockKey)(block: LockTokenW[S] => U): U =
    useLock(getLock(id).writeLock, newLockW)(block)

  def readM[M[_]](id: LockKey): ResourceLeaseMonad1[LockTokenR[S], M] =
    new ResourceLeaseMonad1[LockTokenR[S], M] {
      protected override def exec[T](f: LockTokenR[S] => T): T = read(id)(f(_))
    }

  def writeM[M[_]](id: LockKey): ResourceLeaseMonad1[LockTokenW[S], M] =
    new ResourceLeaseMonad1[LockTokenW[S], M] {
      protected override def exec[T](f: LockTokenW[S] => T): T = write(id)(f(_))
    }
}
