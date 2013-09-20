package com.beardedlogic.usecase
package db

import util.{ResourceLeaseMonadL, ResourceLeaseMonadR, ResourceLeaseMonad1}

trait DaoProvider {

  def get: Dao
  def withSession[T](block: Dao => T): T
  def withTransaction[T](block: Dao => T): T

  def withInstance[T](transaction: Boolean)(block: Dao => T): T = {
    if (transaction) withTransaction(block) else withSession(block)
  }

  def forSession[M[_]] = new ResourceLeaseMonad1[Dao, M] {protected override def exec[T](f: Dao => T): T = withSession(f(_))}
  def forSessionLeft[R, M[_, R]] = new ResourceLeaseMonadL[Dao, R, M] {protected override def exec[T](f: Dao => T): T = withSession(f(_))}
  def forSessionRight[L, M[L, _]] = new ResourceLeaseMonadR[Dao, L, M] {protected override def exec[T](f: Dao => T): T = withSession(f(_))}

  def forTransaction[M[_]] = new ResourceLeaseMonad1[Dao, M] {protected override def exec[T](f: Dao => T): T = withTransaction(f(_))}
  def forTransactionLeft[R, M[_, R]] = new ResourceLeaseMonadL[Dao, R, M] {protected override def exec[T](f: Dao => T): T = withTransaction(f(_))}
  def forTransactionRight[L, M[L, _]] = new ResourceLeaseMonadR[Dao, L, M] {protected override def exec[T](f: Dao => T): T = withTransaction(f(_))}
}

