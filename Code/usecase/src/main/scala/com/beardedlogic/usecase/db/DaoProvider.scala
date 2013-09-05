package com.beardedlogic.usecase
package db

import util.{ResourceLeaseMonadL, ResourceLeaseMonadR, ResourceLeaseMonad1}

trait DaoProvider {

  def get: DAO
  def withSession[T](block: DAO => T): T
  def withTransaction[T](block: DAO => T): T

  def withInstance[T](transaction: Boolean)(block: DAO => T): T = {
    if (transaction) withTransaction(block) else withSession(block)
  }

  def forSession[M[_]] = new ResourceLeaseMonad1[DAO, M] {protected override def exec[T](f: DAO => T): T = withSession(f(_))}
  def forSessionLeft[R, M[_, R]] = new ResourceLeaseMonadL[DAO, R, M] {protected override def exec[T](f: DAO => T): T = withSession(f(_))}
  def forSessionRight[L, M[L, _]] = new ResourceLeaseMonadR[DAO, L, M] {protected override def exec[T](f: DAO => T): T = withSession(f(_))}

  def forTransaction[M[_]] = new ResourceLeaseMonad1[DAO, M] {protected override def exec[T](f: DAO => T): T = withTransaction(f(_))}
  def forTransactionLeft[R, M[_, R]] = new ResourceLeaseMonadL[DAO, R, M] {protected override def exec[T](f: DAO => T): T = withTransaction(f(_))}
  def forTransactionRight[L, M[L, _]] = new ResourceLeaseMonadR[DAO, L, M] {protected override def exec[T](f: DAO => T): T = withTransaction(f(_))}
}

