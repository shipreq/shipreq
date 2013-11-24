package com.beardedlogic.usecase
package db

import util.{ResourceLeaseMonadL, ResourceLeaseMonadR, ResourceLeaseMonad1}

trait DaoProvider {

  def withSession[T](block: DaoS => T): T
  def withTransaction[T](block: DaoT => T): T

  def forSession[M[_]] = new ResourceLeaseMonad1[DaoS, M] {protected override def exec[T](f: DaoS => T): T = withSession(f(_))}
  def forSessionLeft[R, M[_, R]] = new ResourceLeaseMonadL[DaoS, R, M] {protected override def exec[T](f: DaoS => T): T = withSession(f(_))}
  def forSessionRight[L, M[L, _]] = new ResourceLeaseMonadR[DaoS, L, M] {protected override def exec[T](f: DaoS => T): T = withSession(f(_))}

  def forTransaction[M[_]] = new ResourceLeaseMonad1[DaoT, M] {protected override def exec[T](f: DaoT => T): T = withTransaction(f(_))}
  def forTransactionLeft[R, M[_, R]] = new ResourceLeaseMonadL[DaoT, R, M] {protected override def exec[T](f: DaoT => T): T = withTransaction(f(_))}
  def forTransactionRight[L, M[L, _]] = new ResourceLeaseMonadR[DaoT, L, M] {protected override def exec[T](f: DaoT => T): T = withTransaction(f(_))}
}
