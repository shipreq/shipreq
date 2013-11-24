package com.beardedlogic.usecase.util

trait ResourceLeaseFunctor[Resource] {
  protected def exec[T](f: Resource => T): T
  def foreach[T](f: Resource => T): Unit = exec(f(_))
  def map[T](f: Resource => T): T = exec(f(_))
}

trait ResourceLeaseMonad1[Resource, M[_]] extends ResourceLeaseFunctor[Resource] {
  def flatMap[T](f: Resource => M[T]): M[T] = exec(f(_))
}

trait ResourceLeaseMonadR[Resource, L, M[L, _]] extends ResourceLeaseFunctor[Resource] {
  def flatMap[T](f: Resource => M[L, T]): M[L, T] = exec(f(_))
}

trait ResourceLeaseMonadL[Resource, R, M[_, R]] extends ResourceLeaseFunctor[Resource] {
  def flatMap[T](f: Resource => M[T, R]): M[T, R] = exec(f(_))
}
