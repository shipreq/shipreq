package com.beardedlogic.usecase.lib.change

import scalaz.NonEmptyList

object ChangeResult {

  private def nel[C](changes: List[C]) = NonEmptyList.nel(changes.head, changes.tail)

  /**
   * Creates either a NoChange or Changed.
   *
   * @param v The changed value (if changes is non-empty).
   * @param changes Potentially-empty list changes.
   */
  def <~[V, C](v: => V, changes: List[C]): ChangeResult[V, C] =
    if (changes.isEmpty) NoChange
    else Changed(v, nel(changes))

  def map3[A, B, C, H, R](ca: ChangeResult[A, H], cb: ChangeResult[B, H], cc: ChangeResult[C, H])
    (da: => A, db: => B, dc: => C)(f: (A, B, C) => R): ChangeResult[R, H] = {

    val changes = collectChanges(ca, cb, cc)
    if (changes.isEmpty) NoChange
    else {
      val va = ca.getOrElse(da)
      val vb = cb.getOrElse(db)
      val vc = cc.getOrElse(dc)
      val r = f(va, vb, vc)
      Changed(r, nel(changes))
    }
  }

  def collectChanges[C](rs: ChangeResultF[_, C]*): List[C] = {
    var found = List.empty[C]
    for (r <- rs) r match {
      case Changed(_, changes) => found ++= changes.list
      case _ =>
    }
    found
  }
}

/**
 * The result of a potential change, or failure to change.
 */
sealed trait ChangeResultF[+V, +C] {
  def failure: Boolean
  def success: Boolean
  def getChanges: List[C]
  def getOrElse[V2 >: V](b: => V2): V2
  def flatMapF[V2, C2](f: (V, NonEmptyList[C]) => ChangeResultF[V2, C2]): ChangeResultF[V2, C2]
  def map[R](f: V => R): ChangeResultF[R, C]
  def mapChanges[C2](f: NonEmptyList[C] => NonEmptyList[C2]): ChangeResultF[V, C2]
}

/**
 * The result of a potential change. (Cannot fail.)
 */
sealed trait ChangeResult[+V, +C] extends ChangeResultF[V, C] {
  def reusable: Boolean
  override def failure = false
  override def success = true
  override def map[R](f: V => R): ChangeResult[R, C]
  override def mapChanges[C2](f: NonEmptyList[C] => NonEmptyList[C2]): ChangeResult[V, C2]
  def flatMap[V2, C2](f: (V, NonEmptyList[C]) => ChangeResult[V2, C2]): ChangeResult[V2, C2]
}

final case class ChangeFailure(errorMessage: String) extends ChangeResultF[Nothing, Nothing] {
  override def failure = true
  override def success = false
  override def getChanges = List.empty
  override def getOrElse[V2](b: => V2) = b
  override def flatMapF[V2, C2](f: (Nothing, NonEmptyList[Nothing]) => ChangeResultF[V2, C2]) = this
  override def map[R](f: Nothing => R) = this
  override def mapChanges[C2](f: NonEmptyList[Nothing] => NonEmptyList[C2]) = this
}

final case object NoChange extends ChangeResult[Nothing, Nothing] {
  override def reusable = true
  override def getChanges = List.empty
  override def getOrElse[V2](b: => V2) = b
  override def flatMapF[V2, C2](f: (Nothing, NonEmptyList[Nothing]) => ChangeResultF[V2, C2]) = this
  override def flatMap[V2, C2](f: (Nothing, NonEmptyList[Nothing]) => ChangeResult[V2, C2]) = this
  override def map[R](f: Nothing => R) = this
  override def mapChanges[C2](f: NonEmptyList[Nothing] => NonEmptyList[C2]) = this
}

final case class Changed[+V, +C](newValue: V, changes: NonEmptyList[C]) extends ChangeResult[V, C] {
  override def reusable = false
  override def getChanges = changes.list
  override def getOrElse[V2 >: V](b: => V2) = newValue
  override def flatMapF[V2, C2](f: (V, NonEmptyList[C]) => ChangeResultF[V2, C2]) = f(newValue, changes)
  override def flatMap[V2, C2](f: (V, NonEmptyList[C]) => ChangeResult[V2, C2]) = f(newValue, changes)
  override def map[R](f: V => R) = Changed(f(newValue), changes)
  override def mapChanges[C2](f: NonEmptyList[C] => NonEmptyList[C2]) = Changed(newValue, f(changes))
}
