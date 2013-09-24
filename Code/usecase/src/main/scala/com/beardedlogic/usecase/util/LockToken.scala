package com.beardedlogic.usecase.util

/**
 * Type-safe locking.
 *
 * Locks subjects are contravariant.
 * Given a graph [car -> door -> handle], a `Lock[Door]` can be satisfied by a `Lock[Door]` or  a `Lock[Car]`, but not
 * a `Lock[DoorHandle]`.
 *
 * Lock types also distinguish between read/write locks. A write-lock may be used when a read-lock is required, but not
 * the inverse.
 */
object LockToken {

  trait Subject
  // trait Subject2[A <: Subject, B <: Subject] extends Subject
  // trait Subject3[A <: Subject, B <: Subject, C <: Subject] extends Subject
  // trait Subject4[A <: Subject, B <: Subject, C <: Subject, D <: Subject] extends Subject


  private case object InstanceLockR extends LockTokenR[Nothing]
  private case object InstanceLockW extends LockTokenW[Nothing]

  trait Factory {
    final protected def newLockR[S <: Subject]: LockTokenR[S] = InstanceLockR.asInstanceOf[LockTokenR[S]]
    final protected def newLockW[S <: Subject]: LockTokenW[S] = InstanceLockW.asInstanceOf[LockTokenW[S]]
  }

  // trait Implicits {
    // implicit def splitL21[L[-_ <: Subject] <: LockR[_], A <: Subject,B <: Subject](l: L[Subject2[A, B]]): L[A] = l.asInstanceOf[L[A]]
    // implicit def splitL22[L[-_ <: Subject] <: LockR[_], A <: Subject,B <: Subject](l: L[Subject2[A, B]]): L[B] = l.asInstanceOf[L[B]]
    // implicit def splitL31[L[-_ <: Subject] <: LockR[_], A <: Subject,B <: Subject,C <: Subject](l: L[Subject3[A, B, C]]): L[A] = l.asInstanceOf[L[A]]
    // implicit def splitL32[L[-_ <: Subject] <: LockR[_], A <: Subject,B <: Subject,C <: Subject](l: L[Subject3[A, B, C]]): L[B] = l.asInstanceOf[L[B]]
    // implicit def splitL33[L[-_ <: Subject] <: LockR[_], A <: Subject,B <: Subject,C <: Subject](l: L[Subject3[A, B, C]]): L[C] = l.asInstanceOf[L[C]]
    // implicit def splitL41[L[-_ <: Subject] <: LockR[_], A <: Subject,B <: Subject,C <: Subject,D <: Subject](l: L[Subject4[A, B, C, D]]): L[A] = l.asInstanceOf[L[A]]
    // implicit def splitL42[L[-_ <: Subject] <: LockR[_], A <: Subject,B <: Subject,C <: Subject,D <: Subject](l: L[Subject4[A, B, C, D]]): L[B] = l.asInstanceOf[L[B]]
    // implicit def splitL43[L[-_ <: Subject] <: LockR[_], A <: Subject,B <: Subject,C <: Subject,D <: Subject](l: L[Subject4[A, B, C, D]]): L[C] = l.asInstanceOf[L[C]]
    // implicit def splitL44[L[-_ <: Subject] <: LockR[_], A <: Subject,B <: Subject,C <: Subject,D <: Subject](l: L[Subject4[A, B, C, D]]): L[D] = l.asInstanceOf[L[D]]
  // }
  // object Implicits extends Implicits
}

/** Read lock token. */
sealed trait LockTokenR[-S <: LockToken.Subject]

/** Write lock token. */
sealed trait LockTokenW[-S <: LockToken.Subject] extends LockTokenR[S]
