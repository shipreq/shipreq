package shipreq.webapp.feature.uc.change

import scalaz.NonEmptyList
import shipreq.webapp.feature.validation.{ValidationResult, VFailure}

object ChangeResult {

  def map3[A, B, C, H, R](ca: ChangeResult[A, H], cb: ChangeResult[B, H], cc: ChangeResult[C, H])
    (da: => A, db: => B, dc: => C)(f: (A, B, C) => R): ChangeResult[R, H] = {

    val changes = collectChanges(ca, cb, cc)
    if (changes.isEmpty) NoChange
    else {
      val va = ca.getValueOrElse(da)
      val vb = cb.getValueOrElse(db)
      val vc = cc.getValueOrElse(dc)
      val r = f(va, vb, vc)
      Changed(r, NonEmptyList.nel(changes.head, changes.tail))
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

  def apply[V, C](newValue: Option[V], changes: NonEmptyList[C]): ChangeResult[V, C] =
    newValue match {
      case None    => NoChange
      case Some(v) => Changed(v, changes)
    }

  def apply[V, C](newValue: => V, changes: List[C]): ChangeResult[V, C] =
    changes match {
      case Nil    => NoChange
      case h :: t => Changed(newValue, NonEmptyList.nel(h, t))
    }

  def fromValidation[VV, V, C](r: ValidationResult[VV])(onSuccess: VV => ChangeResultF[V, C]): ChangeResultF[V, C] =
    r match {
      case scalaz.Failure(f) => ChangeFailure(f)
      case scalaz.Success(v) => onSuccess(v)
    }
}

/**
 * The result of a potential change, or failure to change.
 */
sealed trait ChangeResultF[+V, +C] {
  def isFailure: Boolean

  def getValue: Option[V]
  def getValueOrElse[VV >: V](other: => VV): VV
  def getChanges: List[C]
  def getChangesOrElse[CC >: C](other: NonEmptyList[CC]): NonEmptyList[CC]

  def mapF[A, B]        (f: (V,NonEmptyList[C]) => (A,NonEmptyList[B])): ChangeResultF[A, B]
  def flatMapF[A, B]    (f: (V,NonEmptyList[C]) => ChangeResultF[A, B]): ChangeResultF[A, B]
  def mapValueF[A]      (f: V                   => A                  ): ChangeResultF[A, C]
  def flatMapValueF[A]  (f: V                   => Option[A]          ): ChangeResultF[A, C]
  def mapChangesF[B]    (f: NonEmptyList[C]     => NonEmptyList[B]    ): ChangeResultF[V, B]
  def flatMapChangesF[B](f: NonEmptyList[C]     => List[B]            ): ChangeResultF[V, B]
  def mapEachChangeF[B] (f: C                   => B                  ): ChangeResultF[V, B] = mapChangesF(_ map f)

  /** NOTE: This is not commutative. When two changed values are encountered, `that`'s is chosen over `this`'s. */
  def appendF[VV >: V, CC >: C](that: ChangeResultF[VV, CC]): ChangeResultF[VV, CC] =
    (this, that) match {
      case (a@ChangeFailure(_), _) => a
      case (_, b@ChangeFailure(_)) => b
      case (a, NoChange) => a
      case (NoChange, b) => b
      case (Changed(_, c1), Changed(v, c2)) => Changed(v, c1 append c2)
    }

  def andThenF[VV >: V, CC >: C](defaultValue: => VV, f: VV => ChangeResultF[VV, CC]): ChangeResultF[VV, CC] =
    appendF(f(getValueOrElse(defaultValue)))
}

/**
 * The result of a potential change. (Cannot fail.)
 */
sealed trait ChangeResult[+V, +C] extends ChangeResultF[V, C] {
  override def isFailure = false
  def map[A, B]        (f: (V,NonEmptyList[C]) => (A,NonEmptyList[B])): ChangeResult[A, B]
  def flatMap[A, B]    (f: (V,NonEmptyList[C]) => ChangeResult[A, B] ): ChangeResult[A, B]
  def mapValue[A]      (f: V                   => A                  ): ChangeResult[A, C]
  def flatMapValue[A]  (f: V                   => Option[A]          ): ChangeResult[A, C]
  def mapChanges[B]    (f: NonEmptyList[C]     => NonEmptyList[B]    ): ChangeResult[V, B]
  def flatMapChanges[B](f: NonEmptyList[C]     => List[B]            ): ChangeResult[V, B]
  def mapEachChange[B] (f: C                   => B                  ): ChangeResult[V, B] = mapChanges(_ map f)

  /** NOTE: This is not commutative. When two changed values are encountered, `that`'s is chosen over `this`'s. */
  def append[VV >: V, CC >: C](that: ChangeResult[VV, CC]): ChangeResult[VV, CC] =
    (this, that) match {
      case (a, NoChange) => a
      case (NoChange, b) => b
      case (Changed(_, c1), Changed(v, c2)) => Changed(v, c1 append c2)
    }

  def andThen[VV >: V, CC >: C](defaultValue: => VV, f: VV => ChangeResult[VV, CC]): ChangeResult[VV, CC] =
    append(f(getValueOrElse(defaultValue)))
}

trait NoChangeOrFailure {
  this: ChangeResultF[Nothing, Nothing] =>
  type V = Nothing
  type C = Nothing

  override def getValue = None
  override def getValueOrElse[VV >: V](other: => VV) = other
  override def getChanges = List.empty
  override def getChangesOrElse[CC >: C](other: NonEmptyList[CC]) = other

  override def mapF[A, B]        (f: (V,NonEmptyList[C]) => (A,NonEmptyList[B])) = this
  override def flatMapF[A, B]    (f: (V,NonEmptyList[C]) => ChangeResultF[A, B]) = this
  override def mapValueF[A]      (f: V                   => A                  ) = this
  override def flatMapValueF[A]  (f: V                   => Option[A]          ) = this
  override def mapChangesF[B]    (f: NonEmptyList[C]     => NonEmptyList[B]    ) = this
  override def flatMapChangesF[B](f: NonEmptyList[C]     => List[B]            ) = this
}

/**
 * Indicates an error occurred attempting to perform a change.
 */
final case class ChangeFailure(failure: VFailure) extends ChangeResultF[Nothing, Nothing] with NoChangeOrFailure {
  override def isFailure = true
}

/**
 * Indicates that the change process completed without having an effect.
 */
final case object NoChange extends ChangeResult[Nothing, Nothing] with NoChangeOrFailure {
  override def map[A, B]        (f: (V,NonEmptyList[C]) => (A,NonEmptyList[B])) = this
  override def flatMap[A, B]    (f: (V,NonEmptyList[C]) => ChangeResult[A, B] ) = this
  override def mapValue[A]      (f: V                   => A                  ) = this
  override def flatMapValue[A]  (f: V                   => Option[A]          ) = this
  override def mapChanges[B]    (f: NonEmptyList[C]     => NonEmptyList[B]    ) = this
  override def flatMapChanges[B](f: NonEmptyList[C]     => List[B]            ) = this
}

/**
 * Indicates that one or more changes were performed successfully.
 */
final case class Changed[+V, +C](value: V, changes: NonEmptyList[C]) extends ChangeResult[V, C] {
  override def getValue = Some(value)
  override def getValueOrElse[VV >: V](other: => VV) = value
  override def getChanges = changes.list
  override def getChangesOrElse[CC >: C](other: NonEmptyList[CC]) = changes

  override def map[A, B]        (f: (V,NonEmptyList[C]) => (A,NonEmptyList[B])) = {val r=f(value, changes); Changed(r._1, r._2)}
  override def flatMap[A, B]    (f: (V,NonEmptyList[C]) => ChangeResult[A, B] ) = f(value, changes)
  override def mapValue[A]      (f: V                   => A                  ) = Changed(f(value), changes)
  override def flatMapValue[A]  (f: V                   => Option[A]          ) = ChangeResult(f(value), changes)
  override def mapChanges[B]    (f: NonEmptyList[C]     => NonEmptyList[B]    ) = Changed(value, f(changes))
  override def flatMapChanges[B](f: NonEmptyList[C]     => List[B]            ) = ChangeResult(value, f(changes))

  override def mapF[A, B]        (f: (V,NonEmptyList[C]) => (A,NonEmptyList[B])) = map(f)
  override def flatMapF[A, B]    (f: (V,NonEmptyList[C]) => ChangeResultF[A, B]) = f(value, changes)
  override def mapValueF[A]      (f: V                   => A                  ) = mapValue(f)
  override def flatMapValueF[A]  (f: V                   => Option[A]          ) = flatMapValue(f)
  override def mapChangesF[B]    (f: NonEmptyList[C]     => NonEmptyList[B]    ) = mapChanges(f)
  override def flatMapChangesF[B](f: NonEmptyList[C]     => List[B]            ) = flatMapChanges(f)
}
