This is about the `shipreq.webapp.base.validation` package.


Generic / Primatives
====================

These are all in `object Generic`.

* Low-level
  * `EndoCorrector[A](live: A => A, full: A => A)`
  * `Corrector[I, C](live: I => I, full: I => C, uncorrect: C => I)`
  * `Invalidator[+E, A](invalidate: A => Option[E])`
  * `Auditor[+E, C, V](audit: C => E \/ V)`

* Composite
  * `EndoValidator[+E, A](corrector: EndoCorrector[A], invalidator: Invalidator[E, A])`
  * `Validator[+E, I, C, V](corrector: Corrector[I, C], auditor: Auditor[E, C, V])`


Simple
======

Simple validation means just correction & validation and nothing else.
Basically just data in, result out.

An `import Simple._` provides the following:

  * `type Invalidity = NonEmptySet[String]` such that errors accumulate
  * Everything in `Generic` with all error types set to `Invalidity`

Strings are not accepted as error messages; wrap them in `Invalidity.apply`.


Composite
=========

This allows validators to be assigned to fields, and composed so that you can validate
a high-level data value, and get a breakdown of exactly which parts of it are invalid.

It also allows stateful validation such as ensuring a name is unique given an external set
of names.

An `import Composite._` provides the following:

  * `FieldInvalidity(field: Option[String], reasons: Simple.Invalidity)`
  * `type Invalidity = NonEmptyVector[FieldInvalidity]` such that errors accumulate per field
  * Everything in `Generic` with all error types set to `Invalidity`
  * New data types:
    * `Stateless[I, C, V](unnamed: Simple.Validator[I, C, V], name: String)`
    * `Stateful[S, I, C, V](stateless: Stateless[I, C, V], unnamedFn: S => Simple.Validator[I, C, V])`
    * documentation on both above types in the ScalaDoc
