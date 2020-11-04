package shipreq.webapp.base.validation

/**
  * Validation and mandatory pre-processing (hereby called correction).
  *
  * Typical usage
  *
  * import shipreq.webapp.base.validation.lib.{CommonValidation => CV, _}
  * import shipreq.webapp.base.validation.lib.Simple._
  * import shipreq.webapp.base.validation.lib.Implicits._
  *
  * =======
  * Generic
  * =======
  *
  *              +---------------+-------------+
  *              | Endomorphic   | Polymorphic |
  * +------------+---------------+-------------+
  * | Correction | EndoCorrector | Corrector   |
  * | Validation | Invalidator   | Auditor     |
  * | Both       | EndoValidator | Validator   |
  * +------------+---------------+-------------+
  *
  * EndoCorrector[A]           - Correction of A => A
  * Corrector    [I, C]        - Correction of Input => Corrected
  * Invalidator  [+E, A]       - A => Option[Error]
  * Auditor      [+E, C, V]    - Corrected => Error \/ Validated
  * EndoValidator[+E, A]       - EndoCorrector & Invalidator
  * Validator    [+E, I, C, V] - Corrector & Auditor
  *
  *
  * ======
  * Simple
  * ======
  *
  * All errors pertain to the same subject.
  *
  * Invalidity = NonEmptySet[String]
  *
  * Importing `Simple._` provides type aliases of all the generic components with E=Invalidity.
  *
  * =========
  * Composite
  * =========
  *
  * All errors are attributed to an optional field.
  * Composite as in it composes validation of multiple subjects.
  *
  * Invalidity      = NonEmptyVector[FieldInvalidity]
  * FieldInvalidity = (field: Option[String], reasons: Simple.Invalidity)
  *
  * Importing `Composite._` provides type aliases of all the generic components with E=Invalidity.
  *
  * There are two additional data types:
  *
  * Composite.Stateless[I, C, V] - Simple validator and a (field/subject) name.
  *                                Call .unnamed for a Simple   .Validator
  *                                Call .named   for a Composite.Validator
  *
  * Composite.Stateful[S, I, C, V] - Validation which depends on external state.
  *                                  S => Composite.Stateless
  *
  *
  * =========
  * Utilities
  * =========
  *
  * CommonValidation - Reusable, low-level logic common to validation.
  * Uniqueness       - Utils that help validate that a subject is unique in some external context
  */
package object lib {
}
