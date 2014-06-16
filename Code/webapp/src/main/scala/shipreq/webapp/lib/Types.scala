package shipreq.webapp.lib

import net.liftweb.http.js.{JsCmd, JsCmds}
import shipreq.taskman.api.UserId
import scalaz.Monoid
import shipreq.base.util.TaggedTypes._
import shipreq.webapp.db._
import shipreq.webapp.feature.uc.UseCase
import shipreq.webapp.feature.uc.field.FieldValues
import shipreq.webapp.feature.uc.persist.UseCaseSaveCheckpoint
import shipreq.webapp.feature.{ExternalId, Inspection}

/**
 * @since 30/05/2013
 */
object Types {

  // ===================================================================================================================
  // Handy Conversions

  @inline final implicit def uc2fieldValues(uc: UseCase): FieldValues = uc.fieldValues
  @inline final implicit def ucr2ui(u: UseCaseRev): UseCaseIdent = u.ident
  @inline final implicit def cp2uc(u: UseCaseSaveCheckpoint): UseCase = u.uc
  @inline final implicit def cp2ucr(u: UseCaseSaveCheckpoint): UseCaseRev = u.rec
  @inline final implicit def cp2uci(u: UseCaseSaveCheckpoint): UseCaseIdent = u.rec

  // -------------------------------------------------------------------------------------------------------------------
  // Implicits

  implicit class StringGeneralExt(val s: String) extends AnyVal {
    def inspect = Inspection.str.shows(s)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // General tags

  final case class InputCorrected[A](value: A) extends TaggedType {
    type U = A
    def map[B](f: A => B) = InputCorrected[B](f(value))
  }
  implicit def InputCorrectedCtor[R] = TaggedTypeCtor[InputCorrected[R]](InputCorrected[R])

  // -------------------------------------------------------------------------------------------------------------------
  // String tags

  final case class NormalisedText(value: String) extends TaggedString
  implicit object NormalisedText extends TaggedTypeCtor[NormalisedText]

  sealed trait AnyLocalId extends TaggedString

  /** A transient ID used to identify a TextField's . */
  final case class LocalTextFieldId(value: String) extends AnyLocalId
  implicit object LocalTextFieldId extends TaggedTypeCtor[LocalTextFieldId]

  /** A transient ID used to identify a single step node in memory. */
  final case class LocalStepId(value: String) extends AnyLocalId
  implicit object LocalStepId extends TaggedTypeCtor[LocalStepId]

  /** A textual label for a tree node. Eg. "1.0.2.a" */
  final case class StepLabel(value: String) extends TaggedString
  implicit object StepLabel extends TaggedTypeCtor[StepLabel]
  implicit val StepLabelOrdering = implicitly[Ordering[String]].on[StepLabel](_.value)

  /** Marks a string as being an ISO-8601 representation of a datetime. */
  final case class ISO8601(value: String) extends TaggedString
  implicit object ISO8601 extends TaggedTypeCtor[ISO8601]

  /** Marks a password as being hashed. */
  final case class HashedStr(value: String) extends TaggedString
  implicit object HashedStr extends TaggedTypeCtor[HashedStr]

  final case class ShareUrlToken(value: String) extends TaggedString
  implicit object ShareUrlToken extends TaggedTypeCtor[ShareUrlToken]

  final case class Username(value: String) extends TaggedString
  implicit object Username extends TaggedTypeCtor[Username]

  // -------------------------------------------------------------------------------------------------------------------
  // Short tags

  /** Marks a Short value as corresponding to `usecase.number`. */
  final case class UseCaseNumber(value: Short) extends TaggedShort
  implicit object UseCaseNumber extends TaggedTypeCtor[UseCaseNumber]
  @inline final implicit def UcIdentToUcN(u: UseCaseIdent): UseCaseNumber = u.number
  @inline final implicit def UcToUcN(u: UseCase): UseCaseNumber = u.number

  // -------------------------------------------------------------------------------------------------------------------
  // Long tags

  /** Marks a Long value as corresponding to `field_key.id`. */
  final case class FieldKeyId(value: Long) extends TaggedLong
  implicit object FieldKeyId extends TaggedTypeCtor[FieldKeyId]
  @inline final implicit def FieldKeyToId(r: FieldKeyRec): FieldKeyId = r.id

  /** Marks a Long value as corresponding to `usecase_rev.id`. */
  final case class UseCaseRevId(value: Long) extends TaggedLong
  implicit object UseCaseRevId extends TaggedTypeCtor[UseCaseRevId]
  @inline final implicit def UseCaseRevToId(r: UseCaseRev): UseCaseRevId = r.id

  /** Marks a Long value as corresponding to `text.id` and `text_rev.ident_id`. */
  final case class TextIdentId(value: Long) extends TaggedLong
  implicit object TextIdentId extends TaggedTypeCtor[TextIdentId]
  @inline final implicit def TextRevToIdentId(r: TextRev): TextIdentId = r.identId

  final case class ShareId(value: Long) extends TaggedLong
  implicit object ShareId extends TaggedTypeCtor[ShareId]
  @inline final implicit def ShareToId(s: Share): ShareId = s.id
  @inline final implicit def ShareSToId(s: ShareSummary): ShareId = s.id

  /** Marks a Long value as corresponding to `usr.id`. */
  @inline final implicit def UserToId1(a: UserDescriptor): UserId = a.id
  @inline final implicit def UserToId2(a: UserRegistrationInfo): UserId = a.id

  // -------------------------------------------------------------------------------------------------------------------
  // Externalisable ID tags

  sealed trait ExteralisableId extends TaggedLong {
    type E <: TaggedString
  }
  
  final case class X(value: Long) extends ExteralisableId
  implicit object X extends TaggedTypeCtor[X]

  /** Marks a Long value as corresponding to `usecase.id` and `usecase_rev.ident_id`. */
  final case class UseCaseIdentIdE(value: String) extends TaggedString
  final case class UseCaseIdentId(value: Long) extends ExteralisableId {
    override type E = UseCaseIdentIdE
  }
  implicit object UseCaseIdentId extends TaggedTypeCtor[UseCaseIdentId]
  implicit object UseCaseIdentIdE extends TaggedTypeCtor[UseCaseIdentIdE]
  @inline final implicit def BasicUseCaseInfoToIdentId(r: BasicUseCaseInfo): UseCaseIdentId = r.identId
  @inline final implicit def UseCaseIdentToIdentId(i: UseCaseIdent): UseCaseIdentId = i.identId
  @inline final implicit def cp2uid(u: UseCaseSaveCheckpoint): UseCaseIdentId = u.rec

  /** Marks a Long value as corresponding to `text_rev.id`. */
  final case class TextRevIdE(value: String) extends TaggedString
  final case class TextRevId(value: Long) extends ExteralisableId {
    override type E = TextRevIdE
  }
  implicit object TextRevId extends TaggedTypeCtor[TextRevId]
  implicit object TextRevIdE extends TaggedTypeCtor[TextRevIdE]
  @inline final implicit def TextRevToId(r: TextRev): TextRevId = r.id

  /** Marks a Long value as corresponding to `project.id`. */
  final case class ProjectIdE(value: String) extends TaggedString
  final case class ProjectId(value: Long) extends ExteralisableId {
    override type E = ProjectIdE
  }
  implicit object ProjectId extends TaggedTypeCtor[ProjectId]
  implicit object ProjectIdE extends TaggedTypeCtor[ProjectIdE]
  @inline final implicit def p2pid(p: Project): ProjectId = p.id
  @inline final implicit def uci2pid(u: UseCaseIdent): ProjectId = u.projectId
  @inline final implicit def ucr2pid(u: UseCaseRev): ProjectId = u.projectId
  @inline final implicit def cp2pid(u: UseCaseSaveCheckpoint): ProjectId = u.rec

  object AutoExternaliseIds {
    implicit def aei_P (id: ProjectId)     : ProjectIdE      = ExternalId.Project(id)
    implicit def aei_UC(id: UseCaseIdentId): UseCaseIdentIdE = ExternalId.UseCase(id)
    implicit def aei_TR(id: TextRevId)     : TextRevIdE      = ExternalId.TextRev(id)
  }

  // ===================================================================================================================
  // Type class instances

  implicit object JsCmdMonoid extends Monoid[JsCmd] {

    import JsCmds.{_Noop => Noop}

    override def zero = Noop
    override def append(a: JsCmd, b: => JsCmd) =
      if (a eq Noop) b
      else if (b eq Noop) a
      else a & b
  }
}
