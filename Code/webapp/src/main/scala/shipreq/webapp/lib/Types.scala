package shipreq.webapp.lib

import net.liftweb.common.Box
import net.liftweb.http.js.{JsCmd, JsCmds}
import scalaz.Monoid
import shipreq.webapp.db._
import shipreq.webapp.feature.uc.UseCase
import shipreq.webapp.feature.uc.field.FieldValues
import shipreq.webapp.feature.uc.persist.UseCaseSaveCheckpoint
import shipreq.webapp.feature.{ExternalId, Inspection}

/**
 * @since 30/05/2013
 */
object Types extends shipreq.taskman.api.Types {

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
    def asLocalStepId = s.asInstanceOf[LocalStepId]
    def asLabel = s.asInstanceOf[StepLabel]
    def inspect = Inspection.str.shows(s)
  }

  implicit class StringTypeTagExt2[F[_]](val s: F[String]) extends AnyVal {
    def asLocalStepIdC = s.asInstanceOf[F[LocalStepId]]
    def asLabelC = s.asInstanceOf[F[StepLabel]]
  }

  implicit class StringTypeTagExt3[G[_], F[G]](val s: F[G[String]]) extends AnyVal {
    def asLocalStepIdC = s.asInstanceOf[F[G[LocalStepId]]]
    def asLabelC = s.asInstanceOf[F[G[StepLabel]]]
  }

  implicit class SLongBoxTypeTagExt(val x: Box[scala.Long]) extends AnyVal {
    def tag[T <: TypeTag[JLong]] = x.map(_.tag[T])
  }

  implicit class ShortBoxTypeTagExt(val x: Box[scala.Short]) extends AnyVal {
    def tag[T <: TypeTag[JShort]] = x.map(_.tag[T])
  }

  // -------------------------------------------------------------------------------------------------------------------
  // General tags

  sealed trait InputCorrected extends TypeTag[AnyRef]

  sealed trait Validated extends InputCorrected

  // -------------------------------------------------------------------------------------------------------------------
  // String tags

  sealed trait IsNormalised extends TypeTag[String]
  type NormalisedText = String @@ IsNormalised

  sealed trait IsLocalId extends TypeTag[String]
  type AnyLocalId = String @@ IsLocalId

  /** A transient ID used to identify a TextField's . */
  sealed trait IsLocalTextFieldId extends IsLocalId
  type LocalTextFieldId = String @@ IsLocalTextFieldId

  /** A transient ID used to identify a single step node in memory. */
  sealed trait IsLocalStepId extends IsLocalId
  type LocalStepId = String @@ IsLocalStepId

  /** A textual label for a tree node. Eg. "1.0.2.a" */
  sealed trait IsStepLabel extends TypeTag[String]
  type StepLabel = String @@ IsStepLabel

  /** Marks a string as being an ISO-8601 representation of a datetime. */
  sealed trait ISO8601 extends TypeTag[String]

  /** Marks a password as being hashed. */
  sealed trait Hashed extends TypeTag[String]

  sealed trait IsShareUrlToken extends TypeTag[String]
  type ShareUrlToken = String @@ IsShareUrlToken

  // -------------------------------------------------------------------------------------------------------------------
  // Short tags

  /** Marks a Short value as corresponding to `usecase.number`. */
  sealed trait IsUseCaseNumber extends TypeTag[JShort]
  type UseCaseNumber = JShort @@ IsUseCaseNumber
  @inline final implicit def UcIdentToUcN(u: UseCaseIdent): UseCaseNumber = u.number
  @inline final implicit def UcToUcN(u: UseCase): UseCaseNumber = u.number

  // -------------------------------------------------------------------------------------------------------------------
  // Long tags

  /** Marks a Long value as corresponding to `field_key.id`. */
  sealed trait IsFieldKeyId extends TypeTag[JLong]
  type FieldKeyId = JLong @@ IsFieldKeyId
  @inline final implicit def FieldKeyToId(r: FieldKeyRec): FieldKeyId = r.id

  /** Marks a Long value as corresponding to `usecase_rev.id`. */
  sealed trait IsUseCaseRevId extends TypeTag[JLong]
  type UseCaseRevId = JLong @@ IsUseCaseRevId
  @inline final implicit def UseCaseRevToId(r: UseCaseRev): UseCaseRevId = r.id

  /** Marks a Long value as corresponding to `text.id` and `text_rev.ident_id`. */
  sealed trait IsTextIdentId extends TypeTag[JLong]
  type TextIdentId = JLong @@ IsTextIdentId
  @inline final implicit def TextRevToIdentId(r: TextRev): TextIdentId = r.identId

  /** Marks a Long value as corresponding to `usr.id`. */
  @inline final implicit def UserToId1(a: UserDescriptor): UserId = a.id
  @inline final implicit def UserToId2(a: UserRegistrationInfo): UserId = a.id

  sealed trait IsShareId extends TypeTag[JLong]
  type ShareId = JLong @@ IsShareId
  @inline final implicit def ShareToId(s: Share): ShareId = s.id
  @inline final implicit def ShareSToId(s: ShareSummary): ShareId = s.id

  // -------------------------------------------------------------------------------------------------------------------
  // Externalisable ID tags

  sealed trait IsExteralisableId extends TypeTag[JLong] {
    type EITag <: TypeTag[String]
    type EI = String @@ EITag
  }

  /** Marks a Long value as corresponding to `usecase.id` and `usecase_rev.ident_id`. */
  sealed trait IsUseCaseIdentId extends IsExteralisableId {
    override type EITag = IsUseCaseIdentEI
  }
  sealed trait IsUseCaseIdentEI extends TypeTag[String]
  type UseCaseIdentId = JLong @@ IsUseCaseIdentId
  type UseCaseIdentEI = String @@ IsUseCaseIdentEI
  @inline final implicit def BasicUseCaseInfoToIdentId(r: BasicUseCaseInfo): UseCaseIdentId = r.identId
  @inline final implicit def UseCaseIdentToIdentId(i: UseCaseIdent): UseCaseIdentId = i.identId
  @inline final implicit def cp2uid(u: UseCaseSaveCheckpoint): UseCaseIdentId = u.rec

  /** Marks a Long value as corresponding to `text_rev.id`. */
  sealed trait IsTextRevId extends IsExteralisableId {
    override type EITag = IsTextRevEI
  }
  sealed trait IsTextRevEI extends TypeTag[String]
  type TextRevId = JLong @@ IsTextRevId
  type TextRevEI = String @@ IsTextRevEI
  @inline final implicit def TextRevToId(r: TextRev): TextRevId = r.id

  /** Marks a Long value as corresponding to `project.id`. */
  trait IsProjectId extends IsExteralisableId {
    override type EITag = IsProjectEI
  }
  sealed trait IsProjectEI extends TypeTag[String]
  type ProjectId = JLong @@ IsProjectId
  type ProjectEI = String @@ IsProjectEI
  @inline final implicit def p2pid(p: Project): ProjectId = p.id
  @inline final implicit def uci2pid(u: UseCaseIdent): ProjectId = u.projectId
  @inline final implicit def ucr2pid(u: UseCaseRev): ProjectId = u.projectId
  @inline final implicit def cp2pid(u: UseCaseSaveCheckpoint): ProjectId = u.rec

  object AutoExternaliseIds {
    implicit def aei_P (id: ProjectId)     : ProjectEI      = ExternalId.Project(id)
    implicit def aei_UC(id: UseCaseIdentId): UseCaseIdentEI = ExternalId.UseCase(id)
    implicit def aei_TR(id: TextRevId)     : TextRevEI      = ExternalId.TextRev(id)
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
