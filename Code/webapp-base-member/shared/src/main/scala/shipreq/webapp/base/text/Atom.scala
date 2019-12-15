package shipreq.webapp.base.text

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptyVector
import monocle.Iso
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.{text => T}

object Atom {

  // Mixin Hierarchy
  // ===============
  // - MultiLine
  //   - SingleLine
  //     - Literal
  //     - PlainTextMarkup
  //   - NewLine
  //   - ListMarkup
  // - Issue
  // - ContentRef
  // - TagRef

  sealed trait Type
  object Type {
    case object Literal        extends Type
    case object BlankLine      extends Type
    case object ReqRef         extends Type
    case object CodeRef        extends Type
    case object UseCaseStepRef extends Type
    case object Issue          extends Type
    case object WebAddress     extends Type
    case object EmailAddress   extends Type
    case object TeX            extends Type
    case object TagRef         extends Type
    case object UnorderedList  extends Type

    val values = AdtMacros.adtValues[Type]

    val of: AnyAtom => Type = {
      case _: Literal         # Literal        => Literal
      case _: NewLine         # BlankLine      => BlankLine
      case _: ContentRef      # ReqRef         => ReqRef
      case _: ContentRef      # CodeRef        => CodeRef
      case _: ContentRef      # UseCaseStepRef => UseCaseStepRef
      case _: Issue           # Issue          => Issue
      case _: PlainTextMarkup # WebAddress     => WebAddress
      case _: PlainTextMarkup # EmailAddress   => EmailAddress
      case _: PlainTextMarkup # TeX            => TeX
      case _: TagRef          # TagRef         => TagRef
      case _: ListMarkup      # UnorderedList  => UnorderedList
    }
  }

  type AnyAtom       = Base#Atom
  type AnyIssue      = Issue#Issue
  type AnyContentRef = ContentRef#ContentRef

  // ===================================================================================================================
  // Basics - reduces down to either SingleLine or MultiLine

  sealed trait Base {
    sealed trait Atom {
      /** Plain text atom, or rich text atom? */
      def isPlain: Boolean
      @inline final def isRich: Boolean = !isPlain
    }
    final type OptionalText = Vector[Atom]
    final type NonEmptyText = NonEmptyVector[Atom]

    @inline final def empty: OptionalText =
      Vector.empty

    def toOptional(o: Option[NonEmptyText]): OptionalText =
      NonEmptyIso.get(o)

    final val NonEmptyIso: Iso[Option[NonEmptyText], OptionalText] =
      Iso[Option[NonEmptyText], OptionalText](_.fold(Vector.empty[Atom])(_.whole))(NonEmptyVector.option)

    final def supportsPTM     = this match { case _: Atom.PlainTextMarkup => true; case _ => false }
    final def supportsReqRefs = this match { case _: Atom.ContentRef      => true; case _ => false }
    final def supportsTags    = this match { case _: Atom.TagRef          => true; case _ => false }
    final def supportsIssues  = this match { case _: Atom.Issue           => true; case _ => false }
  }

  /** Literal text, like "hello there" */
  trait Literal extends Base {
    case class Literal(value: String) extends Atom {
      override final def isPlain = true
      // For tests
      def map(f: String => String): this.type = Literal(f(value)).asInstanceOf[this.type]
    }
  }

  trait NewLine extends Base {
    case class BlankLine() extends Atom {
      override final def isPlain = true
    }
    final val blankLine = BlankLine()
  }

  trait ListMarkup extends Base {
    final type ListItem = Vector[Atom]
    case class UnorderedList(items: NonEmptyVector[ListItem]) extends Atom {
      override final def isPlain = false
      // For tests
      def filterAtoms(f: Atom => Boolean): this.type = UnorderedList(items.map(_ filter f)).asInstanceOf[this.type]
      def map(f: ListItem => ListItem): this.type = UnorderedList(items map f).asInstanceOf[this.type]
    }
  }

  trait PlainTextMarkup extends Base {
    /** Web address, like "https://www.google.com" */
    case class WebAddress(value: String) extends Atom {
      override final def isPlain = false
    }

    /** Email address, like "bob@hotmail.com" */
    case class EmailAddress(value: String) extends Atom {
      override final def isPlain = false
    }

    /** Content in TeX format, like "\frac{22}{7}-\pi" */
    case class TeX(value: String) extends Atom {
      override final def isPlain = false
    }
  }

  trait SingleLine extends Literal with PlainTextMarkup {
    val lineCardinality: LineCardinality = T.SingleLine
  }

  trait MultiLine extends SingleLine with NewLine with ListMarkup  {
    override final val lineCardinality = T.MultiLine
  }

  // ===================================================================================================================
  // Optional bells 'n' whistles

  /** An inline issue, like "#TBD" */
  trait Issue extends Base {
    case class Issue(typ: CustomIssueTypeId, desc: Text.InlineIssueDesc.OptionalText) extends Atom {
      override final def isPlain = false
    }
  }

  trait ContentRef extends Base { self =>
    sealed trait ContentRef extends Atom

    /** Reference to a requirement, like "UC-4". */
    case class ReqRef(value: ReqId) extends self.ContentRef {
      override final def isPlain = false
    }

    /** Reference to a requirement via its [[ReqCode]]. */
    case class CodeRef(value: ReqCodeId) extends self.ContentRef {
      override final def isPlain = false
    }

    /** Reference to a UC step, like "UC-4.0.1.a". */
    case class UseCaseStepRef(value: UseCaseStepId) extends self.ContentRef {
      override final def isPlain = false
    }
  }

  /** An inline tag, like "#pri=high" */
  trait TagRef extends Base {
    case class TagRef(value: ApplicableTagId) extends Atom {
      override final def isPlain = false
    }
  }

  // ===================================================================================================================

  // Prove that UnivEq[Atom] is acceptable.
  // A proof for all atom args should be added here.
  UnivEq[NonEmptyVector[Vector[String]]]
  UnivEq[ApplicableTagId]
  UnivEq[CustomIssueTypeId]
  UnivEq[ReqId]
  UnivEq[UseCaseStepId]

  /** The main title/desc of a top-level requirement. */
  trait ReqTitle extends SingleLine
    with Issue
    with ContentRef
    with TagRef
}
