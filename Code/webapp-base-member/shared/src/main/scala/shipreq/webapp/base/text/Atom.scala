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
  //   - CodeBlock
  // - Issue
  // - ContentRef
  // - TagRef

  sealed abstract class TypeGroup
  object TypeGroup {
    case object Literal         extends TypeGroup
    case object ContentRef      extends TypeGroup
    case object Issue           extends TypeGroup
    case object ListMarkup      extends TypeGroup
    case object NewLine         extends TypeGroup
    case object PlainTextMarkup extends TypeGroup
    case object TagRef          extends TypeGroup
    case object CodeBlock       extends TypeGroup

    val values = AdtMacros.adtValues[TypeGroup]
  }

  sealed abstract class Type(final val group: TypeGroup)
  object Type {
    case object Literal        extends Type(TypeGroup.Literal)
    case object BlankLine      extends Type(TypeGroup.NewLine)
    case object ReqRef         extends Type(TypeGroup.ContentRef)
    case object CodeRef        extends Type(TypeGroup.ContentRef)
    case object UseCaseStepRef extends Type(TypeGroup.ContentRef)
    case object Issue          extends Type(TypeGroup.Issue)
    case object WebAddress     extends Type(TypeGroup.PlainTextMarkup)
    case object EmailAddress   extends Type(TypeGroup.PlainTextMarkup)
    case object TeX            extends Type(TypeGroup.PlainTextMarkup)
    case object TagRef         extends Type(TypeGroup.TagRef)
    case object UnorderedList  extends Type(TypeGroup.ListMarkup)
    case object CodeBlock      extends Type(TypeGroup.CodeBlock)

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
      case _: CodeBlock       # CodeBlock      => CodeBlock
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
      def containsMultipleLines: Boolean
      def isBlankLine = false
      def allowBlankLineAfter = true
      @inline final def allowBlankLineBefore = allowBlankLineAfter // so far this holds but it might not always
    }
    final type OptionalText = Vector[Atom]
    final type NonEmptyText = NonEmptyVector[Atom]

    @inline final def empty: OptionalText =
      Vector.empty

    def toOptional(o: Option[NonEmptyText]): OptionalText =
      NonEmptyIso.get(o)

    final val NonEmptyIso: Iso[Option[NonEmptyText], OptionalText] =
      Iso[Option[NonEmptyText], OptionalText](_.fold(Vector.empty[Atom])(_.whole))(NonEmptyVector.option)

    final def supports(g: TypeGroup): Boolean =
      g match {
        case TypeGroup.ContentRef      => this.isInstanceOf[Atom.ContentRef]
        case TypeGroup.Issue           => this.isInstanceOf[Atom.Issue]
        case TypeGroup.ListMarkup      => this.isInstanceOf[Atom.ListMarkup]
        case TypeGroup.Literal         => this.isInstanceOf[Atom.Literal]
        case TypeGroup.NewLine         => this.isInstanceOf[Atom.NewLine]
        case TypeGroup.PlainTextMarkup => this.isInstanceOf[Atom.PlainTextMarkup]
        case TypeGroup.TagRef          => this.isInstanceOf[Atom.TagRef]
        case TypeGroup.CodeBlock       => this.isInstanceOf[Atom.CodeBlock]
      }

    final def supports(t: Type): Boolean =
      supports(t.group)
  }

  /** Literal text, like "hello there" */
  trait Literal extends Base {
    case class Literal(value: String) extends Atom {
      override final def isPlain = true
      override final def containsMultipleLines = false
      // For tests
      def map(f: String => String): this.type = Literal(f(value)).asInstanceOf[this.type]
    }
  }

  trait NewLine extends Base {
    case class BlankLine() extends Atom {
      override final def isPlain = true
      override final def isBlankLine = true
      override final def containsMultipleLines = true
      override final def allowBlankLineAfter = false
    }
    final val blankLine = BlankLine()
  }

  trait ListMarkup extends Base {
    final type ListItem = Vector[Atom]
    case class UnorderedList(items: NonEmptyVector[ListItem]) extends Atom {
      override final def isPlain = false
      override final def containsMultipleLines = (items.length > 1) || items.head.exists(_.containsMultipleLines)

      val itemsContainMultipleLines = items.exists(_.exists(_.containsMultipleLines))

      // For tests
      def filterAtoms(f: Atom => Boolean): this.type = UnorderedList(items.map(_ filter f)).asInstanceOf[this.type]
      def map(f: ListItem => ListItem): this.type = UnorderedList(items map f).asInstanceOf[this.type]
    }
  }

  trait PlainTextMarkup extends Base {
    /** Web address, like "https://www.google.com" */
    case class WebAddress(value: String) extends Atom {
      override final def isPlain = false
      override final def containsMultipleLines = false
    }

    /** Email address, like "bob@hotmail.com" */
    case class EmailAddress(value: String) extends Atom {
      override final def isPlain = false
      override final def containsMultipleLines = false
    }

    /** Content in TeX format, like "\frac{22}{7}-\pi" */
    case class TeX(value: String) extends Atom {
      override final def isPlain = false
      override final def containsMultipleLines = false
    }
  }

  trait CodeBlock extends Base {
    case class CodeBlock(content: String) extends Atom {
      override final def isPlain = false
      override final def containsMultipleLines = true
      override final def allowBlankLineAfter = false
    }
  }

  trait SingleLine extends Literal with PlainTextMarkup {
    val lineCardinality: LineCardinality = T.SingleLine
  }

  trait MultiLine extends SingleLine with NewLine with ListMarkup with CodeBlock {
    override final val lineCardinality = T.MultiLine
  }

  // ===================================================================================================================
  // Optional bells 'n' whistles

  /** An inline issue, like "#TBD" */
  trait Issue extends Base {
    case class Issue(typ: CustomIssueTypeId, desc: Text.InlineIssueDesc.OptionalText) extends Atom {
      override final def isPlain = false
      override final def containsMultipleLines = false
    }
  }

  trait ContentRef extends Base { self =>
    sealed trait ContentRef extends Atom

    /** Reference to a requirement, like "UC-4". */
    case class ReqRef(value: ReqId) extends self.ContentRef {
      override final def isPlain = false
      override final def containsMultipleLines = false
    }

    /** Reference to a requirement via its [[ReqCode]]. */
    case class CodeRef(value: ReqCodeId) extends self.ContentRef {
      override final def isPlain = false
      override final def containsMultipleLines = false
    }

    /** Reference to a UC step, like "UC-4.0.1.a". */
    case class UseCaseStepRef(value: UseCaseStepId) extends self.ContentRef {
      override final def isPlain = false
      override final def containsMultipleLines = false
    }
  }

  /** An inline tag, like "#pri=high" */
  trait TagRef extends Base {
    case class TagRef(value: ApplicableTagId) extends Atom {
      override final def isPlain = false
      override final def containsMultipleLines = false
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
