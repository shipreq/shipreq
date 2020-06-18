package shipreq.webapp.base.text

import japgolly.microlibs.adt_macros.AdtMacros
import monocle.Iso
import scala.collection.immutable.ArraySeq
import scalaz.Applicative
import scalaz.Scalaz.Id
import scalaz.syntax.traverse
import shipreq.base.util.NonEmptyArraySeq
import shipreq.base.util.ScalazExtra.foldableArraySeq
import shipreq.base.util.Util.ShipReqOpsForArraySeq
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
  // - Headings

  sealed abstract class TypeGroup
  object TypeGroup {
    case object CodeBlock       extends TypeGroup
    case object ContentRef      extends TypeGroup
    case object Headings        extends TypeGroup
    case object Issue           extends TypeGroup
    case object ListMarkup      extends TypeGroup
    case object Literal         extends TypeGroup
    case object NewLine         extends TypeGroup
    case object PlainTextMarkup extends TypeGroup
    case object TagRef          extends TypeGroup

    val values = AdtMacros.adtValues[TypeGroup]
  }

  sealed abstract class Type(final val group: TypeGroup)
  object Type {
    case object BlankLine      extends Type(TypeGroup.NewLine)
    case object CodeBlock      extends Type(TypeGroup.CodeBlock)
    case object CodeRef        extends Type(TypeGroup.ContentRef)
    case object EmailAddress   extends Type(TypeGroup.PlainTextMarkup)
    case object Heading1       extends Type(TypeGroup.Headings)
    case object Heading2       extends Type(TypeGroup.Headings)
    case object Heading3       extends Type(TypeGroup.Headings)
    case object Heading4       extends Type(TypeGroup.Headings)
    case object Heading5       extends Type(TypeGroup.Headings)
    case object Heading6       extends Type(TypeGroup.Headings)
    case object Issue          extends Type(TypeGroup.Issue)
    case object Literal        extends Type(TypeGroup.Literal)
    case object Monospace      extends Type(TypeGroup.PlainTextMarkup)
    case object ReqRef         extends Type(TypeGroup.ContentRef)
    case object TagRef         extends Type(TypeGroup.TagRef)
    case object TeX            extends Type(TypeGroup.PlainTextMarkup)
    case object UnorderedList  extends Type(TypeGroup.ListMarkup)
    case object UseCaseStepRef extends Type(TypeGroup.ContentRef)
    case object WebAddress     extends Type(TypeGroup.PlainTextMarkup)

    val values = AdtMacros.adtValues[Type]

    val of: AnyAtom => Type = {
      case _: CodeBlock       # CodeBlock      => CodeBlock
      case _: ContentRef      # CodeRef        => CodeRef
      case _: ContentRef      # ReqRef         => ReqRef
      case _: ContentRef      # UseCaseStepRef => UseCaseStepRef
      case _: Headings        # Heading1       => Heading1
      case _: Headings        # Heading2       => Heading2
      case _: Headings        # Heading3       => Heading3
      case _: Headings        # Heading4       => Heading4
      case _: Headings        # Heading5       => Heading5
      case _: Headings        # Heading6       => Heading6
      case _: Issue           # Issue          => Issue
      case _: ListMarkup      # UnorderedList  => UnorderedList
      case _: Literal         # Literal        => Literal
      case _: NewLine         # BlankLine      => BlankLine
      case _: PlainTextMarkup # EmailAddress   => EmailAddress
      case _: PlainTextMarkup # Monospace      => Monospace
      case _: PlainTextMarkup # TeX            => TeX
      case _: PlainTextMarkup # WebAddress     => WebAddress
      case _: TagRef          # TagRef         => TagRef
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

      // For tests
      def modText(f: String => String): this.type = this
      def modTextF[F[_]](f: String => F[String])(implicit F: Applicative[F]): F[this.type] = F.pure(this)
    }

    final type OptionalText = ArraySeq[Atom]
    final type NonEmptyText = NonEmptyArraySeq[Atom]

    @inline final def empty: OptionalText =
      ArraySeq.empty

    def apply(as: Atom*): OptionalText =
      ArraySeq.from(as)

    def nonEmpty(a1: Atom, an: Atom*): NonEmptyText = {
      val b = ArraySeq.newBuilder[Atom]
      b += a1
      b ++= an
      NonEmptyArraySeq.force(b.result())
    }

    def toOptional(o: Option[NonEmptyText]): OptionalText =
      NonEmptyIso.get(o)

    final val NonEmptyIso: Iso[Option[NonEmptyText], OptionalText] =
      Iso[Option[NonEmptyText], OptionalText](_.fold(ArraySeq.empty[Atom])(_.whole))(NonEmptyArraySeq.option)

    final def supports(g: TypeGroup): Boolean =
      g match {
        case TypeGroup.CodeBlock        => this.isInstanceOf[Atom.CodeBlock]
        case TypeGroup.ContentRef       => this.isInstanceOf[Atom.ContentRef]
        case TypeGroup.Headings         => this.isInstanceOf[Atom.Headings]
        case TypeGroup.Issue            => this.isInstanceOf[Atom.Issue]
        case TypeGroup.ListMarkup       => this.isInstanceOf[Atom.ListMarkup]
        case TypeGroup.Literal          => this.isInstanceOf[Atom.Literal]
        case TypeGroup.NewLine          => this.isInstanceOf[Atom.NewLine]
        case TypeGroup.PlainTextMarkup  => this.isInstanceOf[Atom.PlainTextMarkup]
        case TypeGroup.TagRef           => this.isInstanceOf[Atom.TagRef]
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

      override def modText(f: String => String): this.type =
        copy(f(value)).asInstanceOf[this.type]

      override def modTextF[F[_]](f: String => F[String])(implicit F: Applicative[F]): F[this.type] =
        F.map(f(value))(copy(_).asInstanceOf[this.type])
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

  trait Headings extends Base { self =>
    val headerTitle: Base

    final type HeadingTitleAtom = headerTitle.Atom
    final type HeadingTitle     = headerTitle.NonEmptyText

    sealed abstract class Heading extends Atom {
      type Self <: Heading
      final val parent: self.type = self
      override final def isPlain = false
      override final def containsMultipleLines = false
      val title: HeadingTitle
      def copy(title: HeadingTitle): Self

      // For tests

      final def modTitle(f: HeadingTitle => HeadingTitle) =
        copy(f(title))

      final override def modText(f: String => String): this.type =
        copy(title.map(_.modText(f))).asInstanceOf[this.type]

      override def modTextF[F[_]](f: String => F[String])(implicit F: Applicative[F]): F[this.type] =
        F.map(title.traverse(_.modTextF(f).asInstanceOf[F[HeadingTitleAtom]]))(copy(_).asInstanceOf[this.type])
    }

    final def unsafeHeadingByIdx(n: Int, title: HeadingTitle): Heading =
      n match {
        case 0 => Heading1(title)
        case 1 => Heading2(title)
        case 2 => Heading3(title)
        case 3 => Heading4(title)
        case 4 => Heading5(title)
        case 5 => Heading6(title)
      }

    case class Heading1(title: HeadingTitle) extends Heading {
      override type Self = Heading1
      override def copy(title: HeadingTitle) = Heading1(title)
    }

    case class Heading2(title: HeadingTitle) extends Heading {
      override type Self = Heading2
      override def copy(title: HeadingTitle) = Heading2(title)
    }

    case class Heading3(title: HeadingTitle) extends Heading {
      override type Self = Heading3
      override def copy(title: HeadingTitle) = Heading3(title)
    }

    case class Heading4(title: HeadingTitle) extends Heading {
      override type Self = Heading4
      override def copy(title: HeadingTitle) = Heading4(title)
    }

    case class Heading5(title: HeadingTitle) extends Heading {
      override type Self = Heading5
      override def copy(title: HeadingTitle) = Heading5(title)
    }

    case class Heading6(title: HeadingTitle) extends Heading {
      override type Self = Heading6
      override def copy(title: HeadingTitle) = Heading6(title)
    }
  }

  trait ListMarkup extends Base { self =>
    final type ListItem = ArraySeq[Atom]

    case class UnorderedList(items: NonEmptyArraySeq[ListItem]) extends Atom {
      val parent: self.type = self
      override final def isPlain = false
      override final def containsMultipleLines = (items.length > 1) || items.head.exists(_.containsMultipleLines)

      val itemsContainMultipleLines = items.exists(_.exists(_.containsMultipleLines))

      // For tests

      def filterAtoms(f: Atom => Boolean): this.type =
        UnorderedList(items.map(_ filter f)).asInstanceOf[this.type]

      def map(f: ListItem => ListItem): this.type =
        UnorderedList(items map f).asInstanceOf[this.type]

      def unsafeWithItems(items: NonEmptyArraySeq[ArraySeq[Base#Atom]]): UnorderedList =
        UnorderedList(items.asInstanceOf[NonEmptyArraySeq[ListItem]])

      override def modText(f: String => String): this.type =
        map(_.map(_.modText(f)))

      override def modTextF[F[_]](f: String => F[String])(implicit F: Applicative[F]): F[this.type] =
        F.map(items.traverse(_.traverse(_.modTextF(f).asInstanceOf[F[Atom]])))(copy(_).asInstanceOf[this.type])
    }
  }

  trait PlainTextMarkup extends Base {
    /** Web address, like "https://www.google.com" */
    case class WebAddress(value: String) extends Atom {
      override final def isPlain = false
      override final def containsMultipleLines = false

      // For tests

      override def modText(f: String => String): this.type =
        copy(f(value)).asInstanceOf[this.type]

      override def modTextF[F[_]](f: String => F[String])(implicit F: Applicative[F]): F[this.type] =
        F.map(f(value))(copy(_).asInstanceOf[this.type])
    }

    /** Email address, like "bob@hotmail.com" */
    case class EmailAddress(value: String) extends Atom {
      override final def isPlain = false
      override final def containsMultipleLines = false

      // For tests

      override def modText(f: String => String): this.type =
        copy(f(value)).asInstanceOf[this.type]

      override def modTextF[F[_]](f: String => F[String])(implicit F: Applicative[F]): F[this.type] =
        F.map(f(value))(copy(_).asInstanceOf[this.type])
    }

    /** Content in TeX format, like "\frac{22}{7}-\pi" */
    case class TeX(value: String) extends Atom {
      override final def isPlain = false
      override final def containsMultipleLines = false

      // For tests

      override def modText(f: String => String): this.type =
        copy(f(value)).asInstanceOf[this.type]

      override def modTextF[F[_]](f: String => F[String])(implicit F: Applicative[F]): F[this.type] =
        F.map(f(value))(copy(_).asInstanceOf[this.type])
    }

    /** Inline monospace block, like `omg_yes("no")`
     *
     * @param value Non-empty. Guarded by ParsersTest.
     */
    case class Monospace(value: String) extends Atom {
      override final def isPlain = false
      override final def containsMultipleLines = false

      // For tests

      override def modText(f: String => String): this.type =
        copy(f(value)).asInstanceOf[this.type]

      override def modTextF[F[_]](f: String => F[String])(implicit F: Applicative[F]): F[this.type] =
        F.map(f(value))(copy(_).asInstanceOf[this.type])
    }
  }

  trait CodeBlock extends Base {
    case class CodeBlock(language: Option[String], code: String) extends Atom {
      override final def isPlain = false
      override final def containsMultipleLines = true
      override final def allowBlankLineAfter = false

      // For tests

      override def modText(f: String => String): this.type =
        copy(code = f(code)).asInstanceOf[this.type]

      override def modTextF[F[_]](f: String => F[String])(implicit F: Applicative[F]): F[this.type] =
        F.map(f(code))(c => copy(code = c).asInstanceOf[this.type])
    }
  }

  trait SingleLine
      extends Literal
         with PlainTextMarkup {
    val lineCardinality: LineCardinality = T.SingleLine
  }

  trait MultiLine
      extends SingleLine
         with NewLine
         with Headings
         with ListMarkup
         with CodeBlock {
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
  UnivEq[NonEmptyArraySeq[ArraySeq[String]]]
  UnivEq[ApplicableTagId]
  UnivEq[CustomIssueTypeId]
  UnivEq[ReqId]
  UnivEq[UseCaseStepId]

  /** The main title/desc of a top-level requirement. */
  trait ReqTitle extends SingleLine
    with Issue
    with ContentRef
    with TagRef

  trait HeadingTitleFull     extends SingleLine with Issue with ContentRef with TagRef
  trait HeadingTitleNoIssues extends SingleLine            with ContentRef with TagRef

}
