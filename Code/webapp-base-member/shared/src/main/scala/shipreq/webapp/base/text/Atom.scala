package shipreq.webapp.base.text

import japgolly.microlibs.adt_macros.AdtMacros
import monocle.Iso
import scala.collection.immutable.TreeSet
import scala.reflect.ClassTag
import scalaz.Applicative
import shipreq.base.util.NonEmptyArraySeq
import shipreq.base.util.Util.ShipReqOpsForArraySeq
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
    case object Bold           extends Type(TypeGroup.PlainTextMarkup)
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
    case object Italic         extends Type(TypeGroup.PlainTextMarkup)
    case object Literal        extends Type(TypeGroup.Literal)
    case object Monospace      extends Type(TypeGroup.PlainTextMarkup)
    case object OrderedList    extends Type(TypeGroup.ListMarkup)
    case object ReqRef         extends Type(TypeGroup.ContentRef)
    case object Strikethrough  extends Type(TypeGroup.PlainTextMarkup)
    case object TagRef         extends Type(TypeGroup.TagRef)
    case object TeX            extends Type(TypeGroup.PlainTextMarkup)
    case object Underline      extends Type(TypeGroup.PlainTextMarkup)
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
      case _: ListMarkup      # OrderedList    => OrderedList
      case _: ListMarkup      # UnorderedList  => UnorderedList
      case _: Literal         # Literal        => Literal
      case _: NewLine         # BlankLine      => BlankLine
      case _: PlainTextMarkup # Bold           => Bold
      case _: PlainTextMarkup # EmailAddress   => EmailAddress
      case _: PlainTextMarkup # Italic         => Italic
      case _: PlainTextMarkup # Monospace      => Monospace
      case _: PlainTextMarkup # Strikethrough  => Strikethrough
      case _: PlainTextMarkup # TeX            => TeX
      case _: PlainTextMarkup # Underline      => Underline
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

      def exists(f: AnyAtom => Boolean): Boolean

      final def containsType[T <: AnyAtom](implicit ct: ClassTag[T]): Boolean =
        exists(ct.runtimeClass.isInstance)

      // For tests
      @nowarn("cat=unused") def modText(f: String => String): this.type = this
      @nowarn("cat=unused") def modTextF[F[_]](f: String => F[String])(implicit F: Applicative[F]): F[this.type] = F.pure(this)
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
      override final def exists(f: AnyAtom => Boolean) = f(this)

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
      override final def exists(f: AnyAtom => Boolean) = f(this)
    }
    final val blankLine = BlankLine()
  }

  trait Headings extends Base { self =>
    val headingTitle: Base

    final type HeadingTitleAtom = headingTitle.Atom
    final type HeadingTitle     = headingTitle.NonEmptyText

    sealed abstract class Heading extends Atom {
      type Self <: Heading
      final val parent: self.type = self
      override final def isPlain = false
      override final def containsMultipleLines = false
      val title: HeadingTitle
      def copy(title: HeadingTitle): Self

      override final def exists(f: AnyAtom => Boolean) =
        f(this) || title.exists(_.exists(f))

      final def modTitle(f: HeadingTitle => HeadingTitle) = {
        val t2 = f(title)
        if (t2 eq title) this else copy(t2)
      }

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

    sealed abstract class ListBase(_items: NonEmptyArraySeq[ListItem]) extends Atom {
      val items: NonEmptyArraySeq[ListItem]

      final val parent: self.type = self
      final override def isPlain = false
      final override def allowBlankLineAfter = false
      final override def containsMultipleLines = (_items.length > 1) || _items.head.exists(_.containsMultipleLines)
      final val itemsContainMultipleLines = _items.exists(_.exists {
        case _: ListBase => true
        case a           => a.containsMultipleLines
      })

      override final def exists(f: AnyAtom => Boolean) =
        f(this) || _items.exists(_.exists(_.exists(f)))

      // For tests

      def unsafeWithItems(items: NonEmptyArraySeq[ArraySeq[Base#Atom]]): this.type

      final def filterAtoms(f: Atom => Boolean): this.type =
        unsafeWithItems(_items.map(_ filter f))

      final def map(f: ListItem => ListItem): this.type =
        unsafeWithItems(_items map f)

      final override def modText(f: String => String): this.type =
        map(_.map(_.modText(f)))

      final override def modTextF[F[_]](f: String => F[String])(implicit F: Applicative[F]): F[this.type] =
        F.map(_items.traverse(_.traverse(_.modTextF(f).asInstanceOf[F[Atom]])))(unsafeWithItems(_))
    }

    case class OrderedList(items: NonEmptyArraySeq[ListItem]) extends ListBase(items) {
      override def unsafeWithItems(items: NonEmptyArraySeq[ArraySeq[Base#Atom]]): this.type =
        if (items eq this.items)
          this
        else
          OrderedList(items.asInstanceOf[NonEmptyArraySeq[ListItem]]).asInstanceOf[this.type]
    }

    case class UnorderedList(items: NonEmptyArraySeq[ListItem]) extends ListBase(items) {
      override def unsafeWithItems(items: NonEmptyArraySeq[ArraySeq[Base#Atom]]): this.type =
        if (items eq this.items)
          this
        else
          UnorderedList(items.asInstanceOf[NonEmptyArraySeq[ListItem]]).asInstanceOf[this.type]
    }
  }

  trait PlainTextMarkup extends Base { self =>

    /** Type of text that can be styled (i.e. be children of bold/italic/etc) */
    val styled: Literal

    final type Styled = styled.NonEmptyText

    sealed trait PlainTextMarkupOfString extends Atom {
      override final def isPlain = false
      override final def containsMultipleLines = false
      override final def exists(f: AnyAtom => Boolean) = f(this)
      type Self <: PlainTextMarkupOfString
      val value: String
      def copy(value: String): Self

      // For tests

      override final def modText(f: String => String): this.type =
        copy(f(value)).asInstanceOf[this.type]

      override final def modTextF[F[_]](f: String => F[String])(implicit F: Applicative[F]): F[this.type] =
        F.map(f(value))(copy(_).asInstanceOf[this.type])
    }

    sealed trait PlainTextMarkupStyled extends Atom {
      final val parent: self.type = self
      override final def isPlain = false
      override final def containsMultipleLines = false
      type Self <: PlainTextMarkupStyled
      val inner: Styled
      def copy(inner: Styled): Self

      override final def exists(f: AnyAtom => Boolean) =
        f(this) || inner.exists(_.exists(f))

      // For tests

      override final def modText(f: String => String): this.type =
        copy(inner.map(_.modText(f))).asInstanceOf[this.type]

      override final def modTextF[F[_]](f: String => F[String])(implicit F: Applicative[F]): F[this.type] =
        F.map(inner.traverse(_.modTextF(f).asInstanceOf[F[styled.Atom]]))(copy(_).asInstanceOf[this.type])

      final def unsafeWithInner(inner: NonEmptyArraySeq[Base#Atom]): Self =
        if (inner eq this.inner)
          this.asInstanceOf[Self]
        else
          copy(inner.asInstanceOf[Styled])
    }

    /** Web address, like "https://www.google.com" */
    case class WebAddress(value: String) extends PlainTextMarkupOfString {
      override type Self = WebAddress
      override def copy(title: String) = WebAddress(title)
    }

    /** Email address, like "bob@hotmail.com" */
    case class EmailAddress(value: String) extends PlainTextMarkupOfString {
      override type Self = EmailAddress
      override def copy(title: String) = EmailAddress(title)
    }

    /** Content in TeX format, like "\frac{22}{7}-\pi" */
    case class TeX(value: String) extends PlainTextMarkupOfString {
      override type Self = TeX
      override def copy(title: String) = TeX(title)
    }

    /** Inline monospace block, like `omg_yes("no")`
     *
     * @param value Non-empty. Guarded by DataProp & ParsersTest.
     */
    case class Monospace(value: String) extends PlainTextMarkupOfString {
      override type Self = Monospace
      override def copy(title: String) = Monospace(title)
    }

    case class Bold(inner: Styled) extends PlainTextMarkupStyled {
      override type Self = Bold
      override def copy(inner: Styled) = Bold(inner)
    }

    case class Italic(inner: Styled) extends PlainTextMarkupStyled {
      override type Self = Italic
      override def copy(inner: Styled) = Italic(inner)
    }

    case class Strikethrough(inner: Styled) extends PlainTextMarkupStyled {
      override type Self = Strikethrough
      override def copy(inner: Styled) = Strikethrough(inner)
    }

    case class Underline(inner: Styled) extends PlainTextMarkupStyled {
      override type Self = Underline
      override def copy(inner: Styled) = Underline(inner)
    }
  }

  final case class CodeBlockDetail(language: String, attributes: TreeSet[String]) {
    lazy val toText: String =
      if (attributes.isEmpty)
        language
      else
        language + ":" + attributes.mkString(":")
  }

  object CodeBlockDetail {
    implicit def univEq: UnivEq[CodeBlockDetail] = UnivEq.derive
  }

  trait CodeBlock extends Base {
    case class CodeBlock(detail: Option[CodeBlockDetail], code: String) extends Atom {
      override final def isPlain = false
      override final def containsMultipleLines = true
      override final def allowBlankLineAfter = false
      override final def exists(f: AnyAtom => Boolean) = f(this)

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
      override final def exists(f: AnyAtom => Boolean) = f(this) || desc.exists(_.exists(f))
    }
  }

  sealed trait DisplayReqRef
  object DisplayReqRef {
    case object AsId         extends DisplayReqRef
    case object AsIdAndTitle extends DisplayReqRef
    implicit def univEq: UnivEq[DisplayReqRef] = UnivEq.derive

    def memoLazy[A](f: DisplayReqRef => A): DisplayReqRef => A = {
      lazy val a = f(AsId)
      lazy val b = f(AsIdAndTitle)

      {
        case DisplayReqRef.AsId         => a
        case DisplayReqRef.AsIdAndTitle => b
      }
    }
  }

  trait ContentRef extends Base { self =>
    sealed trait ContentRef extends Atom {
      override final def exists(f: AnyAtom => Boolean) = f(this)
    }

    /** Reference to a requirement, like "UC-4". */
    case class ReqRef(id: ReqId, display: DisplayReqRef) extends self.ContentRef {
      override final def isPlain = false
      override final def containsMultipleLines = false
    }

    /** Reference to a requirement via its [[ReqCode]]. */
    case class CodeRef(id: ReqCodeId, display: DisplayReqRef) extends self.ContentRef {
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
      override final def exists(f: AnyAtom => Boolean) = f(this)
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

  trait HeadingTitleFull     extends SingleLine with Issue with ContentRef with TagRef
  trait HeadingTitleNoIssues extends SingleLine            with ContentRef with TagRef

  trait StyledInnerFull       extends SingleLine with Issue with ContentRef with TagRef
  trait StyledInnerContentRef extends SingleLine            with ContentRef
  trait StyledInnerNoIssues   extends SingleLine            with ContentRef with TagRef
  trait StyledInnerNoTags     extends SingleLine with Issue with ContentRef

  /** The main title/desc of a top-level requirement. */
  trait ReqTitle extends SingleLine
    with Issue
    with ContentRef
    with TagRef
}
