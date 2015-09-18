package shipreq.webapp.base.text

import monocle.Iso
import shipreq.base.util.{NonEmptyVector, UnivEq}
import shipreq.webapp.base.data._

object Atom {

  sealed trait Type
  object Type {
    case object Literal       extends Type
    case object BlankLine     extends Type
    case object ReqRef        extends Type
    case object CodeRef       extends Type
    case object Issue         extends Type
    case object WebAddress    extends Type
    case object EmailAddress  extends Type
    case object MathTeX       extends Type
    case object TagRef        extends Type
    case object UnorderedList extends Type

    val values = NonEmptyVector[Type](
      Literal, WebAddress, EmailAddress, MathTeX,
      ReqRef, CodeRef, TagRef, Issue,
      BlankLine, UnorderedList)

    val of: AnyAtom => Type = {
      case _: Literal         # Literal       => Literal
      case _: NewLine         # BlankLine     => BlankLine
      case _: ReqRef          # ReqRef        => ReqRef
      case _: ReqRef          # CodeRef       => CodeRef
      case _: Issue           # Issue         => Issue
      case _: PlainTextMarkup # WebAddress    => WebAddress
      case _: PlainTextMarkup # EmailAddress  => EmailAddress
      case _: PlainTextMarkup # MathTeX       => MathTeX
      case _: TagRef          # TagRef        => TagRef
      case _: ListMarkup      # UnorderedList => UnorderedList
    }
  }

  type AnyAtom  = Base#Atom
  type AnyIssue = Issue#Issue

  // ===================================================================================================================

  sealed trait Base {
    sealed trait Atom
    final type OptionalText = Vector[Atom]
    final type NonEmptyText = NonEmptyVector[Atom]

    final val NonEmptyIso: Iso[Option[NonEmptyText], OptionalText] =
      Iso[Option[NonEmptyText], OptionalText](_.fold(Vector.empty[Atom])(_.whole))(NonEmptyVector.option)
  }

  /** Literal text, like "hello there" */
  trait Literal extends Base {
    case class Literal(value: String) extends Atom {
      // For tests
      def map(f: String => String): this.type = Literal(f(value)).asInstanceOf[this.type]
    }
  }

  trait NewLine extends Base {
    case class BlankLine() extends Atom
    final val blankLine = BlankLine()
  }

  trait ListMarkup extends Base {
    final type ListItem = Vector[Atom]
    case class UnorderedList(items: NonEmptyVector[ListItem]) extends Atom {
      // For tests
      def filterAtoms(f: Atom => Boolean): this.type = UnorderedList(items.map(_ filter f)).asInstanceOf[this.type]
      def map(f: ListItem => ListItem): this.type = UnorderedList(items map f).asInstanceOf[this.type]
    }
  }

  trait PlainTextMarkup extends Base {
    /** Web address, like "https://www.google.com" */
    case class WebAddress(value: String) extends Atom

    /** Email address, like "bob@hotmail.com" */
    case class EmailAddress(value: String) extends Atom

    /** Math in TeX format, like "\frac{22}{7}-\pi" */
    case class MathTeX(value: String) extends Atom
  }

  trait ReqRef extends Base {

    /** Reference to a requirement, like "UC-4" */
    case class ReqRef(value: ReqId) extends Atom

    /** Reference to a [[ReqCode.Target]] */
    case class CodeRef(value: ReqCodeId) extends Atom
  }

  /** An inline issue, like "#TBD" */
  trait Issue extends Base {
    case class Issue(typ: CustomIssueTypeId, desc: Text.InlineIssueDesc.OptionalText) extends Atom
  }

  /** An inline tag, like "#pri=high" */
  trait TagRef extends Base {
    case class TagRef(value: ApplicableTagId) extends Atom
  }

  // Prove that UnivEq[Atom] is acceptable.
  // A proof for all atom args should be added here.
  UnivEq[NonEmptyVector[Vector[String]]]
  UnivEq[ApplicableTagId]
  UnivEq[CustomIssueTypeId]
  UnivEq[ReqId]

  // ===================================================================================================================

  trait SingleLine extends Literal with PlainTextMarkup {
    val multiLine = false
  }

  trait MultiLine extends SingleLine with NewLine with ListMarkup  {
    override final val multiLine = true
  }

  /** The main title/desc of a top-level requirement. */
  trait ReqTitle extends SingleLine
    with Issue
    with ReqRef
    with TagRef
}
