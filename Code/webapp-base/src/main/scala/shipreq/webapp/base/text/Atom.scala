package shipreq.webapp.base.text

import shipreq.base.util.{NonEmptyVector, UnivEq}
import shipreq.webapp.base.data._

object Atom {

  sealed trait Type
  object Type {
    case object Literal       extends Type
    case object NewLine       extends Type
    case object ReqRef        extends Type
    case object Issue         extends Type
    case object WebAddress    extends Type
    case object EmailAddress  extends Type
    case object MathTeX       extends Type
    case object TagRef        extends Type
    case object UnorderedList extends Type

    val values = NonEmptyVector[Type](
      Literal, WebAddress, EmailAddress, MathTeX,
      ReqRef, TagRef, Issue,
      NewLine, UnorderedList)

    val of: Generic => Type = {
      case _: Literal         # Literal       => Literal
      case _: NewLine         # NewLine       => NewLine
      case _: ReqRef          # ReqRef        => ReqRef
      case _: Issue           # Issue         => Issue
      case _: PlainTextMarkup # WebAddress    => WebAddress
      case _: PlainTextMarkup # EmailAddress  => EmailAddress
      case _: PlainTextMarkup # MathTeX       => MathTeX
      case _: TagRef          # TagRef        => TagRef
      case _: ListMarkup      # UnorderedList => UnorderedList
    }
  }

  type Generic = Base#Atom

  // ===================================================================================================================

  sealed trait Base {
    sealed trait Atom
    final type OptionalText = Vector[Atom]
    final type NonEmptyText = NonEmptyVector[Atom]

    implicit def atomEquality[A <: Atom]: UnivEq[A] = UnivEq.force
  }

  /** Literal text, like "hello there" */
  trait Literal extends Base {
    case class Literal(value: String) extends Atom {
      def map(f: String => String): this.type = Literal(f(value)).asInstanceOf[this.type]
    }
  }

  trait NewLine extends Base {
    case class NewLine() extends Atom
    final val newLine = NewLine()
  }

  trait ListMarkup extends Base {
    final type ListItem = Vector[Atom]
    case class UnorderedList(items: NonEmptyVector[ListItem]) extends Atom
  }

  trait PlainTextMarkup extends Base {
    /** Web address, like "https://www.google.com" */
    case class WebAddress(value: String) extends Atom

    /** Email address, like "bob@hotmail.com" */
    case class EmailAddress(value: String) extends Atom

    /** Math in TeX format, like "\frac{22}{7}-\pi" */
    case class MathTeX(value: String) extends Atom
  }

  /** Reference to a requirement, like "UC-4" */
  trait ReqRef extends Base {
    case class ReqRef(value: Req.Id) extends Atom
  }

  // ReqCodes need IDs?
  // TODO ↓ FR-152 needs much thought!
  //    case class ValidReqCode(tgt: ReqCode.Target, pref: ReqCode) extends Atom
  //    with ReqTitle

  // UC Step     - pending UC data types

  /** An inline issue, like "#TBD" */
  trait Issue extends Base {
    case class Issue(typ: CustomIssueType.Id, desc: Text.InlineIssueDesc.OptionalText) extends Atom
  }

  /** An inline tag, like "#pri=high" */
  trait TagRef extends Base {
    case class TagRef(value: ApplicableTag.Id) extends Atom
  }

  // Prove that UnivEq[Atom] is acceptable.
  // A proof for all atom args should be added here.
  UnivEq[NonEmptyVector[Vector[String]]]
  UnivEq[ApplicableTag.Id]
  UnivEq[CustomIssueType.Id]
  UnivEq[Req.Id]

  // ===================================================================================================================

  trait SingleLine extends Literal with PlainTextMarkup

  trait MultiLine extends SingleLine with NewLine with ListMarkup

  /** The main title/desc of a top-level requirement. */
  trait ReqTitle extends SingleLine
    with ReqRef
    with Issue
}
