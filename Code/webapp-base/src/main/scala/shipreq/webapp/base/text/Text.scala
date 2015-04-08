package shipreq.webapp.base.text

import shipreq.base.util.{NonEmptyVector, UnivEq}
import shipreq.webapp.base.data._

object Text {

  sealed trait AtomType
  object AtomType {
    case object Literal       extends AtomType
    case object NewLine       extends AtomType
    case object ReqRef        extends AtomType
    case object Issue         extends AtomType
    case object WebAddress    extends AtomType
    case object EmailAddress  extends AtomType
    case object MathTeX       extends AtomType
    case object TagRef        extends AtomType
    case object UnorderedList extends AtomType

    val values = NonEmptyVector[AtomType](
      Literal, WebAddress, EmailAddress, MathTeX,
      ReqRef, TagRef, Issue,
      NewLine, UnorderedList)

    val of: Text.Generic#Atom => AtomType = {
      case _: Text.Generic.Literal         # Literal       => Literal
      case _: Text.Generic.NewLine         # NewLine       => NewLine
      case _: Text.Generic.ReqRef          # ReqRef        => ReqRef
      case _: Text.Generic.Issue           # Issue         => Issue
      case _: Text.Generic.PlainTextMarkup # WebAddress    => WebAddress
      case _: Text.Generic.PlainTextMarkup # EmailAddress  => EmailAddress
      case _: Text.Generic.PlainTextMarkup # MathTeX       => MathTeX
      case _: Text.Generic.TagRef          # TagRef        => TagRef
      case _: Text.Generic.ListMarkup      # UnorderedList => UnorderedList
    }
  }

  // ===================================================================================================================
  // Generic

  sealed trait Generic {
    sealed trait Atom
    final type OptionalText = Vector[Atom]
    final type NonEmptyText = NonEmptyVector[Atom]

    implicit def atomEquality[A <: Atom]: UnivEq[A] = UnivEq.force
  }

  object Generic {

    /** Literal text, like "hello there" */
    sealed trait Literal extends Generic {
      case class Literal(value: String) extends Atom {
        def map(f: String => String): this.type = Literal(f(value)).asInstanceOf[this.type]
      }
    }

    sealed trait NewLine extends Generic {
      case class NewLine() extends Atom
      final val newLine = NewLine()
    }

    sealed trait ListMarkup extends Generic {
      final type ListItem = Vector[Atom]
      case class UnorderedList(items: NonEmptyVector[ListItem]) extends Atom
    }

    sealed trait PlainTextMarkup extends Generic {
      /** Web address, like "https://www.google.com" */
      case class WebAddress(value: String) extends Atom

      /** Email address, like "bob@hotmail.com" */
      case class EmailAddress(value: String) extends Atom

      /** Math in TeX format, like "\frac{22}{7}-\pi" */
      case class MathTeX(value: String) extends Atom
    }

    sealed trait SingleLine extends Literal with PlainTextMarkup

    sealed trait MultiLine extends SingleLine with NewLine with ListMarkup

    /** Reference to a requirement, like "UC-4" */
    sealed trait ReqRef extends Generic {
      case class ReqRef(value: Req.Id) extends Atom
    }

    // ReqCodes need IDs?
    // TODO ↓ FR-152 needs much thought!
    //    case class ValidReqCode(tgt: ReqCode.Target, pref: ReqCode) extends Atom
    //    with ReqTitle

    // UC Step     - pending UC data types

    /** An inline issue, like "#TBD" */
    sealed trait Issue extends Generic {
      case class Issue(typ: CustomIssueType.Id, desc: InlineIssueDesc.OptionalText) extends Atom
    }

    /** An inline tag, like "#pri=high" */
    sealed trait TagRef extends Generic {
      case class TagRef(value: ApplicableTag.Id) extends Atom
    }

    /** The main title/desc of a top-level requirement. */
    sealed trait ReqTitle extends SingleLine
      with ReqRef
      with Issue
  }

  // Prove that UnivEq[Atom] is acceptable.
  // A proof for all atom args should be added here.
  UnivEq[ApplicableTag.Id]
  UnivEq[CustomIssueType.Id]
  UnivEq[Req.Id]

  // ===================================================================================================================
  // Specialised

  import Generic._

  object RecCodeGroupDesc extends ReqTitle

  object GenericReqDesc extends ReqTitle

  object InlineIssueDesc extends SingleLine
    with ReqRef

  object CustomTextField extends MultiLine
    with ReqRef
    with Issue
    with TagRef
}
