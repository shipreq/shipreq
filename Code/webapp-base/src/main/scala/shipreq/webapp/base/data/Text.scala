package shipreq.webapp.base.data

import scalaz.NonEmptyList

object Text {

  // ===================================================================================================================
  // Generic

  sealed trait Generic {
    sealed trait Atom
    final type OptionalText = List[Atom]
    final type NonEmptyText = NonEmptyList[Atom]
  }

  object Generic {

    /** Literal text, like "hello there" */
    sealed trait Literal extends Generic {
      case class Literal(value: String) extends Atom
    }

    sealed trait NewLine extends Generic {
      case class NewLine() extends Atom
      final val newLine = NewLine()
    }

    sealed trait ListMarkup extends Generic {
      final type ListItem = List[Atom]
      case class UnorderedList(items: NonEmptyList[ListItem]) extends Atom
    }

    sealed trait PlainTextMarkup extends Generic {
      /** Web address, like "https://www.google.com" */
      case class WebAddress(value: String) extends Atom

      /** Email address, like "bob@hotmail.com" */
      case class EmailAddress(value: String) extends Atom

      /** Math in TeX format, like "\frac{22}{7}-\pi" */
      case class MathTeX(value: String) extends Atom
    }

    sealed trait SingleLineText extends Literal with PlainTextMarkup

    sealed trait MultiLineText extends SingleLineText with NewLine with ListMarkup

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
      case class TagRef(value: Tag.Id) extends Atom
    }

    /** The main title/desc of a top-level requirement. */
    sealed trait ReqTitle extends SingleLineText
      with ReqRef
      with Issue
  }

  // ===================================================================================================================
  // Specialised

  import Generic._

  object RecCodeGroupDesc extends ReqTitle

  object GenericReqDesc extends ReqTitle

  object InlineIssueDesc extends SingleLineText
    with ReqRef

  object CustomTextField extends MultiLineText
    with ReqRef
    with Issue
    with TagRef

}
