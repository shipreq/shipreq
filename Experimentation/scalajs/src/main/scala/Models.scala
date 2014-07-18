import scalaz.\/

object Models {

  // ------------------------------------------------------------------------------------------
  // Fields

  // ------------------------------------------------------------------------------------------
  // ReqTypes

  // Invariants:
  // - name and mnemonic unique
  // - mnemonic can never be reused

  object ReqTypes {
    type Mnemonic = String

    trait ReqType {
      def deleted: Boolean
      def mnemonic: Mnemonic
      def name: String
      def implicationRequired: Boolean
    }

    trait UC extends ReqType
    trait UserDefined extends ReqType

    class Collection {
      val c: List[ReqType] = ???
      val m: Map[Mnemonic, ReqType] = ???
    }
  }

  // ------------------------------------------------------------------------------------------
  // Tags

  // ------------------------------------------------------------------------------------------
  // Issues

  type Field = Nothing
  type Tag = Nothing
  type Req_+ = Nothing // UC, MF, etc.
  type Req_- = Nothing // UC Step, custom field
  type Req = Req_- \/ Req_+
  type Ref = Nothing
  type SHR = Nothing
  type RichText = Nothing

  object Issues {
    sealed trait Issue

    // Invariants
    // - Name/refkey is unique
    // - Refkey doesn't conflict with Tag refkeys
    class UserDefinedIssueType {

    }

    case class TagNeedsChildren(t: Tag) extends Issue
    case class FieldBuiltOnDeletedTag(f: Field) extends Issue
    case class RefInvalid(r: Ref, in: Req) extends Issue
    case class RefToDeleted(r: Ref, in: Req) extends Issue
    case class ReqNeedsImplication(r: Req_+) extends Issue
    case class ReqNeedsTag(r: Req_+, tagFamily: Tag) extends Issue
    case class ReqNeedsValue(r: Req) extends Issue
    case class TagsConflict(r: Req, tagFamily: Tag) extends Issue
    case class ShrNeedsChildren(r: SHR) extends Issue
    case class Loose(content: RichText) extends Issue
    case class UserDef(t: UserDefinedIssueType, supp: Option[RichText]) extends Issue


  }

  // ------------------------------------------------------------------------------------------
  // Reqs

  // ------------------------------------------------------------------------------------------
  // Project Config

  class ProjectConfig {

  }

}