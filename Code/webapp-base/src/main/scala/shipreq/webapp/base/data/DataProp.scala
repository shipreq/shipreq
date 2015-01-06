package shipreq.webapp.base.data

import shipreq.base.util.Debug._
import scalaz.syntax.equal._
import scalaz.std.AllInstances._
import shipreq.prop._
import DataImplicits._

object DataProp {

  lazy val rev =
    Prop.test[Rev]("rev ≥ 0", _.value >= 0)

  def revAnd[T] =
    rev.contramap[RevAnd[T]](_.rev)

  // -------------------------------------------------------------------------------------------------------------------
  object customIssueTypes {

    def all = revAnd[CustomIssueTypeIMap] rename "CustomIssueTypes"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object customReqType {

    lazy val reqType =
      Prop.test[ReqType]("oldMnemonics doesn't contain current mnemonic", a => !a.oldMnemonics.contains(a.mnemonic))

    // starting to overlap with validation....
    lazy val mnemonicStatic =
      Prop.test[CustomReqType]("mnemonic doesn't overlap with static",
        a => ReqType.staticMnemonics.intersect(a.oldMnemonics + a.mnemonic).isEmpty)

    lazy val all = mnemonicStatic ∧ reqType.subst
  }

  object customReqTypes {
    type T = CustomReqTypeIMap

    lazy val uniqueMnemonics =
      Prop.distinct("mnemonic", (_: T).values.toStream.flatMap(b => b.mnemonic #:: b.oldMnemonics.toStream).map(_.value))

    lazy val uniqueNames =
      Prop.distinct("name", (_: T).values.toStream.map(_.name))

    lazy val each =
      customReqType.all.forall[T, Stream](_.values.toStream)

    lazy val all =
      (revAnd[T] ∧ (uniqueMnemonics ∧ uniqueNames ∧ each).contramap[RevAnd[T]](_.data)) rename "CustomReqTypes"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object fields {

    def uniqueNames =
      Prop.distinct("name", (_: FieldSet).fields.map(_.name))

    def uniqueKeys =
      Prop.distinct("FieldRefKey", (_: FieldSet).fields.flatMap(_.keyO.toVector))

    def orderNoDups =
      Prop.distinct("order", (_: FieldSet).order)

    def orderCustomFieldsIso =
      Prop.equal[FieldSet]("order.customFields = fieldSet.customFields")(
        _.customFields.keySet,
        _.order.foldLeft(Set.empty[CustomField.Id])((q, id) => id match {
          case i: CustomField.Id => q + i
          case _: Field.Static   => q
        }))

    def orderHasAllUndeletableStaticFields =
      Prop.prohibitMissingElements[FieldSet]("order ⊇ undeletable static")(
        _ => Field.static.filter(_.deletable ≟ Deletable.Not),
        _.order.toSet)

    def fieldSet = "FieldSet" rename_: (
      uniqueNames ∧ uniqueKeys ∧
      orderNoDups ∧ orderCustomFieldsIso ∧ orderHasAllUndeletableStaticFields)

    lazy val all = (revAnd[FieldSet] ∧ fieldSet.contramap(_.data)) rename "Fields"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object tags {
    type T = TagTree
    import Tag.Id

    def uniqueNames =
      Prop.distinct("name", (_: T).vstream(_.tag.name))

    def uniqueSiblings =
      Prop.distinctC[Vector, Id]("siblings").forall((_: T).vstream(_.children))

    def noCycles =
      Tag.CycleDetectors.tagTree.noCycleProp("structure")

    def noDeadLinks =
      Prop.subset[T]("ids refers to available tags")(_.keySet, _.vstreamf(_.children.toStream))

    def tagTree =
      (uniqueNames ∧ uniqueSiblings ∧ noCycles ∧ noDeadLinks) rename "TagTree"

    lazy val all =
      (tagTree.contramap[RevAnd[T]](_.data) ∧ rev.contramap(_.rev)) rename "Tags"
  }

  // -------------------------------------------------------------------------------------------------------------------

  lazy val uniqueHashRefKeys =
    Prop.distinct[Project, HashRefKey]("HashRefKey", p =>
      p.customIssueTypes.data.values.toStream.map(_.key) #:::
      p.tags.data.vstreamf(_.tag.keyO.toStream))

  lazy val project = "Project" rename_: (
    uniqueHashRefKeys
      ∧ customIssueTypes.all.contramap(_.customIssueTypes)
      ∧   customReqTypes.all.contramap(_.customReqTypes)
      ∧           fields.all.contramap(_.fields)
      ∧             tags.all.contramap(_.tags)
    )
}
