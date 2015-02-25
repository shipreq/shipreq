package shipreq.webapp.base.data

import japgolly.nyaya._
import scalaz.syntax.equal._
import scalaz.std.AllInstances._
import shipreq.base.util.Debug._
import shipreq.base.util.Must
import shipreq.base.util.ScalaExt._
import DataImplicits._

object DataProp {

  val rev =
    Prop.test[Rev]("rev ≥ 0", _.value >= 0)

  def justRevAnd[T]: Prop[RevAnd[T]] =
    rev.contramap[RevAnd[T]](_.rev)

  def revAnd[T](p: Prop[T]): Prop[RevAnd[T]] =
    justRevAnd ∧ p.contramap[RevAnd[T]](_.data)

  def must[A](name: => String): Prop[Must[A]] =
    Prop.atom[Must[A]](name, _.fold(Some(_), _ => None))

  def must[A, B](name: => String, f: A => Must[B]): Prop[A] =
    must[B](name).contramap(f)

  def mustThen[A](name: => String, ifExists: Prop[A]): Prop[Must[A]] =
    Prop.eval[Must[A]](m => m.fold(
      e => Eval.atom(name, m, Some(e)),
      a => ifExists(a).liftL))

  def isubsetContents[A]: ISubset[Set, A] => Set[A] = {
    case ISubset.All()   => Set.empty[A]
    case ISubset.Only(v) => v.tail + v.head
    case ISubset.Not(v)  => v.tail + v.head
  }

  private implicit class MapStreamingExt[K, V](val m: Map[K, V]) extends AnyVal {
    @inline def vstream[A](f: V => A): Stream[A] = m.values.toStream.map(f)
    @inline def vstreamf[A](f: V => Stream[A]): Stream[A] = m.values.toStream.flatMap(f)
  }

  // -------------------------------------------------------------------------------------------------------------------
  object customIssueTypes {

    def all = justRevAnd[CustomIssueTypeIMap] rename "CustomIssueTypes"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object customReqType {

    def reqType =
      Prop.test[ReqType]("oldMnemonics doesn't contain current mnemonic", a => !a.oldMnemonics.contains(a.mnemonic))

    // starting to overlap with validation....
    def mnemonicStatic =
      Prop.blacklist[CustomReqType]("mnemonic doesn't overlap with static")(
        _ => StaticReqType.mnemonics, a => a.oldMnemonics + a.mnemonic)

    lazy val all = mnemonicStatic ∧ reqType.subst
  }

  object customReqTypes {
    type T = CustomReqTypeIMap

    def uniqueMnemonics =
      Prop.distinct("mnemonic", (_: T).values.toStream.flatMap(b => b.mnemonic #:: b.oldMnemonics.toStream).map(_.value))

    def uniqueNames =
      Prop.distinct("name", (_: T).values.toStream.map(_.name))

    def each =
      customReqType.all.forall[T, Stream](_.values.toStream)

    lazy val all =
      revAnd[T](uniqueMnemonics ∧ uniqueNames ∧ each) rename "CustomReqTypes"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object fields {

    type Fields = Vector[Field]

    def uniqueNames =
      Prop.distinct("name", (_: Fields).flatMap(_.independentName.toVector))

    def uniqueKeys =
      Prop.distinct("FieldRefKey", (_: Fields).flatMap(_.keyO.toVector))

    def fields =
      mustThen[Fields]("FieldSet.fields", uniqueNames ∧ uniqueKeys)
        .contramap[FieldSet](_.fields)

    def orderNoDups =
      Prop.distinct("order", (_: FieldSet).order)

    def orderCustomFieldsIso =
      Prop.equal[FieldSet]("order.customFields = fieldSet.customFields")(
        _.customFields.keySet,
        _.order.foldLeft(Set.empty[CustomField.Id])((q, id) => id match {
          case i: CustomField.Id => q + i
          case _: StaticField    => q
        }))

    def orderHasAllUndeletableStaticFields =
      Prop.allPresent[FieldSet]("order ⊇ undeletable static")(Function const StaticField.notDeletable.toSet, _.order)

    def filteredFields[T](f: PartialFunction[CustomField, T]): FieldSet => Stream[T] = {
      val ff = f.lift
      _.customFields.values.toStream.flatMap(ff(_).toStream)
    }

    def tagFieldsUnique =
      Prop.distinct("Tag field", filteredFields { case t: CustomField.Tag => t.tagId })

    def implicationFieldsUnique =
      Prop.distinct("Implication field", filteredFields { case t: CustomField.Implication => t.reqTypeId })

    def fieldSet = "FieldSet" rename_: (
      fields ∧
      orderNoDups ∧ orderCustomFieldsIso ∧ orderHasAllUndeletableStaticFields ∧
      tagFieldsUnique ∧ implicationFieldsUnique)

    lazy val all =
      revAnd(fieldSet) rename "Fields"
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
      Prop.whitelist[T]("ids refer to available tags")(_.keySet, _.vstreamf(_.children.toStream))

    def tagTree =
      (uniqueNames ∧ uniqueSiblings ∧ noCycles ∧ noDeadLinks) rename "TagTree"

    lazy val all =
      revAnd(tagTree) rename "Tags"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object reqs {
    type T = Requirements

    def reqPubidsInRegister =
      Prop.forall((_: T).reqs.values.toStream)(t =>
        Prop.equal[Req]("Req's pubid refers to itself in the Pubid register")(
          _.id.some,
          r => t.reqIdByPubid(r.pubId)))

    def pubidsResolveToReqs =
      Prop.whitelist[T]("Pubid register")(_.reqs.keySet, _.pubids.m.vstreamf(_.toStream))

    lazy val all =
      revAnd(reqPubidsInRegister ∧ pubidsResolveToReqs) rename "Requirements"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object reqCodes {
    type T = ReqCodes
    import ReqCode._

    def branchesMustBranch =
      Prop.test[TrieBranch]("TrieBranch branches", _.next.nonEmpty)
        .forall[T, List](t => Trie.simpleFold[List[TrieBranch]](t.trie, Nil)((q, _, n) => n match {
          case b: TrieBranch => b :: q
          case _: Target     => q
        })) rename "All TrieBranches branch"

//    def noSharedTrieBranches =
//      Prop.distinct("No shared trie branches", (_: T).nodeIdsInTrie)

    lazy val all =
      revAnd(branchesMustBranch) rename "ReqCodes"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object implications {
    type T = ReqFieldData.Implications

    def noCycles =
      ReqFieldData.implicationCycleDetector.noCycleProp("implications")
        .contramap[T](_.srcToTgt.m)

    lazy val all = noCycles
  }

  // -------------------------------------------------------------------------------------------------------------------
  object project {
    type T = Project

    def constituents = (
        customIssueTypes.all.contramap[T](_.customIssueTypes)
      ∧   customReqTypes.all.contramap[T](_.customReqTypes)
      ∧           fields.all.contramap[T](_.fields)
      ∧             tags.all.contramap[T](_.tags)
      ∧             reqs.all.contramap[T](_.reqs)
      ∧         reqCodes.all.contramap[T](_.reqCodes)
      ∧     implications.all.contramap[T](_.reqFieldData.data.implications)
    ) rename "constituents"


    def uniqueHashRefKeys =
      Prop.distinct[T, HashRefKey]("HashRefKey", p =>
        p.customIssueTypes.data.values.toStream.map(_.key) #:::
        p.tags.data.vstreamf(_.tag.keyO.toStream))

    def validRefs = {
      def validateFieldIds(name: String, data: T => Traversable[CustomField.Id]) =
        Prop.whitelist[T](name + " are resolvable")(_.fields.data.customFields.keySet, data)

      def validateReqIds(name: String, data: T => Traversable[Req.Id]) =
        Prop.whitelist[T](name + " are resolvable")(_.reqs.data.reqs.vstream(_.id).toSet, data)

      def validateReqTypeIds(name: String, data: T => Traversable[ReqType.Id]) =
        Prop.whitelist[T](name + " are resolvable")(_.reqTypes.map(_.reqTypeId).toSet, data)

      def validateTagIds(name: String, data: T => Traversable[Tag.Id]) =
        Prop.whitelist[T](name + " are resolvable")(_.tags.data.keySet, data)

      (  validateReqTypeIds("Field.reqTypes",
          _.fields.data.customFields.values.toStream.flatMap(f => isubsetContents(f.reqTypes).toStream))

      ∧ validateReqTypeIds("CustomField.Implication.reqTypeIds",
          p => fields.filteredFields({ case t: CustomField.Implication => t.reqTypeId})(p.fields.data))

      ∧ validateReqTypeIds("Pubid keys",                      _.reqs.data.pubids.m.keys)
      ∧ validateFieldIds  ("ReqFieldData.text TextField ids", _.reqFieldData.data.text.keys)
      ∧ validateReqIds    ("ReqFieldData.text.*.reqIds",      _.reqFieldData.data.text.vstreamf(_.keys.toStream))
      ∧ validateReqIds    ("ReqFieldData.tags keys",          _.reqFieldData.data.tags.keys)
      ∧ validateTagIds    ("ReqFieldData.tags values",        _.reqFieldData.data.tags.vstreamf(_.toStream))
      ∧ validateReqIds    ("ReqFieldData.implications",       _.reqFieldData.data.implications.members)
      ) rename "Cross-constituent refs"
    }

    lazy val all =
      "Project" rename_: (constituents ==> (uniqueHashRefKeys ∧ validRefs))
  }
}
