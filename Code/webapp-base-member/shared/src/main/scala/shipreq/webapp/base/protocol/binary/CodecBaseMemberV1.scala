package shipreq.webapp.base.protocol.binary

import boopickle.ConstPickler
import boopickle.DefaultBasic._
import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.univeq.UnivEq
import java.time.Instant
import shipreq.base.util.{Direction, IMap}
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.issue.IssueCategory
import shipreq.webapp.base.sort.SortMethod
import shipreq.webapp.base.text.{AtomTC, ProjectText, Text}

object CodecBaseMemberV1 {
  import CodecBaseV1._

  // ===================================================================================================================
  // Polymorphic definitions
  // (non-implicit, "pickle" prefix)

  def pickleIMapD[K: UnivEq : Pickler, V: Pickler](implicit d: DataIdAux[V, K]): Pickler[IMap[K, V]] =
    pickleIMap(d.emptyIMap)

  // ===================================================================================================================
  // Concrete picklers for base data type
  // (implicit lazy vals, "pickler" prefix)

  object AtomPicklers extends AtomTC[Pickler] {
    import shipreq.webapp.base.text._
    import Atom._
    import Text.Equality._

    override def lazily[A](f: => Pickler[A]): Pickler[A] = pickleLazily(f)

    override def vec[A](implicit a: Pickler[A]) = implicitly

    override def nev[A](as: Pickler[Vector[A]])(implicit a: Pickler[A]) = pickleNEV

    override def sum[T <: Atom.Base](t: T)(f: t.Atom => Pickler[t.Atom], index: t.Atom => Int, all: Vector[Pickler[t.Atom]]): Pickler[t.Atom] =
      new Pickler[t.Atom] {
        override def pickle(a: t.Atom)(implicit state: PickleState): Unit = {
          val i = index(a)
          state.enc.writeInt(i)
          all(i).pickle(a)
        }
        override def unpickle(implicit state: UnpickleState): t.Atom = {
          val i = state.dec.readInt
          all(i).unpickle
        }
      }

    override def blankLine[T <: NewLine](t: T): Pickler[t.BlankLine] =
      ConstPickler(t.blankLine)

    override def literal[T <: Literal](t: T): Pickler[t.Literal] =
      transformPickler((i: String) => t.Literal(i))(_.value)

    override def webAddress[T <: PlainTextMarkup](t: T): Pickler[t.WebAddress] =
      transformPickler((i: String) => t.WebAddress(i))(_.value)

    override def emailAddress[T <: PlainTextMarkup](t: T): Pickler[t.EmailAddress] =
      transformPickler((i: String) => t.EmailAddress(i))(_.value)

    override def mathTeX[T <: PlainTextMarkup](t: T): Pickler[t.MathTeX] =
      transformPickler((i: String) => t.MathTeX(i))(_.value)

    override def reqRef[T <: ContentRef](t: T): Pickler[t.ReqRef] =
      transformPickler((i: ReqId) => t.ReqRef(i))(_.value)

    override def codeRef[T <: ContentRef](t: T): Pickler[t.CodeRef] =
      transformPickler((i: ReqCodeId) => t.CodeRef(i))(_.value)

    override def useCaseStepRef[T <: ContentRef](t: T): Pickler[t.UseCaseStepRef] =
      transformPickler((i: UseCaseStepId) => t.UseCaseStepRef(i))(_.value)

    override def tagRef[T <: TagRef](t: T): Pickler[t.TagRef] =
      transformPickler((i: ApplicableTagId) => t.TagRef(i))(_.value)

    override def issue[T <: Issue](t: T)(implicit h: Pickler[Text.InlineIssueDesc.OptionalText]): Pickler[t.Issue] =
      new Pickler[t.Issue] {
        override def pickle(a: t.Issue)(implicit state: PickleState): Unit = {
          state.pickle(a.typ)
          state.pickle(a.desc)
        }
        override def unpickle(implicit state: UnpickleState): t.Issue = {
          val typ  = state.unpickle[CustomIssueTypeId]
          val desc = state.unpickle[Text.InlineIssueDesc.OptionalText]
          t.Issue(typ, desc)
        }
      }

    override def unorderedList[T <: ListMarkup](t: T)(implicit h: Pickler[NonEmptyVector[t.ListItem]]): Pickler[t.UnorderedList] =
      transformPickler((i: NonEmptyVector[t.ListItem]) => t.UnorderedList(i))(_.items)
  }

  import AtomPicklers.instances._

  object ReqTableDataPicklers {
    import reqtable._

    implicit val picklerColumnImplications: Pickler[Column.Implications] =
      transformPickler(Column.Implications.apply)(_.dir)

    implicit val picklerColumnCustomField: Pickler[Column.CustomField] =
      transformPickler(Column.CustomField.apply)(_.id)

    implicit val picklerColumn: Pickler[Column] =
      new Pickler[Column] {
        import Column._
        private[this] final val KeyCode           = 0
        private[this] final val KeyCustomField    = 1
        private[this] final val KeyDeletionReason = 2
        private[this] final val KeyImplications   = 3
        private[this] final val KeyPubid          = 4
        private[this] final val KeyReqType        = 5
        private[this] final val KeyTags           = 6
        private[this] final val KeyTitle          = 7
        override def pickle(a: Column)(implicit state: PickleState): Unit =
          a match {
            case Code              => state.enc.writeByte(KeyCode          )
            case b: CustomField    => state.enc.writeByte(KeyCustomField   ); state.pickle(b)
            case DeletionReason    => state.enc.writeByte(KeyDeletionReason)
            case b: Implications   => state.enc.writeByte(KeyImplications  ); state.pickle(b)
            case Pubid             => state.enc.writeByte(KeyPubid         )
            case ReqType           => state.enc.writeByte(KeyReqType       )
            case Tags              => state.enc.writeByte(KeyTags          )
            case Title             => state.enc.writeByte(KeyTitle         )
          }
        override def unpickle(implicit state: UnpickleState): Column =
          state.dec.readByte match {
            case KeyCode           => Code
            case KeyCustomField    => state.unpickle[CustomField]
            case KeyDeletionReason => DeletionReason
            case KeyImplications   => state.unpickle[Implications]
            case KeyPubid          => Pubid
            case KeyReqType        => ReqType
            case KeyTags           => Tags
            case KeyTitle          => Title
          }
      }

    implicit val picklerColumnSortConclusive: Pickler[Column.SortConclusive] =
      new Pickler[Column.SortConclusive] {
        private[this] final val KeyPubid = 0
        override def pickle(a: Column.SortConclusive)(implicit state: PickleState): Unit =
          a match {
            case Column.Pubid => state.enc.writeByte(KeyPubid)
          }
        override def unpickle(implicit state: UnpickleState): Column.SortConclusive =
          state.dec.readByte match {
            case KeyPubid => Column.Pubid
          }
      }

    implicit val picklerColumnSortInconclusive: Pickler[Column.SortInconclusive] =
      new Pickler[Column.SortInconclusive] {
        private[this] final val KeyCode           = 0
        private[this] final val KeyCustomField    = 1
        private[this] final val KeyDeletionReason = 2
        private[this] final val KeyImplications   = 3
        private[this] final val KeyReqType        = 4
        private[this] final val KeyTags           = 5
        private[this] final val KeyTitle          = 6
        override def pickle(a: Column.SortInconclusive)(implicit state: PickleState): Unit =
          a match {
            case Column.Code              => state.enc.writeByte(KeyCode          )
            case b: Column.CustomField    => state.enc.writeByte(KeyCustomField   ); state.pickle(b)
            case Column.DeletionReason    => state.enc.writeByte(KeyDeletionReason)
            case b: Column.Implications   => state.enc.writeByte(KeyImplications  ); state.pickle(b)
            case Column.ReqType           => state.enc.writeByte(KeyReqType       )
            case Column.Tags              => state.enc.writeByte(KeyTags          )
            case Column.Title             => state.enc.writeByte(KeyTitle         )
          }
        override def unpickle(implicit state: UnpickleState): Column.SortInconclusive =
          state.dec.readByte match {
            case KeyCode           => Column.Code
            case KeyCustomField    => state.unpickle[Column.CustomField]
            case KeyDeletionReason => Column.DeletionReason
            case KeyImplications   => state.unpickle[Column.Implications]
            case KeyReqType        => Column.ReqType
            case KeyTags           => Column.Tags
            case KeyTitle          => Column.Title
          }
      }

    implicit val picklerColumnSortInconclusiveHasBlanks: Pickler[Column.SortInconclusiveHasBlanks] =
      new Pickler[Column.SortInconclusiveHasBlanks] {
        private[this] final val KeyCode           = 0
        private[this] final val KeyCustomField    = 1
        private[this] final val KeyDeletionReason = 2
        private[this] final val KeyImplications   = 3
        private[this] final val KeyTags           = 4
        private[this] final val KeyTitle          = 5
        override def pickle(a: Column.SortInconclusiveHasBlanks)(implicit state: PickleState): Unit =
          a match {
            case Column.Code              => state.enc.writeByte(KeyCode          )
            case b: Column.CustomField    => state.enc.writeByte(KeyCustomField   ); state.pickle(b)
            case Column.DeletionReason    => state.enc.writeByte(KeyDeletionReason)
            case b: Column.Implications   => state.enc.writeByte(KeyImplications  ); state.pickle(b)
            case Column.Tags              => state.enc.writeByte(KeyTags          )
            case Column.Title             => state.enc.writeByte(KeyTitle         )
          }
        override def unpickle(implicit state: UnpickleState): Column.SortInconclusiveHasBlanks =
          state.dec.readByte match {
            case KeyCode           => Column.Code
            case KeyCustomField    => state.unpickle[Column.CustomField]
            case KeyDeletionReason => Column.DeletionReason
            case KeyImplications   => state.unpickle[Column.Implications]
            case KeyTags           => Column.Tags
            case KeyTitle          => Column.Title
          }
      }

    implicit val picklerColumnSortInconclusiveNoBlanks: Pickler[Column.SortInconclusiveNoBlanks] =
      new Pickler[Column.SortInconclusiveNoBlanks] {
        private[this] final val KeyReqType = 0
        override def pickle(a: Column.SortInconclusiveNoBlanks)(implicit state: PickleState): Unit =
          a match {
            case Column.ReqType => state.enc.writeByte(KeyReqType)
          }
        override def unpickle(implicit state: UnpickleState): Column.SortInconclusiveNoBlanks =
          state.dec.readByte match {
            case KeyReqType => Column.ReqType
          }
      }

    implicit val pickleColumnSIs: Pickler[Vector[Column.SortInconclusive]] =
      iterablePickler

    implicit val pickleColumnNEV: Pickler[NonEmptyVector[Column]] =
      pickleNEV

    implicit val picklerSortMethod: Pickler[SortMethod] =
      new Pickler[SortMethod] {
        import SortMethod._
        private[this] final val KeyAsc            = 0
        private[this] final val KeyAscThenBlanks  = 1
        private[this] final val KeyBlanksThenAsc  = 2
        private[this] final val KeyBlanksThenDesc = 3
        private[this] final val KeyDesc           = 4
        private[this] final val KeyDescThenBlanks = 5
        override def pickle(a: SortMethod)(implicit state: PickleState): Unit =
          a match {
            case Asc            => state.enc.writeByte(KeyAsc           )
            case AscThenBlanks  => state.enc.writeByte(KeyAscThenBlanks )
            case BlanksThenAsc  => state.enc.writeByte(KeyBlanksThenAsc )
            case BlanksThenDesc => state.enc.writeByte(KeyBlanksThenDesc)
            case Desc           => state.enc.writeByte(KeyDesc          )
            case DescThenBlanks => state.enc.writeByte(KeyDescThenBlanks)
          }
        override def unpickle(implicit state: UnpickleState): SortMethod =
          state.dec.readByte match {
            case KeyAsc            => Asc
            case KeyAscThenBlanks  => AscThenBlanks
            case KeyBlanksThenAsc  => BlanksThenAsc
            case KeyBlanksThenDesc => BlanksThenDesc
            case KeyDesc           => Desc
            case KeyDescThenBlanks => DescThenBlanks
          }
      }

    /*
    implicit val picklerSortMethodAscHalf: Pickler[SortMethod.AscHalf] =
      new Pickler[SortMethod.AscHalf] {
        private[this] final val KeyAsc           = 0
        private[this] final val KeyAscThenBlanks = 1
        private[this] final val KeyBlanksThenAsc = 2
        override def pickle(a: SortMethod.AscHalf)(implicit state: PickleState): Unit =
          a match {
            case SortMethod.Asc           => state.enc.writeByte(KeyAsc          )
            case SortMethod.AscThenBlanks => state.enc.writeByte(KeyAscThenBlanks)
            case SortMethod.BlanksThenAsc => state.enc.writeByte(KeyBlanksThenAsc)
          }
        override def unpickle(implicit state: UnpickleState): SortMethod.AscHalf =
          state.dec.readByte match {
            case KeyAsc           => SortMethod.Asc
            case KeyAscThenBlanks => SortMethod.AscThenBlanks
            case KeyBlanksThenAsc => SortMethod.BlanksThenAsc
          }
      }

    implicit val picklerSortMethodDescHalf: Pickler[SortMethod.DescHalf] =
      new Pickler[SortMethod.DescHalf] {
        private[this] final val KeyBlanksThenDesc = 0
        private[this] final val KeyDesc           = 1
        private[this] final val KeyDescThenBlanks = 2
        override def pickle(a: SortMethod.DescHalf)(implicit state: PickleState): Unit =
          a match {
            case SortMethod.BlanksThenDesc => state.enc.writeByte(KeyBlanksThenDesc)
            case SortMethod.Desc           => state.enc.writeByte(KeyDesc          )
            case SortMethod.DescThenBlanks => state.enc.writeByte(KeyDescThenBlanks)
          }
        override def unpickle(implicit state: UnpickleState): SortMethod.DescHalf =
          state.dec.readByte match {
            case KeyBlanksThenDesc => SortMethod.BlanksThenDesc
            case KeyDesc           => SortMethod.Desc
            case KeyDescThenBlanks => SortMethod.DescThenBlanks
          }
      }
      */

    implicit val picklerSortMethodIgnoreBlanks: Pickler[SortMethod.IgnoreBlanks] =
      new Pickler[SortMethod.IgnoreBlanks] {
        private[this] final val KeyAsc  = 0
        private[this] final val KeyDesc = 1
        override def pickle(a: SortMethod.IgnoreBlanks)(implicit state: PickleState): Unit =
          a match {
            case SortMethod.Asc  => state.enc.writeByte(KeyAsc )
            case SortMethod.Desc => state.enc.writeByte(KeyDesc)
          }
        override def unpickle(implicit state: UnpickleState): SortMethod.IgnoreBlanks =
          state.dec.readByte match {
            case KeyAsc  => SortMethod.Asc
            case KeyDesc => SortMethod.Desc
          }
      }

    implicit val picklerSortMethodConsiderBlanks: Pickler[SortMethod.ConsiderBlanks] =
      new Pickler[SortMethod.ConsiderBlanks] {
        private[this] final val KeyAscThenBlanks  = 0
        private[this] final val KeyBlanksThenAsc  = 1
        private[this] final val KeyBlanksThenDesc = 2
        private[this] final val KeyDescThenBlanks = 3
        override def pickle(a: SortMethod.ConsiderBlanks)(implicit state: PickleState): Unit =
          a match {
            case SortMethod.AscThenBlanks  => state.enc.writeByte(KeyAscThenBlanks )
            case SortMethod.BlanksThenAsc  => state.enc.writeByte(KeyBlanksThenAsc )
            case SortMethod.BlanksThenDesc => state.enc.writeByte(KeyBlanksThenDesc)
            case SortMethod.DescThenBlanks => state.enc.writeByte(KeyDescThenBlanks)
          }
        override def unpickle(implicit state: UnpickleState): SortMethod.ConsiderBlanks =
          state.dec.readByte match {
            case KeyAscThenBlanks  => SortMethod.AscThenBlanks
            case KeyBlanksThenAsc  => SortMethod.BlanksThenAsc
            case KeyBlanksThenDesc => SortMethod.BlanksThenDesc
            case KeyDescThenBlanks => SortMethod.DescThenBlanks
          }
      }

    implicit val picklerSortCriterionInconclusiveCB: Pickler[SortCriterion.InconclusiveCB] =
      new Pickler[SortCriterion.InconclusiveCB] {
        override def pickle(a: SortCriterion.InconclusiveCB)(implicit state: PickleState): Unit = {
          state.pickle(a.column)
          state.pickle(a.method)
        }
        override def unpickle(implicit state: UnpickleState): SortCriterion.InconclusiveCB = {
          val column = state.unpickle[Column.SortInconclusiveHasBlanks]
          val method = state.unpickle[SortMethod.ConsiderBlanks]
          SortCriterion.InconclusiveCB(column, method)
        }
      }

    implicit val picklerSortCriterionInconclusiveIB: Pickler[SortCriterion.InconclusiveIB] =
      new Pickler[SortCriterion.InconclusiveIB] {
        override def pickle(a: SortCriterion.InconclusiveIB)(implicit state: PickleState): Unit = {
          state.pickle(a.column)
          state.pickle(a.method)
        }
        override def unpickle(implicit state: UnpickleState): SortCriterion.InconclusiveIB = {
          val column = state.unpickle[Column.SortInconclusiveNoBlanks]
          val method = state.unpickle[SortMethod.IgnoreBlanks]
          SortCriterion.InconclusiveIB(column, method)
        }
      }

    implicit val picklerSortCriterionConclusive: Pickler[SortCriterion.Conclusive] =
      new Pickler[SortCriterion.Conclusive] {
        override def pickle(a: SortCriterion.Conclusive)(implicit state: PickleState): Unit = {
          state.pickle(a.column)
          state.pickle(a.method)
        }
        override def unpickle(implicit state: UnpickleState): SortCriterion.Conclusive = {
          val column = state.unpickle[Column.SortConclusive]
          val method = state.unpickle[SortMethod.IgnoreBlanks]
          SortCriterion.Conclusive(column, method)
        }
      }

    implicit val picklerSortCriterionInconclusive: Pickler[SortCriterion.Inconclusive] =
      new Pickler[SortCriterion.Inconclusive] {
        private[this] final val KeyInconclusiveCB = 0
        private[this] final val KeyInconclusiveIB = 1
        override def pickle(a: SortCriterion.Inconclusive)(implicit state: PickleState): Unit =
          a match {
            case b: SortCriterion.InconclusiveCB => state.enc.writeByte(KeyInconclusiveCB); state.pickle(b)
            case b: SortCriterion.InconclusiveIB => state.enc.writeByte(KeyInconclusiveIB); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): SortCriterion.Inconclusive =
          state.dec.readByte match {
            case KeyInconclusiveCB => state.unpickle[SortCriterion.InconclusiveCB]
            case KeyInconclusiveIB => state.unpickle[SortCriterion.InconclusiveIB]
          }
      }

    implicit val picklerSortCriterion: Pickler[SortCriterion] =
      new Pickler[SortCriterion] {
        import SortCriterion._
        private[this] final val KeyConclusive     = 0
        private[this] final val KeyInconclusiveCB = 1
        private[this] final val KeyInconclusiveIB = 2
        override def pickle(a: SortCriterion)(implicit state: PickleState): Unit =
          a match {
            case b: Conclusive     => state.enc.writeByte(KeyConclusive    ); state.pickle(b)
            case b: InconclusiveCB => state.enc.writeByte(KeyInconclusiveCB); state.pickle(b)
            case b: InconclusiveIB => state.enc.writeByte(KeyInconclusiveIB); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): SortCriterion =
          state.dec.readByte match {
            case KeyConclusive     => state.unpickle[Conclusive]
            case KeyInconclusiveCB => state.unpickle[InconclusiveCB]
            case KeyInconclusiveIB => state.unpickle[InconclusiveIB]
          }
      }

    implicit val pickleSortCriterionIs: Pickler[Vector[SortCriterion.Inconclusive]] =
      iterablePickler

    implicit val picklerSortCriteria: Pickler[SortCriteria] =
      new Pickler[SortCriteria] {
        override def pickle(a: SortCriteria)(implicit state: PickleState): Unit = {
          state.pickle(a.init)
          state.pickle(a.last)
        }
        override def unpickle(implicit state: UnpickleState): SortCriteria = {
          val init = state.unpickle[Vector[SortCriterion.Inconclusive]]
          val last = state.unpickle[SortCriterion.Conclusive]
          SortCriteria(init, last)
        }
      }

    implicit val picklerView: Pickler[View] =
      new Pickler[View] {
        override def pickle(a: View)(implicit state: PickleState): Unit = {
          state.pickle(a.columns)
          state.pickle(a.order)
          state.pickle(a.filterDead)
          state.pickle(a.filter)
        }
        override def unpickle(implicit state: UnpickleState): View = {
          val columns    = state.unpickle[NonEmptyVector[Column]]
          val order      = state.unpickle[SortCriteria]
          val filterDead = state.unpickle[FilterDead]
          val filter     = state.unpickle[Option[Filter.Valid]]
          View(columns, order, filterDead, filter)
        }
      }

    implicit val picklerSavedViewId: Pickler[SavedView.Id] =
      transformPickler(SavedView.Id.apply)(_.value)

    implicit val picklerSavedViewName: Pickler[SavedView.Name] =
      transformPickler(SavedView.Name.apply)(_.value)

    implicit val picklerSavedView: Pickler[SavedView] =
      new Pickler[SavedView] {
        override def pickle(a: SavedView)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.name)
          state.pickle(a.view)
        }
        override def unpickle(implicit state: UnpickleState): SavedView = {
          val id   = state.unpickle[SavedView.Id]
          val name = state.unpickle[SavedView.Name]
          val view = state.unpickle[View]
          SavedView(id, name, view)
        }
      }

    implicit val pickleSavedViewsND: Pickler[SavedViews.NonDefault] =
      pickleIMap(SavedViews.emptyNonDefault)

    implicit val pickleSavedViews: Pickler[SavedViews.NonEmpty] =
      new Pickler[SavedViews.NonEmpty] {
        override def pickle(a: SavedViews.NonEmpty)(implicit state: PickleState): Unit = {
          state.pickle(a.default)
          state.pickle(a.nonDefault)
        }
        override def unpickle(implicit state: UnpickleState): SavedViews.NonEmpty = {
          val default    = state.unpickle[SavedView]
          val nonDefault = state.unpickle[SavedViews.NonDefault]
          SavedViews.NonEmpty(default, nonDefault)
        }
      }
  }

  import ReqTableDataPicklers.pickleSavedViews

  implicit lazy val picklerApplicableTag: Pickler[ApplicableTag] =
    new Pickler[ApplicableTag] {
      override def pickle(a: ApplicableTag)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.name)
        state.pickle(a.desc)
        state.pickle(a.key)
        state.pickle(a.live)
      }
      override def unpickle(implicit state: UnpickleState): ApplicableTag = {
        val id   = state.unpickle[ApplicableTagId]
        val name = state.unpickle[String]
        val desc = state.unpickle[Option[String]]
        val key  = state.unpickle[HashRefKey]
        val live = state.unpickle[Live]
        ApplicableTag(id, name, desc, key, live)
      }
    }

  implicit lazy val picklerApplicableTagId: Pickler[ApplicableTagId] =
    pickleTaggedI(ApplicableTagId).reuseByUnivEq

  implicit lazy val picklerApReqCodeId: Pickler[ApReqCodeId] =
    pickleTaggedI(ApReqCodeId.apply).reuseByUnivEq

  implicit lazy val picklerApReqCodeIdAndValue: Pickler[ApReqCodeId.AndValue] =
    new Pickler[ApReqCodeId.AndValue] {
      override def pickle(a: ApReqCodeId.AndValue)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.value)
      }
      override def unpickle(implicit state: UnpickleState): ApReqCodeId.AndValue = {
        val id    = state.unpickle[ApReqCodeId]
        val value = state.unpickle[ReqCode.Value]
        ApReqCodeId.AndValue(id, value)
      }
    }

  implicit lazy val picklerCodeGroup: Pickler[CodeGroup] =
    new Pickler[CodeGroup] {
      private[this] final val KeyDeadCodeGroup = 'd'
      private[this] final val KeyLiveCodeGroup = 'l'
      override def pickle(a: CodeGroup)(implicit state: PickleState): Unit =
        a match {
          case b: DeadCodeGroup => state.enc.writeByte(KeyDeadCodeGroup); state.pickle(b)
          case b: LiveCodeGroup => state.enc.writeByte(KeyLiveCodeGroup); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): CodeGroup =
        state.dec.readByte match {
          case KeyDeadCodeGroup => state.unpickle[DeadCodeGroup]
          case KeyLiveCodeGroup => state.unpickle[LiveCodeGroup]
        }
    }

  implicit lazy val picklerCustomField: Pickler[CustomField] =
    new Pickler[CustomField] {
      private[this] final val KeyImplication = 'i'
      private[this] final val KeyTag         = 't'
      private[this] final val KeyText        = 'x'
      override def pickle(a: CustomField)(implicit state: PickleState): Unit =
        a match {
          case b: CustomField.Implication => state.enc.writeByte(KeyImplication); state.pickle(b)
          case b: CustomField.Tag         => state.enc.writeByte(KeyTag        ); state.pickle(b)
          case b: CustomField.Text        => state.enc.writeByte(KeyText       ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): CustomField =
        state.dec.readByte match {
          case KeyImplication => state.unpickle[CustomField.Implication]
          case KeyTag         => state.unpickle[CustomField.Tag        ]
          case KeyText        => state.unpickle[CustomField.Text       ]
        }
    }

  implicit lazy val picklerCustomFieldId: Pickler[CustomFieldId] =
    new Pickler[CustomFieldId] {
      private[this] final val KeyImplication = 'i'
      private[this] final val KeyTag         = 't'
      private[this] final val KeyText        = 'x'
      override def pickle(a: CustomFieldId)(implicit state: PickleState): Unit =
        a match {
          case b: CustomField.Implication.Id => state.enc.writeByte(KeyImplication); state.pickle(b)
          case b: CustomField.Tag        .Id => state.enc.writeByte(KeyTag        ); state.pickle(b)
          case b: CustomField.Text       .Id => state.enc.writeByte(KeyText       ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): CustomFieldId =
        state.dec.readByte match {
          case KeyImplication => state.unpickle[CustomField.Implication.Id]
          case KeyTag         => state.unpickle[CustomField.Tag        .Id]
          case KeyText        => state.unpickle[CustomField.Text       .Id]
        }
    }

  implicit lazy val picklerCustomFieldImplication: Pickler[CustomField.Implication] =
    new Pickler[CustomField.Implication] {
      override def pickle(a: CustomField.Implication)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.reqTypeId)
        state.pickle(a.mandatory)
        state.pickle(a.reqTypes)
        state.pickle(a.liveExplicitly)
      }
      override def unpickle(implicit state: UnpickleState): CustomField.Implication = {
        val id             = state.unpickle[CustomField.Implication.Id]
        val reqTypeId      = state.unpickle[ReqTypeId]
        val mandatory      = state.unpickle[Mandatory]
        val reqTypes       = state.unpickle[Field.ApplicableReqTypes]
        val liveExplicitly = state.unpickle[Live]
        CustomField.Implication(id, reqTypeId, mandatory, reqTypes, liveExplicitly)
      }
    }

  implicit lazy val picklerCustomFieldImplicationId: Pickler[CustomField.Implication.Id] =
    pickleTaggedI(CustomField.Implication.Id).reuseByUnivEq

  implicit lazy val picklerCustomFieldTag: Pickler[CustomField.Tag] =
    new Pickler[CustomField.Tag] {
      override def pickle(a: CustomField.Tag)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.tagId)
        state.pickle(a.mandatory)
        state.pickle(a.reqTypes)
        state.pickle(a.liveExplicitly)
      }
      override def unpickle(implicit state: UnpickleState): CustomField.Tag = {
        val id             = state.unpickle[CustomField.Tag.Id]
        val tagId          = state.unpickle[TagId]
        val mandatory      = state.unpickle[Mandatory]
        val reqTypes       = state.unpickle[Field.ApplicableReqTypes]
        val liveExplicitly = state.unpickle[Live]
        CustomField.Tag(id, tagId, mandatory, reqTypes, liveExplicitly)
      }
    }

  implicit lazy val picklerCustomFieldTagId: Pickler[CustomField.Tag.Id] =
    pickleTaggedI(CustomField.Tag.Id).reuseByUnivEq

  implicit lazy val picklerCustomFieldText: Pickler[CustomField.Text] =
    new Pickler[CustomField.Text] {
      override def pickle(a: CustomField.Text)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.name)
        state.pickle(a.key)
        state.pickle(a.mandatory)
        state.pickle(a.reqTypes)
        state.pickle(a.liveExplicitly)
      }
      override def unpickle(implicit state: UnpickleState): CustomField.Text = {
        val id             = state.unpickle[CustomField.Text.Id]
        val name           = state.unpickle[String]
        val key            = state.unpickle[FieldRefKey]
        val mandatory      = state.unpickle[Mandatory]
        val reqTypes       = state.unpickle[Field.ApplicableReqTypes]
        val liveExplicitly = state.unpickle[Live]
        CustomField.Text(id, name, key, mandatory, reqTypes, liveExplicitly)
      }
    }

  implicit lazy val picklerCustomFieldTextId: Pickler[CustomField.Text.Id] =
    pickleTaggedI(CustomField.Text.Id).reuseByUnivEq

  implicit lazy val picklerCustomFieldType: Pickler[CustomFieldType] =
    new Pickler[CustomFieldType] {
      private[this] final val KeyImplication = 'i'
      private[this] final val KeyTag         = 't'
      private[this] final val KeyText        = 'x'
      override def pickle(a: CustomFieldType)(implicit state: PickleState): Unit =
        a match {
          case CustomFieldType.Implication => state.enc.writeByte(KeyImplication)
          case CustomFieldType.Tag         => state.enc.writeByte(KeyTag        )
          case CustomFieldType.Text        => state.enc.writeByte(KeyText       )
        }
      override def unpickle(implicit state: UnpickleState): CustomFieldType =
        state.dec.readByte match {
          case KeyImplication => CustomFieldType.Implication
          case KeyTag         => CustomFieldType.Tag
          case KeyText        => CustomFieldType.Text
        }
    }

  implicit lazy val picklerCustomIssueType: Pickler[CustomIssueType] =
    new Pickler[CustomIssueType] {
      override def pickle(a: CustomIssueType)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.key)
        state.pickle(a.desc)
        state.pickle(a.live)
      }
      override def unpickle(implicit state: UnpickleState): CustomIssueType = {
        val id   = state.unpickle[CustomIssueTypeId]
        val key  = state.unpickle[HashRefKey]
        val desc = state.unpickle[Option[String]]
        val live = state.unpickle[Live]
        CustomIssueType(id, key, desc, live)
      }
    }

  implicit lazy val picklerCustomIssueTypeId: Pickler[CustomIssueTypeId] =
    pickleTaggedI(CustomIssueTypeId).reuseByUnivEq

  implicit lazy val picklerCustomReqTypeId: Pickler[CustomReqTypeId] =
    pickleTaggedI(CustomReqTypeId).reuseByUnivEq

  implicit lazy val picklerCustomIssueTypes: Pickler[CustomIssueTypeIMap] =
    pickleIMapD

  implicit lazy val picklerCustomReqType: Pickler[CustomReqType] =
    new Pickler[CustomReqType] {
      private[this] implicit val picklerSetMnemonics: Pickler[Set[ReqType.Mnemonic]] = iterablePickler
      override def pickle(a: CustomReqType)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.mnemonic)
        state.pickle(a.oldMnemonics)
        state.pickle(a.name)
        state.pickle(a.imp)
        state.pickle(a.live)
      }
      override def unpickle(implicit state: UnpickleState): CustomReqType = {
        val id           = state.unpickle[CustomReqTypeId]
        val mnemonic     = state.unpickle[ReqType.Mnemonic]
        val oldMnemonics = state.unpickle[Set[ReqType.Mnemonic]]
        val name         = state.unpickle[String]
        val imp          = state.unpickle[ImplicationRequired]
        val live         = state.unpickle[Live]
        CustomReqType(id, mnemonic, oldMnemonics, name, imp, live)
      }
    }

  implicit lazy val picklerCustomReqTypes: Pickler[ReqTypes.Custom] =
    pickleIMapD

  implicit lazy val picklerDeadCodeGroup: Pickler[DeadCodeGroup] =
    new Pickler[DeadCodeGroup] {
      override def pickle(a: DeadCodeGroup)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.title)
      }
      override def unpickle(implicit state: UnpickleState): DeadCodeGroup = {
        val id    = state.unpickle[ReqCodeGroupId]
        val title = state.unpickle[Text.CodeGroupTitle.OptionalText]
        DeadCodeGroup(id, title)
      }
    }

  implicit lazy val picklerDeletable: Pickler[Deletable] =
    pickleBool(Deletable)

  implicit lazy val picklerDeletionReasonIdO =
    optionPickler(pickleTaggedI(DeletionReasonId)).reuseByUnivEq

  implicit lazy val picklerDeletionReasons: Pickler[DeletionReasons] =
    new Pickler[DeletionReasons] {
      private[this] implicit val picklerRA: Pickler[DeletionReasons.ReqApplication] = pickleMultimap
      override def pickle(a: DeletionReasons)(implicit state: PickleState): Unit = {
        state.pickle(a.reasons)
        state.pickle(a.reqApplication)
      }
      override def unpickle(implicit state: UnpickleState): DeletionReasons = {
        val reasons        = state.unpickle[Vector[Text.DeletionReason.NonEmptyText]]
        val reqApplication = state.unpickle[DeletionReasons.ReqApplication]
        DeletionReasons(reasons, reqApplication)
      }
    }

  implicit lazy val picklerFieldApplicableReqTypes: Pickler[Field.ApplicableReqTypes] =
    pickleISubset

  implicit lazy val picklerFieldRefKey: Pickler[FieldRefKey] =
    pickleTaggedS(FieldRefKey)

  implicit lazy val picklerFieldId: Pickler[FieldId] =
    new Pickler[FieldId] {
      private[this] final val KeyCustomImplication       = 'i'
      private[this] final val KeyCustomTag               = 't'
      private[this] final val KeyCustomText              = 'x'
      private[this] final val KeyStaticExceptionStepTree = 'E'
      private[this] final val KeyStaticImplicationGraph  = 'I'
      private[this] final val KeyStaticNormalAltStepTree = 'N'
      private[this] final val KeyStaticStepGraph         = 'G'
      override def pickle(a: FieldId)(implicit state: PickleState): Unit =
        a match {
          case b: CustomField.Implication.Id => state.enc.writeByte(KeyCustomImplication          ); state.pickle(b)
          case b: CustomField.Tag        .Id => state.enc.writeByte(KeyCustomTag                  ); state.pickle(b)
          case b: CustomField.Text       .Id => state.enc.writeByte(KeyCustomText                 ); state.pickle(b)
          case StaticField.ExceptionStepTree => state.enc.writeByte(KeyStaticExceptionStepTree    )
          case StaticField.ImplicationGraph  => state.enc.writeByte(KeyStaticImplicationGraph     )
          case StaticField.NormalAltStepTree => state.enc.writeByte(KeyStaticNormalAltStepTree    )
          case StaticField.StepGraph         => state.enc.writeByte(KeyStaticStepGraph            )
        }
      override def unpickle(implicit state: UnpickleState): FieldId =
        state.dec.readByte match {
          case KeyCustomImplication       => state.unpickle[CustomField.Implication.Id]
          case KeyCustomTag               => state.unpickle[CustomField.Tag        .Id]
          case KeyCustomText              => state.unpickle[CustomField.Text       .Id]
          case KeyStaticExceptionStepTree => StaticField.ExceptionStepTree
          case KeyStaticImplicationGraph  => StaticField.ImplicationGraph
          case KeyStaticNormalAltStepTree => StaticField.NormalAltStepTree
          case KeyStaticStepGraph         => StaticField.StepGraph
        }
    }

  implicit lazy val picklerFieldSet: Pickler[FieldSet] =
    new Pickler[FieldSet] {
      override def pickle(a: FieldSet)(implicit state: PickleState): Unit = {
        state.pickle(a.customFields)
        state.pickle(a.order)
      }
      override def unpickle(implicit state: UnpickleState): FieldSet = {
        val customFields = state.unpickle[FieldSet.CustomFields]
        val order        = state.unpickle[FieldSet.Order]
        FieldSet(customFields, order)
      }
    }

  implicit lazy val picklerFieldSetCustomFields: Pickler[FieldSet.CustomFields] =
    pickleIMap(FieldSet.emptyCustomFields)

  implicit lazy val picklerFieldType: Pickler[FieldType] =
    new Pickler[FieldType] {
      private[this] final val KeyImplication      = 'i'
      private[this] final val KeyImplicationGraph = 'I'
      private[this] final val KeyStepGraph        = 'G'
      private[this] final val KeyStepTree         = 'T'
      private[this] final val KeyTag              = 't'
      private[this] final val KeyText             = 'x'
      override def pickle(a: FieldType)(implicit state: PickleState): Unit =
        a match {
          case CustomFieldType.Implication      => state.enc.writeByte(KeyImplication     )
          case StaticFieldType.ImplicationGraph => state.enc.writeByte(KeyImplicationGraph)
          case StaticFieldType.StepGraph        => state.enc.writeByte(KeyStepGraph       )
          case StaticFieldType.StepTree         => state.enc.writeByte(KeyStepTree        )
          case CustomFieldType.Tag              => state.enc.writeByte(KeyTag             )
          case CustomFieldType.Text             => state.enc.writeByte(KeyText            )
        }
      override def unpickle(implicit state: UnpickleState): FieldType =
        state.dec.readByte match {
          case KeyImplication      => CustomFieldType.Implication
          case KeyImplicationGraph => StaticFieldType.ImplicationGraph
          case KeyStepGraph        => StaticFieldType.StepGraph
          case KeyStepTree         => StaticFieldType.StepTree
          case KeyTag              => CustomFieldType.Tag
          case KeyText             => CustomFieldType.Text
        }
    }

  implicit lazy val picklerFilterDead: Pickler[FilterDead] =
    pickleBool(ShowDead)

  implicit lazy val picklerGenericReq: Pickler[GenericReq] =
    new Pickler[GenericReq] {
      override def pickle(a: GenericReq)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.pubid)
        state.pickle(a.title)
        state.pickle(a.liveExplicitly)
      }
      override def unpickle(implicit state: UnpickleState): GenericReq = {
        val id             = state.unpickle[GenericReqId]
        val pubid          = state.unpickle[PubidC]
        val title          = state.unpickle[Text.GenericReqTitle.OptionalText]
        val liveExplicitly = state.unpickle[Live]
        GenericReq(id, pubid, title, liveExplicitly)
      }
    }

  implicit lazy val picklerGenericReqId: Pickler[GenericReqId] =
    pickleTaggedI(GenericReqId).reuseByUnivEq

  implicit lazy val picklerGenericReqsById: Pickler[GenericReqIMap] =
    pickleIMapD

  implicit lazy val picklerHashRefKey: Pickler[HashRefKey] =
    pickleTaggedS(HashRefKey)

  implicit lazy val picklerIdCeilings: Pickler[IdCeilings] =
    new Pickler[IdCeilings] {
      override def pickle(a: IdCeilings)(implicit state: PickleState): Unit = {
        state.pickle(a.customIssueType)
        state.pickle(a.customReqType)
        state.pickle(a.customField)
        state.pickle(a.tag)
        state.pickle(a.req)
        state.pickle(a.useCaseStep)
        state.pickle(a.reqCode)
        state.pickle(a.reqtableView)
      }
      override def unpickle(implicit state: UnpickleState): IdCeilings = {
        val customIssueType = state.unpickle[Int]
        val customReqType   = state.unpickle[Int]
        val customField     = state.unpickle[Int]
        val tag             = state.unpickle[Int]
        val req             = state.unpickle[Int]
        val useCaseStep     = state.unpickle[Int]
        val reqCode         = state.unpickle[Int]
        val reqtableView    = state.unpickle[Int]
        IdCeilings(customIssueType, customReqType, customField, tag, req, useCaseStep, reqCode, reqtableView)
      }
    }

  implicit lazy val picklerImplications: Pickler[Implications] =
    pickleDigraphBiDir

  implicit lazy val picklerImplRequired: Pickler[ImplicationRequired] =
    pickleBool(ImplicationRequired)

  implicit lazy val picklerIssueCategory: Pickler[IssueCategory] =
    new Pickler[IssueCategory] {
      import IssueCategory._
      private[this] final val KeyBadData     = 'b'
      private[this] final val KeyFutility    = 'f'
      private[this] final val KeyMissingData = 'm'
      private[this] final val KeyUserDefined = 'u'
      override def pickle(a: IssueCategory)(implicit state: PickleState): Unit =
        a match {
          case BadData     => state.enc.writeByte(KeyBadData    )
          case Futility    => state.enc.writeByte(KeyFutility   )
          case MissingData => state.enc.writeByte(KeyMissingData)
          case UserDefined => state.enc.writeByte(KeyUserDefined)
        }
      override def unpickle(implicit state: UnpickleState): IssueCategory =
        state.dec.readByte match {
          case KeyBadData     => BadData
          case KeyFutility    => Futility
          case KeyMissingData => MissingData
          case KeyUserDefined => UserDefined
        }
    }

  implicit lazy val picklerLive: Pickler[Live] =
    pickleBool(Live)

  implicit lazy val picklerLiveCodeGroup: Pickler[LiveCodeGroup] =
    new Pickler[LiveCodeGroup] {
      override def pickle(a: LiveCodeGroup)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.title)
      }
      override def unpickle(implicit state: UnpickleState): LiveCodeGroup = {
        val id    = state.unpickle[ReqCodeGroupId]
        val title = state.unpickle[Text.CodeGroupTitle.OptionalText]
        LiveCodeGroup(id, title)
      }
    }

  implicit lazy val picklerMandatory: Pickler[Mandatory] =
    pickleBool(Mandatory)

  implicit lazy val picklerManualIssue: Pickler[ManualIssue] =
    new Pickler[ManualIssue] {
      override def pickle(a: ManualIssue)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.text)
      }
      override def unpickle(implicit state: UnpickleState): ManualIssue = {
        val id   = state.unpickle[ManualIssueId]
        val text = state.unpickle[Text.ManualIssue.NonEmptyText]
        ManualIssue(id, text)
      }
    }

  implicit lazy val picklerManualIssueId: Pickler[ManualIssueId] =
    pickleTaggedI(ManualIssueId)

  implicit lazy val picklerManualIssueIMap: Pickler[ManualIssue.IMap] =
    pickleIMap(ManualIssue.emptyIMap)

  implicit lazy val picklerManualIssues: Pickler[ManualIssues] =
    new Pickler[ManualIssues] {
      override def pickle(a: ManualIssues)(implicit state: PickleState): Unit = {
        state.pickle(a.imap)
        state.pickle(a.nextId)
      }
      override def unpickle(implicit state: UnpickleState): ManualIssues = {
        val imap   = state.unpickle[ManualIssue.IMap]
        val nextId = state.unpickle[ManualIssueId]
        ManualIssues(imap, nextId)
      }
    }

  implicit lazy val picklerMutexChildren: Pickler[MutexChildren] =
    pickleBool(MutexChildren)

  implicit lazy val picklerOn: Pickler[On] =
    pickleBool(On)

  implicit lazy val picklerProject: Pickler[Project] =
    new Pickler[Project] {
      override def pickle(a: Project)(implicit state: PickleState): Unit = {
        state.pickle(a.name)
        state.pickle(a.config)
        state.pickle(a.content)
        state.pickle(a.manualIssues)
        state.pickle(a.reqtableViews)
        state.pickle(a.idCeilings)
      }
      override def unpickle(implicit state: UnpickleState): Project = {
        val name          = state.unpickle[Project.Name]
        val config        = state.unpickle[ProjectConfig]
        val content       = state.unpickle[ProjectContent]
        val manualIssues  = state.unpickle[ManualIssues]
        val reqtableViews = state.unpickle[reqtable.SavedViews.Optional]
        val idCeilings    = state.unpickle[IdCeilings]
        Project(name, config, content, manualIssues, reqtableViews, idCeilings)
      }
    }

  implicit lazy val picklerProjectConfig: Pickler[ProjectConfig] =
    new Pickler[ProjectConfig] {
      override def pickle(a: ProjectConfig)(implicit state: PickleState): Unit = {
        state.pickle(a.customIssueTypes)
        state.pickle(a.reqTypes)
        state.pickle(a.fields)
        state.pickle(a.tags)
      }
      override def unpickle(implicit state: UnpickleState): ProjectConfig = {
        val customIssueTypes = state.unpickle[CustomIssueTypeIMap]
        val reqTypes         = state.unpickle[ReqTypes]
        val fields           = state.unpickle[FieldSet]
        val tags             = state.unpickle[Tags]
        ProjectConfig(customIssueTypes, reqTypes, fields, tags)
      }
    }

  implicit lazy val picklerProjectContent: Pickler[ProjectContent] =
    new Pickler[ProjectContent] {
      override def pickle(a: ProjectContent)(implicit state: PickleState): Unit = {
        state.pickle(a.reqs)
        state.pickle(a.reqCodes)
        state.pickle(a.reqText)
        state.pickle(a.reqTags)
        state.pickle(a.implications)
        state.pickle(a.deletionReasons)
      }
      override def unpickle(implicit state: UnpickleState): ProjectContent = {
        val reqs            = state.unpickle[Requirements]
        val reqCodes        = state.unpickle[ReqCodes]
        val reqText         = state.unpickle[ReqData.Text]
        val reqTags         = state.unpickle[ReqData.Tags]
        val implications    = state.unpickle[Implications.BiDir]
        val deletionReasons = state.unpickle[DeletionReasons]
        ProjectContent(reqs, reqCodes, reqText, reqTags, implications, deletionReasons)
      }
    }

  implicit lazy val picklerProjectIdPublic: Pickler[ProjectId.Public] =
    pickleObfuscated

  implicit lazy val picklerProjectMetaData: Pickler[ProjectMetaData] =
    new Pickler[ProjectMetaData] {
      override def pickle(a: ProjectMetaData)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.name)
        state.pickle(a.initEventCount)
        state.pickle(a.totalEventCount)
        state.pickle(a.reqCount)
        state.pickle(a.createdAt)
        state.pickle(a.lastUpdatedAt)
      }
      override def unpickle(implicit state: UnpickleState): ProjectMetaData = {
        val id              = state.unpickle[ProjectId.Public]
        val name            = state.unpickle[Project.Name]
        val initEventCount  = state.unpickle[Int]
        val totalEventCount = state.unpickle[Int]
        val reqCount        = state.unpickle[Int]
        val createdAt       = state.unpickle[Instant]
        val lastUpdatedAt   = state.unpickle[Option[Instant]]
        ProjectMetaData(id, name, initEventCount, totalEventCount, reqCount, createdAt, lastUpdatedAt)
      }
    }

  implicit lazy val picklerProjectTextContext: Pickler[ProjectText.Context] =
    new Pickler[ProjectText.Context] {
      import ProjectText.Context._
      private[this] implicit val picklerReq: Pickler[Req] = transformPickler(Req.apply)(_.id)
      private[this] final val KeyNone = 0
      private[this] final val KeyReq  = 'r'
      override def pickle(a: ProjectText.Context)(implicit state: PickleState): Unit =
        a match {
          case None    => state.enc.writeByte(KeyNone)
          case b: Req  => state.enc.writeByte(KeyReq ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): ProjectText.Context =
        state.dec.readByte match {
          case KeyNone => None
          case KeyReq  => state.unpickle[Req]
        }
    }

  implicit lazy val picklerPubid: Pickler[Pubid] =
    new Pickler[Pubid] {
      override def pickle(a: Pubid)(implicit state: PickleState): Unit = {
        state.pickle(a.reqTypeId)
        state.pickle(a.pos)
      }
      override def unpickle(implicit state: UnpickleState): Pubid = {
        val reqTypeId = state.unpickle[ReqTypeId]
        val pos       = state.unpickle[ReqTypePos]
        PubidT(reqTypeId, pos)
      }
    }

  implicit def picklerPubidT[T <: ReqTypeId]: Pickler[PubidT[T]] =
    picklerPubid.asInstanceOf[Pickler[PubidT[T]]]

  implicit lazy val picklerPubidRegister: Pickler[PubidRegister] =
    transformPickler(PubidRegister.apply)(_.value)(pickleMultimap)

  implicit lazy val picklerReq: Pickler[Req] =
    new Pickler[Req] {
      private[this] final val KeyGenericReq = 'g'
      private[this] final val KeyUseCase    = 'u'
      override def pickle(a: Req)(implicit state: PickleState): Unit =
        a match {
          case b: GenericReq => state.enc.writeByte(KeyGenericReq); state.pickle(b)
          case b: UseCase    => state.enc.writeByte(KeyUseCase   ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): Req =
        state.dec.readByte match {
          case KeyGenericReq => state.unpickle[GenericReq]
          case KeyUseCase    => state.unpickle[UseCase]
        }
    }

  implicit lazy val picklerReqCodeData: Pickler[ReqCode.Data] = {
    import ReqCode._

    implicit val picklerReqInactive =
      pickleMultimap[ReqId, Set, ApReqCodeId]

    implicit val picklerInactive: Pickler[Inactive] =
      new Pickler[Inactive] {
        override def pickle(a: Inactive)(implicit state: PickleState): Unit = {
          state.pickle(a.deadGroup)
          state.pickle(a.reqInactive)
        }
        override def unpickle(implicit state: UnpickleState): Inactive = {
          val deadGroup   = state.unpickle[DeadGroup]
          val reqInactive = state.unpickle[ReqInactive]
          Inactive(deadGroup, reqInactive)
        }
      }

    implicit val picklerActiveReq: Pickler[ActiveReq] =
      new Pickler[ActiveReq] {
        override def pickle(a: ActiveReq)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.reqId)
          state.pickle(a.deadGroup)
          state.pickle(a.reqInactive)
        }
        override def unpickle(implicit state: UnpickleState): ActiveReq = {
          val id          = state.unpickle[ApReqCodeId]
          val reqId       = state.unpickle[ReqId]
          val deadGroup   = state.unpickle[DeadGroup]
          val reqInactive = state.unpickle[ReqInactive]
          ActiveReq(id, reqId, deadGroup, reqInactive)
        }
      }

    implicit val picklerActiveGroup: Pickler[ActiveGroup] =
      new Pickler[ActiveGroup] {
        override def pickle(a: ActiveGroup)(implicit state: PickleState): Unit = {
          state.pickle(a.group)
          state.pickle(a.reqInactive)
        }
        override def unpickle(implicit state: UnpickleState): ActiveGroup = {
          val group       = state.unpickle[LiveCodeGroup]
          val reqInactive = state.unpickle[ReqInactive]
          ActiveGroup(group, reqInactive)
        }
      }

    new Pickler[Data] {
      private[this] final val KeyActiveGroup = 'g'
      private[this] final val KeyActiveReq   = 'r'
      private[this] final val KeyInactive    = 'i'
      override def pickle(a: Data)(implicit state: PickleState): Unit =
        a match {
          case b: ActiveGroup => state.enc.writeByte(KeyActiveGroup); state.pickle(b)
          case b: ActiveReq   => state.enc.writeByte(KeyActiveReq  ); state.pickle(b)
          case b: Inactive    => state.enc.writeByte(KeyInactive   ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): Data =
        state.dec.readByte match {
          case KeyActiveGroup => state.unpickle[ActiveGroup]
          case KeyActiveReq   => state.unpickle[ActiveReq]
          case KeyInactive    => state.unpickle[Inactive]
        }
    }
  }

  implicit lazy val picklerReqCodeGroupId: Pickler[ReqCodeGroupId] =
    pickleTaggedI(ReqCodeGroupId).reuseByUnivEq

  implicit lazy val picklerReqCodeId: Pickler[ReqCodeId] =
    new Pickler[ReqCodeId] {
      private[this] final val KeyApReqCodeId    = 'a'
      private[this] final val KeyReqCodeGroupId = 'g'
      override def pickle(a: ReqCodeId)(implicit state: PickleState): Unit =
        a match {
          case b: ApReqCodeId    => state.enc.writeByte(KeyApReqCodeId   ); state.pickle(b)
          case b: ReqCodeGroupId => state.enc.writeByte(KeyReqCodeGroupId); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): ReqCodeId =
        state.dec.readByte match {
          case KeyApReqCodeId    => state.unpickle[ApReqCodeId]
          case KeyReqCodeGroupId => state.unpickle[ReqCodeGroupId]
        }
    }

  implicit lazy val picklerReqCodeNode: Pickler[ReqCode.Node] =
    transformPickler(ReqCode.Node.apply)(_.value) // xmap[String] already reuses

  implicit lazy val picklerReqCodes: Pickler[ReqCodes] =
    transformPickler(ReqCodes.apply)(_.trie)

  implicit lazy val picklerReqCodeTrie: Pickler[ReqCode.Trie] =
    pickleTrie

  implicit lazy val picklerReqCodeValue: Pickler[ReqCode.Value] =
    pickleNEV

  implicit lazy val picklerReqDataTags: Pickler[ReqData.Tags] =
    pickleMultimap

  implicit lazy val picklerReqDataText: Pickler[ReqData.Text] =
    pickleMap

  implicit lazy val picklerReqId: Pickler[ReqId] =
    new Pickler[ReqId] {
      private[this] final val KeyGenericReqId = 'g'
      private[this] final val KeyUseCaseId    = 'u'
      override def pickle(a: ReqId)(implicit state: PickleState): Unit =
        a match {
          case b: GenericReqId => state.enc.writeByte(KeyGenericReqId); state.pickle(b)
          case b: UseCaseId    => state.enc.writeByte(KeyUseCaseId   ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): ReqId =
        state.dec.readByte match {
          case KeyGenericReqId => state.unpickle[GenericReqId]
          case KeyUseCaseId    => state.unpickle[UseCaseId]
        }
    }

  implicit lazy val picklerReqIdsByDirection: Pickler[Direction.Values[Set[ReqId]]] =
    pickleIsoBoolValues

  implicit lazy val picklerReqOrSubReqId: Pickler[ReqOrSubReqId] =
    new Pickler[ReqOrSubReqId] {
      private[this] final val KeyGenericReqId  = 'g'
      private[this] final val KeyUseCaseId     = 'u'
      private[this] final val KeyUseCaseStepId = 's'
      override def pickle(a: ReqOrSubReqId)(implicit state: PickleState): Unit =
        a match {
          case b: GenericReqId  => state.enc.writeByte(KeyGenericReqId ); state.pickle(b)
          case b: UseCaseId     => state.enc.writeByte(KeyUseCaseId    ); state.pickle(b)
          case b: UseCaseStepId => state.enc.writeByte(KeyUseCaseStepId); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): ReqOrSubReqId =
        state.dec.readByte match {
          case KeyGenericReqId  => state.unpickle[GenericReqId]
          case KeyUseCaseId     => state.unpickle[UseCaseId]
          case KeyUseCaseStepId => state.unpickle[UseCaseStepId]
        }
    }

  implicit lazy val picklerReqTypeMnemonic: Pickler[ReqType.Mnemonic] =
    pickleTaggedS(ReqType.Mnemonic)

  implicit lazy val picklerReqTypeId: Pickler[ReqTypeId] =
    new Pickler[ReqTypeId] {
      private[this] final val KeyCustomReqTypeId = 'c'
      private[this] final val KeyUseCase         = 'u'
      override def pickle(a: ReqTypeId)(implicit state: PickleState): Unit =
        a match {
          case b: CustomReqTypeId    => state.enc.writeByte(KeyCustomReqTypeId); state.pickle(b)
          case StaticReqType.UseCase => state.enc.writeByte(KeyUseCase        )
        }
      override def unpickle(implicit state: UnpickleState): ReqTypeId =
        state.dec.readByte match {
          case KeyCustomReqTypeId => state.unpickle[CustomReqTypeId]
          case KeyUseCase         => StaticReqType.UseCase
        }
    }

  implicit lazy val picklerReqTypePos: Pickler[ReqTypePos] =
    pickleTaggedI(ReqTypePos)

  implicit lazy val picklerReqTypes: Pickler[ReqTypes] =
    transformPickler(ReqTypes.apply)(_.custom)

  implicit lazy val picklerRequirements: Pickler[Requirements] =
    new Pickler[Requirements] {
      override def pickle(a: Requirements)(implicit state: PickleState): Unit = {
        state.pickle(a.genericReqs)
        state.pickle(a.useCases)
        state.pickle(a.pubids)
      }
      override def unpickle(implicit state: UnpickleState): Requirements = {
        val genericReqs = state.unpickle[GenericReqIMap]
        val useCases    = state.unpickle[UseCases]
        val pubids      = state.unpickle[PubidRegister]
        Requirements(genericReqs, useCases, pubids)
      }
    }

  implicit lazy val picklerStaticField: Pickler[StaticField] =
    new Pickler[StaticField] {
      import StaticField._
      private[this] final val KeyExceptionStepTree = 'e'
      private[this] final val KeyImplicationGraph  = 'i'
      private[this] final val KeyNormalAltStepTree = 'n'
      private[this] final val KeyStepGraph         = 'g'
      override def pickle(a: StaticField)(implicit state: PickleState): Unit =
        a match {
          case ExceptionStepTree => state.enc.writeByte(KeyExceptionStepTree)
          case ImplicationGraph  => state.enc.writeByte(KeyImplicationGraph )
          case NormalAltStepTree => state.enc.writeByte(KeyNormalAltStepTree)
          case StepGraph         => state.enc.writeByte(KeyStepGraph        )
        }
      override def unpickle(implicit state: UnpickleState): StaticField =
        state.dec.readByte match {
          case KeyExceptionStepTree => ExceptionStepTree
          case KeyImplicationGraph  => ImplicationGraph
          case KeyNormalAltStepTree => NormalAltStepTree
          case KeyStepGraph         => StepGraph
        }
    }

  implicit lazy val picklerStaticFieldType: Pickler[StaticFieldType] =
    new Pickler[StaticFieldType] {
      private[this] final val KeyImplicationGraph = 'i'
      private[this] final val KeyStepGraph        = 'g'
      private[this] final val KeyStepTree         = 't'
      override def pickle(a: StaticFieldType)(implicit state: PickleState): Unit =
        a match {
          case StaticFieldType.ImplicationGraph => state.enc.writeByte(KeyImplicationGraph)
          case StaticFieldType.StepGraph        => state.enc.writeByte(KeyStepGraph       )
          case StaticFieldType.StepTree         => state.enc.writeByte(KeyStepTree        )
        }
      override def unpickle(implicit state: UnpickleState): StaticFieldType =
        state.dec.readByte match {
          case KeyImplicationGraph => StaticFieldType.ImplicationGraph
          case KeyStepGraph        => StaticFieldType.StepGraph
          case KeyStepTree         => StaticFieldType.StepTree
        }
    }

  implicit lazy val picklerStaticReqType: Pickler[StaticReqType] =
    new Pickler[StaticReqType] {
      import StaticReqType._
      private[this] final val KeyUseCase = 'u'
      override def pickle(a: StaticReqType)(implicit state: PickleState): Unit =
        a match {
          case UseCase => state.enc.writeByte(KeyUseCase)
        }
      override def unpickle(implicit state: UnpickleState): StaticReqType =
        state.dec.readByte match {
          case KeyUseCase => UseCase
        }
    }

  implicit lazy val picklerSubReqId: Pickler[SubReqId] =
    new Pickler[SubReqId] {
      private[this] final val KeyUseCaseStepId = 's'
      override def pickle(a: SubReqId)(implicit state: PickleState): Unit =
        a match {
          case b: UseCaseStepId => state.enc.writeByte(KeyUseCaseStepId); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): SubReqId =
        state.dec.readByte match {
          case KeyUseCaseStepId => state.unpickle[UseCaseStepId]
        }
    }

  implicit lazy val picklerTag: Pickler[Tag] =
    new Pickler[Tag] {
      private[this] final val KeyApplicableTag = 'a'
      private[this] final val KeyTagGroup      = 'g'
      override def pickle(a: Tag)(implicit state: PickleState): Unit =
        a match {
          case b: ApplicableTag => state.enc.writeByte(KeyApplicableTag); state.pickle(b)
          case b: TagGroup      => state.enc.writeByte(KeyTagGroup     ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): Tag =
        state.dec.readByte match {
          case KeyApplicableTag => state.unpickle[ApplicableTag]
          case KeyTagGroup      => state.unpickle[TagGroup]
        }
    }

  implicit lazy val picklerTagGroup: Pickler[TagGroup] =
    new Pickler[TagGroup] {
      override def pickle(a: TagGroup)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.name)
        state.pickle(a.desc)
        state.pickle(a.mutexChildren)
        state.pickle(a.live)
      }
      override def unpickle(implicit state: UnpickleState): TagGroup = {
        val id            = state.unpickle[TagGroupId]
        val name          = state.unpickle[String]
        val desc          = state.unpickle[Option[String]]
        val mutexChildren = state.unpickle[MutexChildren]
        val live          = state.unpickle[Live]
        TagGroup(id, name, desc, mutexChildren, live)
      }
    }

  implicit lazy val picklerTagGroupId: Pickler[TagGroupId] =
    pickleTaggedI(TagGroupId).reuseByUnivEq

  implicit lazy val picklerTagId: Pickler[TagId] =
    new Pickler[TagId] {
      private[this] final val KeyApplicableTagId = 'a'
      private[this] final val KeyTagGroupId      = 'g'
      override def pickle(a: TagId)(implicit state: PickleState): Unit =
        a match {
          case b: ApplicableTagId => state.enc.writeByte(KeyApplicableTagId); state.pickle(b)
          case b: TagGroupId      => state.enc.writeByte(KeyTagGroupId     ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): TagId =
        state.dec.readByte match {
          case KeyApplicableTagId => state.unpickle[ApplicableTagId]
          case KeyTagGroupId      => state.unpickle[TagGroupId]
        }
    }

  implicit lazy val picklerTagInTree: Pickler[TagInTree] =
    new Pickler[TagInTree] {
      override def pickle(a: TagInTree)(implicit state: PickleState): Unit = {
        state.pickle(a.tag)
        state.pickle(a.children)
      }
      override def unpickle(implicit state: UnpickleState): TagInTree = {
        val tag      = state.unpickle[Tag]
        val children = state.unpickle[TagInTree.Children]
        TagInTree(tag, children)
      }
    }

  implicit lazy val picklerTags: Pickler[Tags] =
    transformPickler(Tags.apply)(_.tree)

  implicit lazy val picklerTagPovRelations: Pickler[TagInTree.Relations] =
    pickleMMTreeRelations

  implicit lazy val picklerTagTree: Pickler[TagTree] =
    pickleIMap(TagTree.empty)

  implicit lazy val picklerUseCase: Pickler[UseCase] =
    new Pickler[UseCase] {
      override def pickle(a: UseCase)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.pos)
        state.pickle(a.title)
        state.pickle(a.stepsNA)
        state.pickle(a.stepsE)
        state.pickle(a.liveExplicitly)
      }
      override def unpickle(implicit state: UnpickleState): UseCase = {
        val id             = state.unpickle[UseCaseId]
        val pos            = state.unpickle[ReqTypePos]
        val title          = state.unpickle[Text.UseCaseTitle.OptionalText]
        val stepsNA        = state.unpickle[UseCaseSteps]
        val stepsE         = state.unpickle[UseCaseSteps]
        val liveExplicitly = state.unpickle[Live]
        UseCase(id, pos, title, stepsNA, stepsE, liveExplicitly)
      }
    }

  implicit lazy val picklerUseCaseId: Pickler[UseCaseId] =
    pickleTaggedI(UseCaseId).reuseByUnivEq

  implicit lazy val picklerUseCases: Pickler[UseCases] =
    picklerUseCasesStateless imap UseCases.statelessIso

  implicit lazy val picklerUseCasesStateless: Pickler[UseCases.Stateless] =
    new Pickler[UseCases.Stateless] {
      override def pickle(a: UseCases.Stateless)(implicit state: PickleState): Unit = {
        state.pickle(a.imap)
        state.pickle(a.stepFlow)
      }
      override def unpickle(implicit state: UnpickleState): UseCases.Stateless = {
        val imap     = state.unpickle[UseCaseIMap]
        val stepFlow = state.unpickle[UseCases.StepFlow]
        UseCases.Stateless(imap, stepFlow)
      }
    }

  implicit lazy val picklerUseCasesById: Pickler[UseCaseIMap] =
    pickleIMapD

  implicit lazy val picklerUseCaseStep: Pickler[UseCaseStep] =
    new Pickler[UseCaseStep] {
      override def pickle(a: UseCaseStep)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.titleExplicitly)
        state.pickle(a.liveExplicitly)
      }
      override def unpickle(implicit state: UnpickleState): UseCaseStep = {
        val id              = state.unpickle[UseCaseStepId]
        val titleExplicitly = state.unpickle[Text.UseCaseStep.OptionalText]
        val liveExplicitly  = state.unpickle[Live]
        UseCaseStep(id, titleExplicitly, liveExplicitly)
      }
    }

  implicit lazy val picklerUseCasesStepFlow: Pickler[UseCases.StepFlow] =
    pickleDigraphBiDir

  implicit lazy val picklerUseCaseStepId: Pickler[UseCaseStepId] =
    pickleTaggedI(UseCaseStepId).reuseByUnivEq

  implicit lazy val picklerUseCaseSteps: Pickler[UseCaseSteps] =
    transformPickler(UseCaseSteps.apply)(_.tree)(pickleVectorTree)

  implicit lazy val pickleValidFilter: Pickler[Filter.Valid] = {
    import shipreq.webapp.base.filter.{IntensionalReqSet, FilterAst}
    import Filter._
    import Filter.Implicits._

    implicit val picklerNonEmptyVectorUnit: Pickler[NonEmptyVector[Unit]] =
      implicitly[Pickler[Int]].xmap(NonEmptyVector force Vector.fill(_)(()))(_.length)

    implicit val picklerNonEmptySetInt: Pickler[NonEmptySet[Int]] =
      pickleNES

    implicit def picklerIRSetS[RT: Pickler]: Pickler[IntensionalReqSet.SomeOfType[RT]] =
      new Pickler[IntensionalReqSet.SomeOfType[RT]] {
        override def pickle(a: IntensionalReqSet.SomeOfType[RT])(implicit state: PickleState): Unit = {
          state.pickle(a.reqType)
          state.pickle(a.numbers)
        }
        override def unpickle(implicit state: UnpickleState): IntensionalReqSet.SomeOfType[RT] = {
          val reqType = state.unpickle[RT]
          val numbers = state.unpickle[NonEmptySet[Int]]
          IntensionalReqSet.SomeOfType(reqType, numbers)
        }
      }

    implicit def picklerIRSetW[RT: Pickler]: Pickler[IntensionalReqSet.WholeType[RT]] =
      transformPickler(IntensionalReqSet.WholeType.apply[RT])(_.reqType)

    def picklerIRSet[RT: Pickler]: Pickler[IntensionalReqSet[RT]] =
      new Pickler[IntensionalReqSet[RT]] {
        import IntensionalReqSet._
        private[this] final val KeySomeOfType = 0
        private[this] final val KeyWholeType  = 1
        override def pickle(a: IntensionalReqSet[RT])(implicit state: PickleState): Unit =
          a match {
            case b: SomeOfType[RT] => state.enc.writeByte(KeySomeOfType); state.pickle(b)
            case b: WholeType[RT]  => state.enc.writeByte(KeyWholeType ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): IntensionalReqSet[RT] =
          state.dec.readByte match {
            case KeySomeOfType => state.unpickle[SomeOfType[RT]]
            case KeyWholeType  => state.unpickle[WholeType[RT]]
          }
      }

    implicit val picklerValidHashTag: Pickler[Valid.HashTag] =
      pickleDisj

    implicit val picklerValidIssueCatNEV: Pickler[NonEmptyVector[Valid.IssueCat]] =
      pickleNEV

    implicit val picklerValidReqSubset: Pickler[Valid.ReqSubset] =
      picklerIRSet

    implicit val picklerValidReqSet: Pickler[Valid.ReqSet] =
      pickleNEV

    implicit val picklerFilterAstAttr: Pickler[FilterAst.Attr] =
      new Pickler[FilterAst.Attr] {
        private[this] final val KeyAnyIssue = 'i'
        private[this] final val KeyAnyTag   = 't'
        override def pickle(a: FilterAst.Attr)(implicit state: PickleState): Unit =
          a match {
            case FilterAst.Attr.AnyIssue => state.enc.writeByte(KeyAnyIssue)
            case FilterAst.Attr.AnyTag   => state.enc.writeByte(KeyAnyTag  )
          }
        override def unpickle(implicit state: UnpickleState): FilterAst.Attr =
          state.dec.readByte match {
            case KeyAnyIssue => FilterAst.Attr.AnyIssue
            case KeyAnyTag   => FilterAst.Attr.AnyTag
          }
      }

    implicit val picklerFilterAstText: Pickler[FilterAst.Text] =
      new Pickler[FilterAst.Text] {
        override def pickle(a: FilterAst.Text)(implicit state: PickleState): Unit = {
          state.pickle(a.text)
          state.pickle(a.quoteChar)
        }
        override def unpickle(implicit state: UnpickleState): FilterAst.Text = {
          val text      = state.unpickle[String]
          val quoteChar = state.unpickle[Option[Char]]
          FilterAst.Text(text, quoteChar)
        }
      }

    implicit val picklerFilterAstRegex: Pickler[FilterAst.Regex] =
      transformPickler(FilterAst.Regex.apply)(_.text)

    implicit val picklerFilterAstPresence: Pickler[FilterAst.Presence[Valid.Attr]] =
      transformPickler(FilterAst.Presence.apply[Valid.Attr])(_.attr)

    implicit val picklerFilterAstHasIssue: Pickler[FilterAst.HasIssue[Valid.IssueCat]] =
      new Pickler[FilterAst.HasIssue[Valid.IssueCat]] {
        override def pickle(a: FilterAst.HasIssue[Valid.IssueCat])(implicit state: PickleState): Unit = {
          state.pickle(a.on)
          state.pickle(a.criteria)
        }
        override def unpickle(implicit state: UnpickleState): FilterAst.HasIssue[Valid.IssueCat] = {
          val on       = state.unpickle[On]
          val criteria = state.unpickle[NonEmptyVector[Valid.IssueCat]]
          FilterAst.HasIssue(on, criteria)
        }
      }

    implicit val picklerFilterAstHashRef: Pickler[FilterAst.HashRef[Valid.HashTag]] =
      transformPickler(FilterAst.HashRef.apply[Valid.HashTag])(_.value)

    implicit val picklerFilterAstImpliesAnyOf: Pickler[FilterAst.ImpliesAnyOf[Valid.ReqSet]] =
      transformPickler(FilterAst.ImpliesAnyOf.apply[Valid.ReqSet])(_.reqs)

    implicit val picklerFilterAstImpliedByAnyOf: Pickler[FilterAst.ImpliedByAnyOf[Valid.ReqSet]] =
      transformPickler(FilterAst.ImpliedByAnyOf.apply[Valid.ReqSet])(_.reqs)

    implicit val picklerFilterAstReqs: Pickler[FilterAst.Reqs[Valid.ReqSet]] =
      transformPickler(FilterAst.Reqs.apply[Valid.ReqSet])(_.reqs)

    implicit val picklerFilterAstReqType: Pickler[FilterAst.ReqType[Valid.ReqType]] =
      transformPickler(FilterAst.ReqType.apply[Valid.ReqType])(_.reqType)

    implicit val picklerFilterAstNot: Pickler[FilterAst.Not[Unit]] =
      transformPickler(FilterAst.Not.apply[Unit])(_.clause)

    implicit val picklerFilterAstAllOf: Pickler[FilterAst.AllOf[Unit]] =
      transformPickler(FilterAst.AllOf.apply[Unit])(_.clauses)

    implicit val picklerFilterAstAnyOf: Pickler[FilterAst.AnyOf[Unit]] =
      new Pickler[FilterAst.AnyOf[Unit]] {
        override def pickle(a: FilterAst.AnyOf[Unit])(implicit state: PickleState): Unit = {
          state.pickle(a.tail)
        }
        override def unpickle(implicit state: UnpickleState): FilterAst.AnyOf[Unit] = {
          val tail = state.unpickle[NonEmptyVector[Unit]]
          FilterAst.AnyOf((), tail)
        }
      }

    implicit val picklerValidF: Pickler[ValidF[Unit]] =
      new Pickler[ValidF[Unit]] {
        private[this] final val KeyAllOf          = 0
        private[this] final val KeyAnyOf          = 1
        private[this] final val KeyHasIssue       = 2
        private[this] final val KeyHashRef        = 3
        private[this] final val KeyImpliedByAnyOf = 4
        private[this] final val KeyImpliesAnyOf   = 5
        private[this] final val KeyNot            = 6
        private[this] final val KeyPresence       = 7
        private[this] final val KeyRegex          = 8
        private[this] final val KeyReqType        = 9
        private[this] final val KeyReqs           = 10
        private[this] final val KeyText           = 11
        override def pickle(a: ValidF[Unit])(implicit state: PickleState): Unit =
          a match {
            case b: FilterAst.AllOf         [Unit]           => state.enc.writeByte(KeyAllOf         ); state.pickle(b)
            case b: FilterAst.AnyOf         [Unit]           => state.enc.writeByte(KeyAnyOf         ); state.pickle(b)
            case b: FilterAst.HasIssue      [Valid.IssueCat] => state.enc.writeByte(KeyHasIssue      ); state.pickle(b)
            case b: FilterAst.HashRef       [Valid.HashTag]  => state.enc.writeByte(KeyHashRef       ); state.pickle(b)
            case b: FilterAst.ImpliedByAnyOf[Valid.ReqSet]   => state.enc.writeByte(KeyImpliedByAnyOf); state.pickle(b)
            case b: FilterAst.ImpliesAnyOf  [Valid.ReqSet]   => state.enc.writeByte(KeyImpliesAnyOf  ); state.pickle(b)
            case b: FilterAst.Not           [Unit]           => state.enc.writeByte(KeyNot           ); state.pickle(b)
            case b: FilterAst.Presence      [Valid.Attr]     => state.enc.writeByte(KeyPresence      ); state.pickle(b)
            case b: FilterAst.Regex                          => state.enc.writeByte(KeyRegex         ); state.pickle(b)
            case b: FilterAst.ReqType       [Valid.ReqType]  => state.enc.writeByte(KeyReqType       ); state.pickle(b)
            case b: FilterAst.Reqs          [Valid.ReqSet]   => state.enc.writeByte(KeyReqs          ); state.pickle(b)
            case b: FilterAst.Text                           => state.enc.writeByte(KeyText          ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): ValidF[Unit] =
          state.dec.readByte match {
            case KeyAllOf          => state.unpickle[FilterAst.AllOf         [Unit          ]]
            case KeyAnyOf          => state.unpickle[FilterAst.AnyOf         [Unit          ]]
            case KeyHasIssue       => state.unpickle[FilterAst.HasIssue      [Valid.IssueCat]]
            case KeyHashRef        => state.unpickle[FilterAst.HashRef       [Valid.HashTag ]]
            case KeyImpliedByAnyOf => state.unpickle[FilterAst.ImpliedByAnyOf[Valid.ReqSet  ]]
            case KeyImpliesAnyOf   => state.unpickle[FilterAst.ImpliesAnyOf  [Valid.ReqSet  ]]
            case KeyNot            => state.unpickle[FilterAst.Not           [Unit          ]]
            case KeyPresence       => state.unpickle[FilterAst.Presence      [Valid.Attr    ]]
            case KeyRegex          => state.unpickle[FilterAst.Regex                         ]
            case KeyReqType        => state.unpickle[FilterAst.ReqType       [Valid.ReqType ]]
            case KeyReqs           => state.unpickle[FilterAst.Reqs          [Valid.ReqSet  ]]
            case KeyText           => state.unpickle[FilterAst.Text                          ]
          }
      }

    pickleFix[ValidF]
  }

}
