package shipreq.webapp.base.protocol.binary.v1

import boopickle.ConstPickler
import boopickle.DefaultBasic._
import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.univeq.UnivEq
import nyaya.util.Multimap
import shipreq.base.util.{Applicable, Exclusive, Exclusivity, IMap, NotApplicable}
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.issue.IssueCategory
import shipreq.webapp.base.sort.SortMethod
import shipreq.webapp.base.text.AtomTC

/** This is the minimum set of codecs required for event codecs.
  *
  * Events (and their dependencies) are expected to be extremely stable and only change very, very rarely if ever.
  */
object BaseMemberData1 {
  import BaseData._

  // ===================================================================================================================
  // Polymorphic definitions
  // (non-implicit, "pickle" prefix)

  def pickleIMapD[K: UnivEq, V: Pickler](implicit d: DataIdAux[V, K]): Pickler[IMap[K, V]] =
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

    override def sum[T <: Atom.Base](t: T)(get: Atom.Type => Pickler[t.Atom], all: List[Pickler[t.Atom]]): Pickler[t.Atom] =
      new Pickler[t.Atom] {
        private[this] final val KeyBlankLine      = '0'
        private[this] final val KeyCodeBlock      = '{'
        private[this] final val KeyCodeRef        = 'c'
        private[this] final val KeyEmailAddress   = '@'
        private[this] final val KeyIssue          = 'i'
        private[this] final val KeyLiteral        = 'l'
        private[this] final val KeyMonospace      = '`'
        private[this] final val KeyReqRef         = 'r'
        private[this] final val KeyTagRef         = 't'
        private[this] final val KeyTeX            = 'X'
        private[this] final val KeyUnorderedList  = '*'
        private[this] final val KeyUseCaseStepRef = 'u'
        private[this] final val KeyWebAddress     = '/'
        override def pickle(a: t.Atom)(implicit state: PickleState): Unit = {
          Atom.Type.of(a) match {
            case t@ Type.Literal        => state.enc.writeByte(KeyLiteral       ); get(t).pickle(a)
            case t@ Type.BlankLine      => state.enc.writeByte(KeyBlankLine     ); get(t).pickle(a)
            case t@ Type.CodeBlock      => state.enc.writeByte(KeyCodeBlock     ); get(t).pickle(a)
            case t@ Type.CodeRef        => state.enc.writeByte(KeyCodeRef       ); get(t).pickle(a)
            case t@ Type.EmailAddress   => state.enc.writeByte(KeyEmailAddress  ); get(t).pickle(a)
            case t@ Type.Issue          => state.enc.writeByte(KeyIssue         ); get(t).pickle(a)
            case t@ Type.Monospace      => state.enc.writeByte(KeyMonospace     ); get(t).pickle(a)
            case t@ Type.ReqRef         => state.enc.writeByte(KeyReqRef        ); get(t).pickle(a)
            case t@ Type.TagRef         => state.enc.writeByte(KeyTagRef        ); get(t).pickle(a)
            case t@ Type.TeX            => state.enc.writeByte(KeyTeX           ); get(t).pickle(a)
            case t@ Type.UnorderedList  => state.enc.writeByte(KeyUnorderedList ); get(t).pickle(a)
            case t@ Type.UseCaseStepRef => state.enc.writeByte(KeyUseCaseStepRef); get(t).pickle(a)
            case t@ Type.WebAddress     => state.enc.writeByte(KeyWebAddress    ); get(t).pickle(a)
          }
        }
        override def unpickle(implicit state: UnpickleState): t.Atom = {
          state.dec.readByte match {
            case KeyLiteral        => get(Type.Literal       ).unpickle
            case KeyBlankLine      => get(Type.BlankLine     ).unpickle
            case KeyCodeBlock      => get(Type.CodeBlock     ).unpickle
            case KeyCodeRef        => get(Type.CodeRef       ).unpickle
            case KeyEmailAddress   => get(Type.EmailAddress  ).unpickle
            case KeyIssue          => get(Type.Issue         ).unpickle
            case KeyMonospace      => get(Type.Monospace     ).unpickle
            case KeyReqRef         => get(Type.ReqRef        ).unpickle
            case KeyTagRef         => get(Type.TagRef        ).unpickle
            case KeyTeX            => get(Type.TeX           ).unpickle
            case KeyUnorderedList  => get(Type.UnorderedList ).unpickle
            case KeyUseCaseStepRef => get(Type.UseCaseStepRef).unpickle
            case KeyWebAddress     => get(Type.WebAddress    ).unpickle
          }
        }
      }

    override def blankLine[T <: NewLine](t: T): Pickler[t.BlankLine] =
      ConstPickler(t.blankLine)

    override def literal[T <: Literal](t: T): Pickler[t.Literal] =
      transformPickler((i: String) => t.Literal(i))(_.value)

    override def codeBlock[T <: CodeBlock](t: T): Pickler[t.CodeBlock] =
      new Pickler[t.CodeBlock] {
        override def pickle(a: t.CodeBlock)(implicit state: PickleState): Unit = {
          state.pickle(a.language)
          state.pickle(a.code)
        }
        override def unpickle(implicit state: UnpickleState): t.CodeBlock = {
          val language = state.unpickle[Option[String]]
          val code     = state.unpickle[String]
          t.CodeBlock(language, code)
        }
      }

    override def monospace[T <: PlainTextMarkup](t: T): Pickler[t.Monospace] =
      transformPickler((i: String) => t.Monospace(i))(_.value)

    override def webAddress[T <: PlainTextMarkup](t: T): Pickler[t.WebAddress] =
      transformPickler((i: String) => t.WebAddress(i))(_.value)

    override def emailAddress[T <: PlainTextMarkup](t: T): Pickler[t.EmailAddress] =
      transformPickler((i: String) => t.EmailAddress(i))(_.value)

    override def teX[T <: PlainTextMarkup](t: T): Pickler[t.TeX] =
      transformPickler((i: String) => t.TeX(i))(_.value)

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

    // Replaced by v1.1
//    implicit val picklerView: Pickler[View] =
//      new Pickler[View] {
//        override def pickle(a: View)(implicit state: PickleState): Unit = {
//          state.pickle(a.columns)
//          state.pickle(a.order)
//          state.pickle(a.filterDead)
//          state.pickle(a.filter)
//        }
//        override def unpickle(implicit state: UnpickleState): View = {
//          val columns    = state.unpickle[NonEmptyVector[Column]]
//          val order      = state.unpickle[SortCriteria]
//          val filterDead = state.unpickle[FilterDead]
//          val filter     = state.unpickle[Option[Filter.Valid]]
//          View(columns, order, filterDead, filter)
//        }
//      }

    implicit val picklerSavedViewId: Pickler[SavedView.Id] =
      transformPickler(SavedView.Id.apply)(_.value)

    implicit val picklerSavedViewName: Pickler[SavedView.Name] =
      transformPickler(SavedView.Name.apply)(_.value)

    // Replaced by v1.1
//    implicit val picklerSavedView: Pickler[SavedView] =
//      new Pickler[SavedView] {
//        override def pickle(a: SavedView)(implicit state: PickleState): Unit = {
//          state.pickle(a.id)
//          state.pickle(a.name)
//          state.pickle(a.view)
//        }
//        override def unpickle(implicit state: UnpickleState): SavedView = {
//          val id   = state.unpickle[SavedView.Id]
//          val name = state.unpickle[SavedView.Name]
//          val view = state.unpickle[View]
//          SavedView(id, name, view)
//        }
//      }
//
//    implicit val pickleSavedViewsND: Pickler[SavedViews.NonDefault] =
//      pickleIMap(SavedViews.emptyNonDefault)
//
//    implicit val pickleSavedViews: Pickler[SavedViews.NonEmpty] =
//      new Pickler[SavedViews.NonEmpty] {
//        override def pickle(a: SavedViews.NonEmpty)(implicit state: PickleState): Unit = {
//          state.pickle(a.default)
//          state.pickle(a.nonDefault)
//        }
//        override def unpickle(implicit state: UnpickleState): SavedViews.NonEmpty = {
//          val default    = state.unpickle[SavedView]
//          val nonDefault = state.unpickle[SavedViews.NonDefault]
//          SavedViews.NonEmpty(default, nonDefault)
//        }
//      }
  }

  // Note: This has been designed to be identical to ISubset[ReqTypeId] which is what it's meant to replace.
  implicit lazy val picklerApplicableReqTypes: Pickler[ApplicableReqTypes] =
    new Pickler[ApplicableReqTypes] {
      private[this] final val KeyAll  = 'a'
      private[this] final val KeyNot  = 'n'
      private[this] final val KeyOnly = 'o'

      override def pickle(a: ApplicableReqTypes)(implicit state: PickleState): Unit =
        if (a.isEmpty)
          state.enc.writeByte(KeyAll)
        else {
          state.enc.writeByte(if (a.applicability is Applicable) KeyOnly else KeyNot)
          state.pickle(a.reqTypes)
        }

      override def unpickle(implicit state: UnpickleState): ApplicableReqTypes =
        state.dec.readByte match {
          case KeyAll  => ApplicableReqTypes.empty
          case KeyNot  => ApplicableReqTypes(NotApplicable, state.unpickle[Set[ReqTypeId]])
          case KeyOnly => ApplicableReqTypes(Applicable,    state.unpickle[Set[ReqTypeId]])
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

  implicit lazy val picklerCustomFieldImplicationId: Pickler[CustomField.Implication.Id] =
    pickleTaggedI(CustomField.Implication.Id).reuseByUnivEq

  implicit lazy val picklerCustomFieldTagId: Pickler[CustomField.Tag.Id] =
    pickleTaggedI(CustomField.Tag.Id).reuseByUnivEq

  implicit lazy val picklerCustomFieldTextId: Pickler[CustomField.Text.Id] =
    pickleTaggedI(CustomField.Text.Id).reuseByUnivEq

  implicit lazy val picklerCustomIssueTypeId: Pickler[CustomIssueTypeId] =
    pickleTaggedI(CustomIssueTypeId).reuseByUnivEq

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

  implicit lazy val picklerCustomReqTypeId: Pickler[CustomReqTypeId] =
    pickleTaggedI(CustomReqTypeId).reuseByUnivEq

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

  implicit lazy val picklerFilterDead: Pickler[FilterDead] =
    pickleBool(ShowDead)

  implicit lazy val picklerGenericReqId: Pickler[GenericReqId] =
    pickleTaggedI(GenericReqId).reuseByUnivEq

  implicit lazy val picklerHashRefKey: Pickler[HashRefKey] =
    pickleTaggedS(HashRefKey)

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

  implicit lazy val picklerMandatory: Pickler[Mandatory] =
    pickleBool(Mandatory)

  implicit lazy val picklerManualIssueId: Pickler[ManualIssueId] =
    pickleTaggedI(ManualIssueId)

  implicit lazy val picklerMultimapReqCodeValueSetApReqCodeId: Pickler[Multimap[ReqCode.Value, Set, ApReqCodeId]] =
    pickleMultimap[ReqCode.Value, Set, ApReqCodeId]

  implicit lazy val picklerExclusivity: Pickler[Exclusivity] =
    pickleBool(Exclusive)

  implicit lazy val picklerOn: Pickler[On] =
    pickleBool(On)

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

  implicit lazy val picklerReqCodeValue: Pickler[ReqCode.Value] =
    pickleNEV

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

  implicit lazy val picklerReqTypeMnemonic: Pickler[ReqType.Mnemonic] =
    pickleTaggedS(ReqType.Mnemonic)

  implicit lazy val picklerReqTypes: Pickler[ReqTypes] =
    transformPickler(ReqTypes.apply)(_.custom)

  implicit lazy val picklerReqTypesCustom: Pickler[ReqTypes.Custom] =
    pickleIMapD

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

  implicit lazy val picklerStaticFieldUseCaseStepTree: Pickler[StaticField.UseCaseStepTree] =
    new Pickler[StaticField.UseCaseStepTree] {
      private[this] final val KeyNormalAltStepTree = 'n'
      private[this] final val KeyExceptionStepTree = 'e'
      override def pickle(a: StaticField.UseCaseStepTree)(implicit state: PickleState): Unit =
        a match {
          case StaticField.NormalAltStepTree => state.enc.writeByte(KeyNormalAltStepTree)
          case StaticField.ExceptionStepTree => state.enc.writeByte(KeyExceptionStepTree)
        }
      override def unpickle(implicit state: UnpickleState): StaticField.UseCaseStepTree =
        state.dec.readByte match {
          case KeyNormalAltStepTree => StaticField.NormalAltStepTree
          case KeyExceptionStepTree => StaticField.ExceptionStepTree
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

  implicit lazy val picklerUseCaseId: Pickler[UseCaseId] =
    pickleTaggedI(UseCaseId).reuseByUnivEq

  implicit lazy val picklerUseCaseStepId: Pickler[UseCaseStepId] =
    pickleTaggedI(UseCaseStepId).reuseByUnivEq

  // Replaced by v1.1
//  implicit lazy val pickleValidFilter: Pickler[Filter.Valid] = {
//    import shipreq.webapp.base.filter.{IntensionalReqSet, FilterAst}
//    import Filter._
//    import Filter.Implicits._
//
//    implicit val picklerNonEmptyVectorUnit: Pickler[NonEmptyVector[Unit]] =
//      implicitly[Pickler[Int]].xmap(NonEmptyVector force Vector.fill(_)(()))(_.length)
//
//    implicit val picklerNonEmptySetInt: Pickler[NonEmptySet[Int]] =
//      pickleNES
//
//    implicit def picklerIRSetS[RT: Pickler]: Pickler[IntensionalReqSet.SomeOfType[RT]] =
//      new Pickler[IntensionalReqSet.SomeOfType[RT]] {
//        override def pickle(a: IntensionalReqSet.SomeOfType[RT])(implicit state: PickleState): Unit = {
//          state.pickle(a.reqType)
//          state.pickle(a.numbers)
//        }
//        override def unpickle(implicit state: UnpickleState): IntensionalReqSet.SomeOfType[RT] = {
//          val reqType = state.unpickle[RT]
//          val numbers = state.unpickle[NonEmptySet[Int]]
//          IntensionalReqSet.SomeOfType(reqType, numbers)
//        }
//      }
//
//    implicit def picklerIRSetW[RT: Pickler]: Pickler[IntensionalReqSet.WholeType[RT]] =
//      transformPickler(IntensionalReqSet.WholeType.apply[RT])(_.reqType)
//
//    def picklerIRSet[RT: Pickler]: Pickler[IntensionalReqSet[RT]] =
//      new Pickler[IntensionalReqSet[RT]] {
//        import IntensionalReqSet._
//        private[this] final val KeySomeOfType = 0
//        private[this] final val KeyWholeType  = 1
//        override def pickle(a: IntensionalReqSet[RT])(implicit state: PickleState): Unit =
//          a match {
//            case b: SomeOfType[RT] => state.enc.writeByte(KeySomeOfType); state.pickle(b)
//            case b: WholeType[RT]  => state.enc.writeByte(KeyWholeType ); state.pickle(b)
//          }
//        override def unpickle(implicit state: UnpickleState): IntensionalReqSet[RT] =
//          state.dec.readByte match {
//            case KeySomeOfType => state.unpickle[SomeOfType[RT]]
//            case KeyWholeType  => state.unpickle[WholeType[RT]]
//          }
//      }
//
//    implicit val picklerValidHashTag: Pickler[Valid.HashTag] =
//      pickleDisj
//
//    implicit val picklerValidIssueCatNEV: Pickler[NonEmptyVector[Valid.IssueCat]] =
//      pickleNEV
//
//    implicit val picklerValidReqSubset: Pickler[Valid.ReqSubset] =
//      picklerIRSet
//
//    implicit val picklerValidReqSet: Pickler[Valid.ReqSet] =
//      pickleNEV
//
//    implicit val picklerFilterAstAttr: Pickler[FilterAst.Attr] =
//      new Pickler[FilterAst.Attr] {
//        private[this] final val KeyAnyIssue = 'i'
//        private[this] final val KeyAnyTag   = 't'
//        override def pickle(a: FilterAst.Attr)(implicit state: PickleState): Unit =
//          a match {
//            case FilterAst.Attr.AnyIssue => state.enc.writeByte(KeyAnyIssue)
//            case FilterAst.Attr.AnyTag   => state.enc.writeByte(KeyAnyTag  )
//          }
//        override def unpickle(implicit state: UnpickleState): FilterAst.Attr =
//          state.dec.readByte match {
//            case KeyAnyIssue => FilterAst.Attr.AnyIssue
//            case KeyAnyTag   => FilterAst.Attr.AnyTag
//          }
//      }
//
//    implicit val picklerFilterAstText: Pickler[FilterAst.Text] =
//      new Pickler[FilterAst.Text] {
//        override def pickle(a: FilterAst.Text)(implicit state: PickleState): Unit = {
//          state.pickle(a.text)
//          state.pickle(a.quoteChar)
//        }
//        override def unpickle(implicit state: UnpickleState): FilterAst.Text = {
//          val text      = state.unpickle[String]
//          val quoteChar = state.unpickle[Option[Char]]
//          FilterAst.Text(text, quoteChar)
//        }
//      }
//
//    implicit val picklerFilterAstRegex: Pickler[FilterAst.Regex] =
//      transformPickler(FilterAst.Regex.apply)(_.text)
//
//    implicit val picklerFilterAstPresence: Pickler[FilterAst.Presence[Valid.Attr]] =
//      transformPickler(FilterAst.Presence.apply[Valid.Attr])(_.attr)
//
//    implicit val picklerFilterAstHasIssue: Pickler[FilterAst.HasIssue[Valid.IssueCat]] =
//      new Pickler[FilterAst.HasIssue[Valid.IssueCat]] {
//        override def pickle(a: FilterAst.HasIssue[Valid.IssueCat])(implicit state: PickleState): Unit = {
//          state.pickle(a.on)
//          state.pickle(a.criteria)
//        }
//        override def unpickle(implicit state: UnpickleState): FilterAst.HasIssue[Valid.IssueCat] = {
//          val on       = state.unpickle[On]
//          val criteria = state.unpickle[NonEmptyVector[Valid.IssueCat]]
//          FilterAst.HasIssue(on, criteria)
//        }
//      }
//
//    implicit val picklerFilterAstHashRef: Pickler[FilterAst.HashRef[Valid.HashTag]] =
//      transformPickler(FilterAst.HashRef.apply[Valid.HashTag])(_.value)
//
//    implicit val picklerFilterAstImpliesAnyOf: Pickler[FilterAst.ImpliesAnyOf[Valid.ReqSet]] =
//      transformPickler(FilterAst.ImpliesAnyOf.apply[Valid.ReqSet])(_.reqs)
//
//    implicit val picklerFilterAstImpliedByAnyOf: Pickler[FilterAst.ImpliedByAnyOf[Valid.ReqSet]] =
//      transformPickler(FilterAst.ImpliedByAnyOf.apply[Valid.ReqSet])(_.reqs)
//
//    implicit val picklerFilterAstReqs: Pickler[FilterAst.Reqs[Valid.ReqSet]] =
//      transformPickler(FilterAst.Reqs.apply[Valid.ReqSet])(_.reqs)
//
//    implicit val picklerFilterAstReqType: Pickler[FilterAst.ReqType[Valid.ReqType]] =
//      transformPickler(FilterAst.ReqType.apply[Valid.ReqType])(_.reqType)
//
//    implicit val picklerFilterAstNot: Pickler[FilterAst.Not[Unit]] =
//      transformPickler(FilterAst.Not.apply[Unit])(_.clause)
//
//    implicit val picklerFilterAstAllOf: Pickler[FilterAst.AllOf[Unit]] =
//      transformPickler(FilterAst.AllOf.apply[Unit])(_.clauses)
//
//    implicit val picklerFilterAstAnyOf: Pickler[FilterAst.AnyOf[Unit]] =
//      new Pickler[FilterAst.AnyOf[Unit]] {
//        override def pickle(a: FilterAst.AnyOf[Unit])(implicit state: PickleState): Unit = {
//          state.pickle(a.tail)
//        }
//        override def unpickle(implicit state: UnpickleState): FilterAst.AnyOf[Unit] = {
//          val tail = state.unpickle[NonEmptyVector[Unit]]
//          FilterAst.AnyOf((), tail)
//        }
//      }
//
//    implicit val picklerValidF: Pickler[ValidF[Unit]] =
//      new Pickler[ValidF[Unit]] {
//        private[this] final val KeyAllOf          = 0
//        private[this] final val KeyAnyOf          = 1
//        private[this] final val KeyHasIssue       = 2
//        private[this] final val KeyHashRef        = 3
//        private[this] final val KeyImpliedByAnyOf = 4
//        private[this] final val KeyImpliesAnyOf   = 5
//        private[this] final val KeyNot            = 6
//        private[this] final val KeyPresence       = 7
//        private[this] final val KeyRegex          = 8
//        private[this] final val KeyReqType        = 9
//        private[this] final val KeyReqs           = 10
//        private[this] final val KeyText           = 11
//        override def pickle(a: ValidF[Unit])(implicit state: PickleState): Unit =
//          a match {
//            case b: FilterAst.AllOf         [Unit]           => state.enc.writeByte(KeyAllOf         ); state.pickle(b)
//            case b: FilterAst.AnyOf         [Unit]           => state.enc.writeByte(KeyAnyOf         ); state.pickle(b)
//            case b: FilterAst.HasIssue      [Valid.IssueCat] => state.enc.writeByte(KeyHasIssue      ); state.pickle(b)
//            case b: FilterAst.HashRef       [Valid.HashTag]  => state.enc.writeByte(KeyHashRef       ); state.pickle(b)
//            case b: FilterAst.ImpliedByAnyOf[Valid.ReqSet]   => state.enc.writeByte(KeyImpliedByAnyOf); state.pickle(b)
//            case b: FilterAst.ImpliesAnyOf  [Valid.ReqSet]   => state.enc.writeByte(KeyImpliesAnyOf  ); state.pickle(b)
//            case b: FilterAst.Not           [Unit]           => state.enc.writeByte(KeyNot           ); state.pickle(b)
//            case b: FilterAst.Presence      [Valid.Attr]     => state.enc.writeByte(KeyPresence      ); state.pickle(b)
//            case b: FilterAst.Regex                          => state.enc.writeByte(KeyRegex         ); state.pickle(b)
//            case b: FilterAst.ReqType       [Valid.ReqType]  => state.enc.writeByte(KeyReqType       ); state.pickle(b)
//            case b: FilterAst.Reqs          [Valid.ReqSet]   => state.enc.writeByte(KeyReqs          ); state.pickle(b)
//            case b: FilterAst.Text                           => state.enc.writeByte(KeyText          ); state.pickle(b)
//          }
//        override def unpickle(implicit state: UnpickleState): ValidF[Unit] =
//          state.dec.readByte match {
//            case KeyAllOf          => state.unpickle[FilterAst.AllOf         [Unit          ]]
//            case KeyAnyOf          => state.unpickle[FilterAst.AnyOf         [Unit          ]]
//            case KeyHasIssue       => state.unpickle[FilterAst.HasIssue      [Valid.IssueCat]]
//            case KeyHashRef        => state.unpickle[FilterAst.HashRef       [Valid.HashTag ]]
//            case KeyImpliedByAnyOf => state.unpickle[FilterAst.ImpliedByAnyOf[Valid.ReqSet  ]]
//            case KeyImpliesAnyOf   => state.unpickle[FilterAst.ImpliesAnyOf  [Valid.ReqSet  ]]
//            case KeyNot            => state.unpickle[FilterAst.Not           [Unit          ]]
//            case KeyPresence       => state.unpickle[FilterAst.Presence      [Valid.Attr    ]]
//            case KeyRegex          => state.unpickle[FilterAst.Regex                         ]
//            case KeyReqType        => state.unpickle[FilterAst.ReqType       [Valid.ReqType ]]
//            case KeyReqs           => state.unpickle[FilterAst.Reqs          [Valid.ReqSet  ]]
//            case KeyText           => state.unpickle[FilterAst.Text                          ]
//          }
//      }
//
//    pickleFix[ValidF]
//  }

}
