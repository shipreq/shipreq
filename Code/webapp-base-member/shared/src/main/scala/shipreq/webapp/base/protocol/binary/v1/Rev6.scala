package shipreq.webapp.base.protocol.binary.v1

import boopickle.{ConstPickler, DefaultBasic}
import japgolly.microlibs.adt_macros.AdtMacros
import java.time.Instant
import scala.collection.immutable.TreeSet
import scala.reflect.ClassTag
import shipreq.base.util.{ErrorMsg, NonEmptyArraySeq}
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.text.{AtomTC, Text}

/** v1.6 */
object Rev6 {
  import boopickle.DefaultBasic._
  import BaseData._
  import BaseMemberData1._
  import BaseMemberData2._
  import Rev1._
  import Rev5._
  import Events._
  import PostEvents._


  object AtomPicklers extends AtomTC[Pickler] {
    import shipreq.webapp.base.text._
    import Atom._

    override def lazily[A](f: => Pickler[A]): Pickler[A] = pickleLazily(f)

    override def xmap[A, B](fa: DefaultBasic.Pickler[A])(f: A => B)(g: B => A): Pickler[B] = fa.xmap(f)(g)

    override def arr[A](implicit a: Pickler[A], ct: ClassTag[A]) = pickleArraySeq[A]

    override def nea[A](as: Pickler[ArraySeq[A]])(implicit a: Pickler[A]) = pickleNEA(as)

    override def str: Pickler[String] = implicitly

    override def sum[T <: Atom.Base](t: T)(get: Atom.Type => Pickler[t.Atom], all: List[Pickler[t.Atom]]): Pickler[t.Atom] =
      new Pickler[t.Atom] {
        private[this] final val KeyBlankLine      = '0'
        private[this] final val KeyBold           = 'B'
        private[this] final val KeyCodeBlock      = '{'
        private[this] final val KeyCodeRef        = 'c'
        private[this] final val KeyEmailAddress   = '@'
        private[this] final val KeyHeading1       = '1'
        private[this] final val KeyHeading2       = '2'
        private[this] final val KeyHeading3       = '3'
        private[this] final val KeyHeading4       = '4'
        private[this] final val KeyHeading5       = '5'
        private[this] final val KeyHeading6       = '6'
        private[this] final val KeyIssue          = 'i'
        private[this] final val KeyItalic         = 'I'
        private[this] final val KeyLiteral        = 'l'
        private[this] final val KeyMonospace      = '`'
        private[this] final val KeyOrderedList    = 'o'
        private[this] final val KeyReqRef         = 'r'
        private[this] final val KeyStrikethrough  = '~'
        private[this] final val KeyTagRef         = 't'
        private[this] final val KeyTeX            = 'X'
        private[this] final val KeyUnderline      = '_'
        private[this] final val KeyUnorderedList  = '*'
        private[this] final val KeyUseCaseStepRef = 'u'
        private[this] final val KeyWebAddress     = '/'

        override def pickle(a: t.Atom)(implicit state: PickleState): Unit = {
          Atom.Type.of(a) match {
            case t@ Type.Literal        => state.enc.writeByte(KeyLiteral       ); get(t).pickle(a)
            case t@ Type.BlankLine      => state.enc.writeByte(KeyBlankLine     ); get(t).pickle(a)
            case t@ Type.Bold           => state.enc.writeByte(KeyBold          ); get(t).pickle(a)
            case t@ Type.CodeBlock      => state.enc.writeByte(KeyCodeBlock     ); get(t).pickle(a)
            case t@ Type.CodeRef        => state.enc.writeByte(KeyCodeRef       ); get(t).pickle(a)
            case t@ Type.EmailAddress   => state.enc.writeByte(KeyEmailAddress  ); get(t).pickle(a)
            case t@ Type.Heading1       => state.enc.writeByte(KeyHeading1      ); get(t).pickle(a)
            case t@ Type.Heading2       => state.enc.writeByte(KeyHeading2      ); get(t).pickle(a)
            case t@ Type.Heading3       => state.enc.writeByte(KeyHeading3      ); get(t).pickle(a)
            case t@ Type.Heading4       => state.enc.writeByte(KeyHeading4      ); get(t).pickle(a)
            case t@ Type.Heading5       => state.enc.writeByte(KeyHeading5      ); get(t).pickle(a)
            case t@ Type.Heading6       => state.enc.writeByte(KeyHeading6      ); get(t).pickle(a)
            case t@ Type.Issue          => state.enc.writeByte(KeyIssue         ); get(t).pickle(a)
            case t@ Type.Italic         => state.enc.writeByte(KeyItalic        ); get(t).pickle(a)
            case t@ Type.Monospace      => state.enc.writeByte(KeyMonospace     ); get(t).pickle(a)
            case t@ Type.OrderedList    => state.enc.writeByte(KeyOrderedList   ); get(t).pickle(a)
            case t@ Type.ReqRef         => state.enc.writeByte(KeyReqRef        ); get(t).pickle(a)
            case t@ Type.Strikethrough  => state.enc.writeByte(KeyStrikethrough ); get(t).pickle(a)
            case t@ Type.TagRef         => state.enc.writeByte(KeyTagRef        ); get(t).pickle(a)
            case t@ Type.TeX            => state.enc.writeByte(KeyTeX           ); get(t).pickle(a)
            case t@ Type.Underline      => state.enc.writeByte(KeyUnderline     ); get(t).pickle(a)
            case t@ Type.UnorderedList  => state.enc.writeByte(KeyUnorderedList ); get(t).pickle(a)
            case t@ Type.UseCaseStepRef => state.enc.writeByte(KeyUseCaseStepRef); get(t).pickle(a)
            case t@ Type.WebAddress     => state.enc.writeByte(KeyWebAddress    ); get(t).pickle(a)
          }
        }

        override def unpickle(implicit state: UnpickleState): t.Atom = {
          state.dec.readByte match {
            case KeyLiteral        => get(Type.Literal       ).unpickle
            case KeyBlankLine      => get(Type.BlankLine     ).unpickle
            case KeyBold           => get(Type.Bold          ).unpickle
            case KeyCodeBlock      => get(Type.CodeBlock     ).unpickle
            case KeyCodeRef        => get(Type.CodeRef       ).unpickle
            case KeyEmailAddress   => get(Type.EmailAddress  ).unpickle
            case KeyHeading1       => get(Type.Heading1      ).unpickle
            case KeyHeading2       => get(Type.Heading2      ).unpickle
            case KeyHeading3       => get(Type.Heading3      ).unpickle
            case KeyHeading4       => get(Type.Heading4      ).unpickle
            case KeyHeading5       => get(Type.Heading5      ).unpickle
            case KeyHeading6       => get(Type.Heading6      ).unpickle
            case KeyIssue          => get(Type.Issue         ).unpickle
            case KeyItalic         => get(Type.Italic        ).unpickle
            case KeyMonospace      => get(Type.Monospace     ).unpickle
            case KeyOrderedList    => get(Type.OrderedList   ).unpickle
            case KeyReqRef         => get(Type.ReqRef        ).unpickle
            case KeyStrikethrough  => get(Type.Strikethrough ).unpickle
            case KeyTagRef         => get(Type.TagRef        ).unpickle
            case KeyTeX            => get(Type.TeX           ).unpickle
            case KeyUnderline      => get(Type.Underline     ).unpickle
            case KeyUnorderedList  => get(Type.UnorderedList ).unpickle
            case KeyUseCaseStepRef => get(Type.UseCaseStepRef).unpickle
            case KeyWebAddress     => get(Type.WebAddress    ).unpickle
          }
        }
      }

    override def blankLine[T <: NewLine](t: T): Pickler[t.BlankLine] =
      ConstPickler(t.blankLine)

    implicit val picklerCodeBlockDetail: Pickler[CodeBlockDetail] =
      new Pickler[CodeBlockDetail] {
        override def pickle(a: CodeBlockDetail)(implicit state: PickleState): Unit = {
          state.pickle(a.language)
          state.pickle(a.attributes)
        }
        override def unpickle(implicit state: UnpickleState): CodeBlockDetail = {
          val language   = state.unpickle[String]
          val attributes = state.unpickle[TreeSet[String]]
          CodeBlockDetail(language, attributes)
        }
      }

    override def codeBlock[T <: CodeBlock](t: T): Pickler[t.CodeBlock] =
      new Pickler[t.CodeBlock] {
        override def pickle(a: t.CodeBlock)(implicit state: PickleState): Unit = {
          writeVersion(1)
          state.pickle(a.code)
          state.pickle(a.detail)
        }
        override def unpickle(implicit state: UnpickleState): t.CodeBlock =
          readByVersion(1) {

            // v1.0
            case 0 =>
              val lang = state.unpickle[Option[String]]
              val code = state.unpickle[String]
              t.CodeBlock(lang.map(CodeBlockDetail(_, TreeSet.empty)), code)

            // v1.1
            case 1 =>
              val code   = state.unpickle[String]
              val detail = state.unpickle[Option[CodeBlockDetail]]
              t.CodeBlock(detail, code)
          }
      }

    private implicit val picklerDisplayReqRef: Pickler[DisplayReqRef] =
      pickleEnum(
        AdtMacros.adtValuesManually[DisplayReqRef](
          DisplayReqRef.AsId,
          DisplayReqRef.AsIdAndTitle,
        )
      )

    private def abstractReqRef[A, I](make: (I, DisplayReqRef) => A)
                                    (getI: A => I,
                                     getD: A => DisplayReqRef)
                                    (implicit I: Pickler[I]): Pickler[A] =
      new Pickler[A] {

        override def pickle(a: A)(implicit state: PickleState): Unit =
          getD(a) match {
            case DisplayReqRef.AsId =>
              // v1.0
              state.pickle(getI(a))

            case d =>
              // v1.1
              writeVersion(1)
              state.pickle(getI(a))
              state.pickle(d)
          }

        override def unpickle(implicit state: UnpickleState): A =
          readByVersion(1) {

            // v1.0
            case 0 =>
              val i = state.unpickle[I]
              make(i, DisplayReqRef.AsId)

            // v1.1
            case 1 =>
              val i = state.unpickle[I]
              val d = state.unpickle[DisplayReqRef]
              make(i, d)
          }
      }

    override def reqRef[T <: ContentRef](t: T): Pickler[t.ReqRef] =
      abstractReqRef[t.ReqRef, ReqId](t.ReqRef(_, _))(_.id, _.display)

    override def codeRef[T <: ContentRef](t: T): Pickler[t.CodeRef] =
      abstractReqRef[t.CodeRef, ReqCodeId](t.CodeRef(_, _))(_.id, _.display)

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

    override def orderedList[T <: ListMarkup](t: T)(implicit h: Pickler[NonEmptyArraySeq[t.ListItem]]): Pickler[t.OrderedList] =
      transformPickler((i: NonEmptyArraySeq[t.ListItem]) => t.OrderedList(i))(_.items)

    override def unorderedList[T <: ListMarkup](t: T)(implicit h: Pickler[NonEmptyArraySeq[t.ListItem]]): Pickler[t.UnorderedList] =
      transformPickler((i: NonEmptyArraySeq[t.ListItem]) => t.UnorderedList(i))(_.items)
  }

  import AtomPicklers.instances._

  // ===================================================================================================================

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

  implicit lazy val picklerGenericReqsById: Pickler[GenericReqIMap] =
    pickleIMapD

  implicit lazy val picklerGenericReqs: Pickler[GenericReqs] =
    picklerGenericReqsById.xmap(GenericReqs.apply)(_.imap)

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

  implicit lazy val picklerReqCodes: Pickler[ReqCodes] =
    transformPickler(ReqCodes.apply)(_.trie)

  implicit lazy val picklerReqCodeTrie: Pickler[ReqCode.Trie] =
    pickleTrie

  implicit lazy val picklerReqDataText: Pickler[ReqData.Text] =
    pickleMap[CustomField.Text.Id, Map[ReqId, Text.CustomTextField.NonEmptyText]]
      .xmap(ReqData.Text.apply)(_.data)

  implicit lazy val picklerRequirements: Pickler[Requirements] =
    new Pickler[Requirements] {
      override def pickle(a: Requirements)(implicit state: PickleState): Unit = {
        state.pickle(a.genericReqs)
        state.pickle(a.useCases)
        state.pickle(a.pubids)
      }
      override def unpickle(implicit state: UnpickleState): Requirements = {
        val genericReqs = state.unpickle[GenericReqs]
        val useCases    = state.unpickle[UseCases]
        val pubids      = state.unpickle[PubidRegister]
        Requirements(genericReqs, useCases, pubids)
      }
    }

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

  implicit lazy val picklerUseCaseSteps: Pickler[UseCaseSteps] =
    transformPickler(UseCaseSteps.apply)(_.tree)(pickleVectorTree)

  // ===================================================================================================================

  implicit val picklerEventNonEmptyCustomTextMap: Pickler[Event.NonEmptyCustomTextMap] = pickleNonEmptyMono

  implicit val pickleCodeGroupGD: Pickler[CodeGroupGD.NonEmptyValues] = {
    import CodeGroupGD._

    implicit val picklerValueForCode  = transformPickler(ValueForCode .apply)(_.value)
    implicit val picklerValueForTitle = transformPickler(ValueForTitle.apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyCode  = 'C'
        private[this] final val KeyTitle = 'T'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForCode  => state.enc.writeByte(KeyCode ); state.pickle(b)
            case b: ValueForTitle => state.enc.writeByte(KeyTitle); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyCode  => state.unpickle[ValueForCode ]
            case KeyTitle => state.unpickle[ValueForTitle]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  implicit val pickleGenericReqGD: Pickler[GenericReqGD.Values] = {
    import GenericReqGD._

    implicit val picklerValueForCodes      = transformPickler(ValueForCodes     .apply)(_.value)
    implicit val picklerValueForCustomText = transformPickler(ValueForCustomText.apply)(_.value)
    implicit val picklerValueForImpSrcs    = transformPickler(ValueForImpSrcs   .apply)(_.value)
    implicit val picklerValueForImpTgts    = transformPickler(ValueForImpTgts   .apply)(_.value)
    implicit val picklerValueForTags       = transformPickler(ValueForTags      .apply)(_.value)
    implicit val picklerValueForTitle      = transformPickler(ValueForTitle     .apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyCodes      = 'C'
        private[this] final val KeyCustomText = 'X'
        private[this] final val KeyImpSrcs    = '>'
        private[this] final val KeyImpTgts    = '<'
        private[this] final val KeyTags       = '#'
        private[this] final val KeyTitle      = 'T'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForCodes      => state.enc.writeByte(KeyCodes     ); state.pickle(b)
            case b: ValueForCustomText => state.enc.writeByte(KeyCustomText); state.pickle(b)
            case b: ValueForImpSrcs    => state.enc.writeByte(KeyImpSrcs   ); state.pickle(b)
            case b: ValueForImpTgts    => state.enc.writeByte(KeyImpTgts   ); state.pickle(b)
            case b: ValueForTags       => state.enc.writeByte(KeyTags      ); state.pickle(b)
            case b: ValueForTitle      => state.enc.writeByte(KeyTitle     ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyCodes      => state.unpickle[ValueForCodes]
            case KeyCustomText => state.unpickle[ValueForCustomText]
            case KeyImpSrcs    => state.unpickle[ValueForImpSrcs]
            case KeyImpTgts    => state.unpickle[ValueForImpTgts]
            case KeyTags       => state.unpickle[ValueForTags]
            case KeyTitle      => state.unpickle[ValueForTitle]
          }
      }

    pickleIMap(emptyValues)
  }

  implicit val pickleUseCaseGD: Pickler[UseCaseGD.Values] = {
    import UseCaseGD._

    implicit val picklerValueForCodes      = transformPickler(ValueForCodes     .apply)(_.value)
    implicit val picklerValueForCustomText = transformPickler(ValueForCustomText.apply)(_.value)
    implicit val picklerValueForImpSrcs    = transformPickler(ValueForImpSrcs   .apply)(_.value)
    implicit val picklerValueForImpTgts    = transformPickler(ValueForImpTgts   .apply)(_.value)
    implicit val picklerValueForTags       = transformPickler(ValueForTags      .apply)(_.value)
    implicit val picklerValueForTitle      = transformPickler(ValueForTitle     .apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyCodes      = 'C'
        private[this] final val KeyCustomText = 'X'
        private[this] final val KeyImpSrcs    = '>'
        private[this] final val KeyImpTgts    = '<'
        private[this] final val KeyTags       = '#'
        private[this] final val KeyTitle      = 'T'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForCodes      => state.enc.writeByte(KeyCodes     ); state.pickle(b)
            case b: ValueForCustomText => state.enc.writeByte(KeyCustomText); state.pickle(b)
            case b: ValueForImpSrcs    => state.enc.writeByte(KeyImpSrcs   ); state.pickle(b)
            case b: ValueForImpTgts    => state.enc.writeByte(KeyImpTgts   ); state.pickle(b)
            case b: ValueForTags       => state.enc.writeByte(KeyTags      ); state.pickle(b)
            case b: ValueForTitle      => state.enc.writeByte(KeyTitle     ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyCodes      => state.unpickle[ValueForCodes]
            case KeyCustomText => state.unpickle[ValueForCustomText]
            case KeyImpSrcs    => state.unpickle[ValueForImpSrcs]
            case KeyImpTgts    => state.unpickle[ValueForImpTgts]
            case KeyTags       => state.unpickle[ValueForTags]
            case KeyTitle      => state.unpickle[ValueForTitle]
          }
      }

    pickleIMap(emptyValues)
  }

  implicit val pickleUseCaseStepGD: Pickler[UseCaseStepGD.NonEmptyValues] = {
    import UseCaseStepGD._

    implicit val picklerValueForFlowIn  = transformPickler(ValueForFlowIn .apply)(_.value)
    implicit val picklerValueForFlowOut = transformPickler(ValueForFlowOut.apply)(_.value)
    implicit val picklerValueForTitle   = transformPickler(ValueForTitle  .apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyFlowIn  = '<'
        private[this] final val KeyFlowOut = '>'
        private[this] final val KeyTitle   = 'T'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForFlowIn  => state.enc.writeByte(KeyFlowIn ); state.pickle(b)
            case b: ValueForFlowOut => state.enc.writeByte(KeyFlowOut); state.pickle(b)
            case b: ValueForTitle   => state.enc.writeByte(KeyTitle  ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyFlowIn  => state.unpickle[ValueForFlowIn]
            case KeyFlowOut => state.unpickle[ValueForFlowOut]
            case KeyTitle   => state.unpickle[ValueForTitle]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  private[v1] implicit val picklerEventGenericReqCreate: Pickler[Event.GenericReqCreate] =
    new Pickler[Event.GenericReqCreate] {
      override def pickle(a: Event.GenericReqCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.rt)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.GenericReqCreate = {
        val id = state.unpickle[GenericReqId]
        val rt = state.unpickle[CustomReqTypeId]
        val vs = state.unpickle[GenericReqGD.Values]
        Event.GenericReqCreate(id, rt, vs)
      }
    }

  private[v1] implicit val picklerEventGenericReqTitleSet: Pickler[Event.GenericReqTitleSet] =
    new Pickler[Event.GenericReqTitleSet] {
      override def pickle(a: Event.GenericReqTitleSet)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.value)
      }
      override def unpickle(implicit state: UnpickleState): Event.GenericReqTitleSet = {
        val id    = state.unpickle[GenericReqId]
        val value = state.unpickle[Text.GenericReqTitle.OptionalText]
        Event.GenericReqTitleSet(id, value)
      }
    }

  private[v1] implicit val picklerEventUseCaseCreate: Pickler[Event.UseCaseCreate] =
    new Pickler[Event.UseCaseCreate] {
      override def pickle(a: Event.UseCaseCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.stepId)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.UseCaseCreate = {
        val id     = state.unpickle[UseCaseId]
        val stepId = state.unpickle[UseCaseStepId]
        val vs     = state.unpickle[UseCaseGD.Values]
        Event.UseCaseCreate(id, stepId, vs)
      }
    }

  private[v1] implicit val picklerEventUseCaseTitleSet: Pickler[Event.UseCaseTitleSet] =
    new Pickler[Event.UseCaseTitleSet] {
      override def pickle(a: Event.UseCaseTitleSet)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.value)
      }
      override def unpickle(implicit state: UnpickleState): Event.UseCaseTitleSet = {
        val id    = state.unpickle[UseCaseId]
        val value = state.unpickle[Text.UseCaseTitle.OptionalText]
        Event.UseCaseTitleSet(id, value)
      }
    }

  private[v1] implicit val picklerEventUseCaseStepUpdate: Pickler[Event.UseCaseStepUpdate] =
    new Pickler[Event.UseCaseStepUpdate] {
      override def pickle(a: Event.UseCaseStepUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.UseCaseStepUpdate = {
        val id = state.unpickle[UseCaseStepId]
        val vs = state.unpickle[UseCaseStepGD.NonEmptyValues]
        Event.UseCaseStepUpdate(id, vs)
      }
    }

  private[v1] implicit val picklerEventCodeGroupCreate: Pickler[Event.CodeGroupCreate] =
    new Pickler[Event.CodeGroupCreate] {
      override def pickle(a: Event.CodeGroupCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.CodeGroupCreate = {
        val id = state.unpickle[ReqCodeGroupId]
        val vs = state.unpickle[CodeGroupGD.NonEmptyValues]
        Event.CodeGroupCreate(id, vs)
      }
    }

  private[v1] implicit val picklerEventCodeGroupUpdate: Pickler[Event.CodeGroupUpdate] =
    new Pickler[Event.CodeGroupUpdate] {
      override def pickle(a: Event.CodeGroupUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.CodeGroupUpdate = {
        val id = state.unpickle[ReqCodeGroupId]
        val vs = state.unpickle[CodeGroupGD.NonEmptyValues]
        Event.CodeGroupUpdate(id, vs)
      }
    }

  private[v1] implicit val picklerEventReqFieldCustomTextSet: Pickler[Event.ReqFieldCustomTextSet] =
    new Pickler[Event.ReqFieldCustomTextSet] {
      override def pickle(a: Event.ReqFieldCustomTextSet)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.fid)
        state.pickle(a.value)
      }
      override def unpickle(implicit state: UnpickleState): Event.ReqFieldCustomTextSet = {
        val id    = state.unpickle[ReqId]
        val fid   = state.unpickle[CustomField.Text.Id]
        val value = state.unpickle[Text.CustomTextField.OptionalText]
        Event.ReqFieldCustomTextSet(id, fid, value)
      }
    }

  private[v1] implicit val picklerEventReqsDelete: Pickler[Event.ReqsDelete] =
    new Pickler[Event.ReqsDelete] {
      override def pickle(a: Event.ReqsDelete)(implicit state: PickleState): Unit = {
        state.pickle(a.reqs)
        state.pickle(a.codeGroups)
        state.pickle(a.reason)
      }
      override def unpickle(implicit state: UnpickleState): Event.ReqsDelete = {
        val reqs       = state.unpickle[NonEmptySet[ReqId]]
        val codeGroups = state.unpickle[Set[ReqCodeGroupId]]
        val reason     = state.unpickle[Text.DeletionReason.OptionalText]
        Event.ReqsDelete(reqs, codeGroups, reason)
      }
    }

  private[v1] implicit val picklerEventManualIssueCreate: Pickler[Event.ManualIssueCreate] =
    new Pickler[Event.ManualIssueCreate] {
      override def pickle(a: Event.ManualIssueCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.text)
      }
      override def unpickle(implicit state: UnpickleState): Event.ManualIssueCreate = {
        val id   = state.unpickle[ManualIssueId]
        val text = state.unpickle[Text.ManualIssue.NonEmptyText]
        Event.ManualIssueCreate(id, text)
      }
    }

  private[v1] implicit val picklerEventManualIssueUpdate: Pickler[Event.ManualIssueUpdate] =
    new Pickler[Event.ManualIssueUpdate] {
      override def pickle(a: Event.ManualIssueUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.text)
      }
      override def unpickle(implicit state: UnpickleState): Event.ManualIssueUpdate = {
        val id   = state.unpickle[ManualIssueId]
        val text = state.unpickle[Text.ManualIssue.NonEmptyText]
        Event.ManualIssueUpdate(id, text)
      }
    }


  // ===================================================================================================================

  implicit lazy val picklerEvent: Pickler[Event] =
    new Pickler[Event] {
      import Event._
      private[this] final val KeyApplicableTagCreateV1   =  0
      private[this] final val KeyApplicableTagUpdateV1   =  1
      private[this] final val KeyCodeGroupCreate         =  2
      private[this] final val KeyCodeGroupUpdate         =  3
      private[this] final val KeyCodeGroupsDelete        =  4
      private[this] final val KeyContentRestore          =  5
      private[this] final val KeyCustomIssueTypeCreate   =  6
      private[this] final val KeyCustomIssueTypeDelete   =  7
      private[this] final val KeyCustomIssueTypeRestore  =  8
      private[this] final val KeyCustomIssueTypeUpdate   =  9
      private[this] final val KeyCustomReqTypeCreateV1   = 10
      private[this] final val KeyCustomReqTypeDelete     = 11
      private[this] final val KeyCustomReqTypeRestore    = 12
      private[this] final val KeyCustomReqTypeUpdateV1   = 13
      private[this] final val KeyFieldCustomDelete       = 14
      private[this] final val KeyFieldCustomImpCreateV1  = 15
      private[this] final val KeyFieldCustomImpUpdateV1  = 16
      private[this] final val KeyFieldCustomRestore      = 17
      private[this] final val KeyFieldCustomTagCreateV1  = 18
      private[this] final val KeyFieldCustomTagUpdateV1  = 19
      private[this] final val KeyFieldCustomTextCreateV1 = 20
      private[this] final val KeyFieldCustomTextUpdateV1 = 21
      private[this] final val KeyFieldReposition         = 22
      private[this] final val KeyFieldStaticAdd          = 23
      private[this] final val KeyFieldStaticRemove       = 24
      private[this] final val KeyGenericReqCreate        = 25
      private[this] final val KeyGenericReqTitleSet      = 26
      private[this] final val KeyGenericReqTypeSet       = 27
      private[this] final val KeyManualIssueCreate       = 28
      private[this] final val KeyManualIssueDelete       = 29
      private[this] final val KeyManualIssueUpdate       = 30
      private[this] final val KeyProjectNameSet          = 31
      private[this] final val KeyProjectTemplateApply    = 32
      private[this] final val KeyReqCodesPatch           = 33
      private[this] final val KeyReqFieldCustomTextSet   = 34
      private[this] final val KeyReqImplicationsPatch    = 35
      private[this] final val KeyReqTagsPatch            = 36
      private[this] final val KeyReqsDelete              = 37
      private[this] final val KeySavedViewCreateV1       = 38
      private[this] final val KeySavedViewDefaultSet     = 39
      private[this] final val KeySavedViewDelete         = 40
      private[this] final val KeySavedViewUpdateV1       = 41
      private[this] final val KeyTagDelete               = 42
      private[this] final val KeyTagGroupCreate          = 43
      private[this] final val KeyTagGroupUpdate          = 44
      private[this] final val KeyTagRestore              = 45
      private[this] final val KeyUseCaseCreate           = 46
      private[this] final val KeyUseCaseStepCreate       = 47
      private[this] final val KeyUseCaseStepDelete       = 48
      private[this] final val KeyUseCaseStepRestore      = 49
      private[this] final val KeyUseCaseStepShiftLeft    = 50
      private[this] final val KeyUseCaseStepShiftRight   = 51
      private[this] final val KeyUseCaseStepUpdate       = 52
      private[this] final val KeyUseCaseTitleSet         = 53
      private[this] final val KeyApplicableTagCreateV2   = 54
      private[this] final val KeyApplicableTagUpdateV2   = 55
      private[this] final val KeyCustomReqTypeDeleteHard = 56
      private[this] final val KeyCustomReqTypeDeleteSoft = 57
      private[this] final val KeyFieldCustomImpCreateV2  = 58
      private[this] final val KeyFieldCustomImpUpdateV2  = 59
      private[this] final val KeyFieldCustomTagCreateV2  = 60
      private[this] final val KeyFieldCustomTagUpdateV2  = 61
      private[this] final val KeyFieldCustomTextCreateV2 = 62
      private[this] final val KeyFieldCustomTextUpdateV2 = 63
      private[this] final val KeyCustomReqTypeCreateV2   = 64
      private[this] final val KeyCustomReqTypeUpdateV2   = 65
      private[this] final val KeySavedViewCreateV2       = 66
      private[this] final val KeySavedViewUpdateV2       = 67

      override def pickle(a: Event)(implicit state: PickleState): Unit =
        a match {
          case b: ApplicableTagCreate     => state.enc.writeByte(KeyApplicableTagCreateV2  ); state.pickle(b)
          case b: ApplicableTagCreateV1   => state.enc.writeByte(KeyApplicableTagCreateV1  ); state.pickle(b)
          case b: ApplicableTagUpdate     => state.enc.writeByte(KeyApplicableTagUpdateV2  ); state.pickle(b)
          case b: ApplicableTagUpdateV1   => state.enc.writeByte(KeyApplicableTagUpdateV1  ); state.pickle(b)
          case b: CodeGroupCreate         => state.enc.writeByte(KeyCodeGroupCreate        ); state.pickle(b)
          case b: CodeGroupsDelete        => state.enc.writeByte(KeyCodeGroupsDelete       ); state.pickle(b)
          case b: CodeGroupUpdate         => state.enc.writeByte(KeyCodeGroupUpdate        ); state.pickle(b)
          case b: ContentRestore          => state.enc.writeByte(KeyContentRestore         ); state.pickle(b)
          case b: CustomIssueTypeCreate   => state.enc.writeByte(KeyCustomIssueTypeCreate  ); state.pickle(b)
          case b: CustomIssueTypeDelete   => state.enc.writeByte(KeyCustomIssueTypeDelete  ); state.pickle(b)
          case b: CustomIssueTypeRestore  => state.enc.writeByte(KeyCustomIssueTypeRestore ); state.pickle(b)
          case b: CustomIssueTypeUpdate   => state.enc.writeByte(KeyCustomIssueTypeUpdate  ); state.pickle(b)
          case b: CustomReqTypeCreate     => state.enc.writeByte(KeyCustomReqTypeCreateV2  ); state.pickle(b)
          case b: CustomReqTypeCreateV1   => state.enc.writeByte(KeyCustomReqTypeCreateV1  ); state.pickle(b)
          case b: CustomReqTypeDelete     => state.enc.writeByte(KeyCustomReqTypeDelete    ); state.pickle(b)
          case b: CustomReqTypeDeleteHard => state.enc.writeByte(KeyCustomReqTypeDeleteHard); state.pickle(b)
          case b: CustomReqTypeDeleteSoft => state.enc.writeByte(KeyCustomReqTypeDeleteSoft); state.pickle(b)
          case b: CustomReqTypeRestore    => state.enc.writeByte(KeyCustomReqTypeRestore   ); state.pickle(b)
          case b: CustomReqTypeUpdate     => state.enc.writeByte(KeyCustomReqTypeUpdateV2  ); state.pickle(b)
          case b: CustomReqTypeUpdateV1   => state.enc.writeByte(KeyCustomReqTypeUpdateV1  ); state.pickle(b)
          case b: FieldCustomDelete       => state.enc.writeByte(KeyFieldCustomDelete      ); state.pickle(b)
          case b: FieldCustomImpCreate    => state.enc.writeByte(KeyFieldCustomImpCreateV2 ); state.pickle(b)
          case b: FieldCustomImpCreateV1  => state.enc.writeByte(KeyFieldCustomImpCreateV1 ); state.pickle(b)
          case b: FieldCustomImpUpdate    => state.enc.writeByte(KeyFieldCustomImpUpdateV2 ); state.pickle(b)
          case b: FieldCustomImpUpdateV1  => state.enc.writeByte(KeyFieldCustomImpUpdateV1 ); state.pickle(b)
          case b: FieldCustomRestore      => state.enc.writeByte(KeyFieldCustomRestore     ); state.pickle(b)
          case b: FieldCustomTagCreate    => state.enc.writeByte(KeyFieldCustomTagCreateV2 ); state.pickle(b)
          case b: FieldCustomTagCreateV1  => state.enc.writeByte(KeyFieldCustomTagCreateV1 ); state.pickle(b)
          case b: FieldCustomTagUpdate    => state.enc.writeByte(KeyFieldCustomTagUpdateV2 ); state.pickle(b)
          case b: FieldCustomTagUpdateV1  => state.enc.writeByte(KeyFieldCustomTagUpdateV1 ); state.pickle(b)
          case b: FieldCustomTextCreate   => state.enc.writeByte(KeyFieldCustomTextCreateV2); state.pickle(b)
          case b: FieldCustomTextCreateV1 => state.enc.writeByte(KeyFieldCustomTextCreateV1); state.pickle(b)
          case b: FieldCustomTextUpdate   => state.enc.writeByte(KeyFieldCustomTextUpdateV2); state.pickle(b)
          case b: FieldCustomTextUpdateV1 => state.enc.writeByte(KeyFieldCustomTextUpdateV1); state.pickle(b)
          case b: FieldReposition         => state.enc.writeByte(KeyFieldReposition        ); state.pickle(b)
          case b: FieldStaticAdd          => state.enc.writeByte(KeyFieldStaticAdd         ); state.pickle(b)
          case b: FieldStaticRemove       => state.enc.writeByte(KeyFieldStaticRemove      ); state.pickle(b)
          case b: GenericReqCreate        => state.enc.writeByte(KeyGenericReqCreate       ); state.pickle(b)
          case b: GenericReqTitleSet      => state.enc.writeByte(KeyGenericReqTitleSet     ); state.pickle(b)
          case b: GenericReqTypeSet       => state.enc.writeByte(KeyGenericReqTypeSet      ); state.pickle(b)
          case b: ManualIssueCreate       => state.enc.writeByte(KeyManualIssueCreate      ); state.pickle(b)
          case b: ManualIssueDelete       => state.enc.writeByte(KeyManualIssueDelete      ); state.pickle(b)
          case b: ManualIssueUpdate       => state.enc.writeByte(KeyManualIssueUpdate      ); state.pickle(b)
          case b: ProjectNameSet          => state.enc.writeByte(KeyProjectNameSet         ); state.pickle(b)
          case b: ProjectTemplateApply    => state.enc.writeByte(KeyProjectTemplateApply   ); state.pickle(b)
          case b: ReqCodesPatch           => state.enc.writeByte(KeyReqCodesPatch          ); state.pickle(b)
          case b: ReqFieldCustomTextSet   => state.enc.writeByte(KeyReqFieldCustomTextSet  ); state.pickle(b)
          case b: ReqImplicationsPatch    => state.enc.writeByte(KeyReqImplicationsPatch   ); state.pickle(b)
          case b: ReqsDelete              => state.enc.writeByte(KeyReqsDelete             ); state.pickle(b)
          case b: ReqTagsPatch            => state.enc.writeByte(KeyReqTagsPatch           ); state.pickle(b)
          case b: SavedViewCreate         => state.enc.writeByte(KeySavedViewCreateV2      ); state.pickle(b)
          case b: SavedViewCreateV1       => state.enc.writeByte(KeySavedViewCreateV1      ); state.pickle(b)
          case b: SavedViewDefaultSet     => state.enc.writeByte(KeySavedViewDefaultSet    ); state.pickle(b)
          case b: SavedViewDelete         => state.enc.writeByte(KeySavedViewDelete        ); state.pickle(b)
          case b: SavedViewUpdate         => state.enc.writeByte(KeySavedViewUpdateV2      ); state.pickle(b)
          case b: SavedViewUpdateV1       => state.enc.writeByte(KeySavedViewUpdateV1      ); state.pickle(b)
          case b: TagDelete               => state.enc.writeByte(KeyTagDelete              ); state.pickle(b)
          case b: TagGroupCreate          => state.enc.writeByte(KeyTagGroupCreate         ); state.pickle(b)
          case b: TagGroupUpdate          => state.enc.writeByte(KeyTagGroupUpdate         ); state.pickle(b)
          case b: TagRestore              => state.enc.writeByte(KeyTagRestore             ); state.pickle(b)
          case b: UseCaseCreate           => state.enc.writeByte(KeyUseCaseCreate          ); state.pickle(b)
          case b: UseCaseStepCreate       => state.enc.writeByte(KeyUseCaseStepCreate      ); state.pickle(b)
          case b: UseCaseStepDelete       => state.enc.writeByte(KeyUseCaseStepDelete      ); state.pickle(b)
          case b: UseCaseStepRestore      => state.enc.writeByte(KeyUseCaseStepRestore     ); state.pickle(b)
          case b: UseCaseStepShiftLeft    => state.enc.writeByte(KeyUseCaseStepShiftLeft   ); state.pickle(b)
          case b: UseCaseStepShiftRight   => state.enc.writeByte(KeyUseCaseStepShiftRight  ); state.pickle(b)
          case b: UseCaseStepUpdate       => state.enc.writeByte(KeyUseCaseStepUpdate      ); state.pickle(b)
          case b: UseCaseTitleSet         => state.enc.writeByte(KeyUseCaseTitleSet        ); state.pickle(b)
        }

      override def unpickle(implicit state: UnpickleState): Event =
        state.dec.readByte match {
          case KeyApplicableTagCreateV1   => state.unpickle[ApplicableTagCreateV1]
          case KeyApplicableTagCreateV2   => state.unpickle[ApplicableTagCreate]
          case KeyApplicableTagUpdateV1   => state.unpickle[ApplicableTagUpdateV1]
          case KeyApplicableTagUpdateV2   => state.unpickle[ApplicableTagUpdate]
          case KeyCodeGroupCreate         => state.unpickle[CodeGroupCreate]
          case KeyCodeGroupsDelete        => state.unpickle[CodeGroupsDelete]
          case KeyCodeGroupUpdate         => state.unpickle[CodeGroupUpdate]
          case KeyContentRestore          => state.unpickle[ContentRestore]
          case KeyCustomIssueTypeCreate   => state.unpickle[CustomIssueTypeCreate]
          case KeyCustomIssueTypeDelete   => state.unpickle[CustomIssueTypeDelete]
          case KeyCustomIssueTypeRestore  => state.unpickle[CustomIssueTypeRestore]
          case KeyCustomIssueTypeUpdate   => state.unpickle[CustomIssueTypeUpdate]
          case KeyCustomReqTypeCreateV1   => state.unpickle[CustomReqTypeCreateV1]
          case KeyCustomReqTypeCreateV2   => state.unpickle[CustomReqTypeCreate]
          case KeyCustomReqTypeDelete     => state.unpickle[CustomReqTypeDelete]
          case KeyCustomReqTypeDeleteHard => state.unpickle[CustomReqTypeDeleteHard]
          case KeyCustomReqTypeDeleteSoft => state.unpickle[CustomReqTypeDeleteSoft]
          case KeyCustomReqTypeRestore    => state.unpickle[CustomReqTypeRestore]
          case KeyCustomReqTypeUpdateV1   => state.unpickle[CustomReqTypeUpdateV1]
          case KeyCustomReqTypeUpdateV2   => state.unpickle[CustomReqTypeUpdate]
          case KeyFieldCustomDelete       => state.unpickle[FieldCustomDelete]
          case KeyFieldCustomImpCreateV1  => state.unpickle[FieldCustomImpCreateV1]
          case KeyFieldCustomImpCreateV2  => state.unpickle[FieldCustomImpCreate]
          case KeyFieldCustomImpUpdateV1  => state.unpickle[FieldCustomImpUpdateV1]
          case KeyFieldCustomImpUpdateV2  => state.unpickle[FieldCustomImpUpdate]
          case KeyFieldCustomRestore      => state.unpickle[FieldCustomRestore]
          case KeyFieldCustomTagCreateV1  => state.unpickle[FieldCustomTagCreateV1]
          case KeyFieldCustomTagCreateV2  => state.unpickle[FieldCustomTagCreate]
          case KeyFieldCustomTagUpdateV1  => state.unpickle[FieldCustomTagUpdateV1]
          case KeyFieldCustomTagUpdateV2  => state.unpickle[FieldCustomTagUpdate]
          case KeyFieldCustomTextCreateV1 => state.unpickle[FieldCustomTextCreateV1]
          case KeyFieldCustomTextCreateV2 => state.unpickle[FieldCustomTextCreate]
          case KeyFieldCustomTextUpdateV1 => state.unpickle[FieldCustomTextUpdateV1]
          case KeyFieldCustomTextUpdateV2 => state.unpickle[FieldCustomTextUpdate]
          case KeyFieldReposition         => state.unpickle[FieldReposition]
          case KeyFieldStaticAdd          => state.unpickle[FieldStaticAdd]
          case KeyFieldStaticRemove       => state.unpickle[FieldStaticRemove]
          case KeyGenericReqCreate        => state.unpickle[GenericReqCreate]
          case KeyGenericReqTitleSet      => state.unpickle[GenericReqTitleSet]
          case KeyGenericReqTypeSet       => state.unpickle[GenericReqTypeSet]
          case KeyManualIssueCreate       => state.unpickle[ManualIssueCreate]
          case KeyManualIssueDelete       => state.unpickle[ManualIssueDelete]
          case KeyManualIssueUpdate       => state.unpickle[ManualIssueUpdate]
          case KeyProjectNameSet          => state.unpickle[ProjectNameSet]
          case KeyProjectTemplateApply    => state.unpickle[ProjectTemplateApply]
          case KeyReqCodesPatch           => state.unpickle[ReqCodesPatch]
          case KeyReqFieldCustomTextSet   => state.unpickle[ReqFieldCustomTextSet]
          case KeyReqImplicationsPatch    => state.unpickle[ReqImplicationsPatch]
          case KeyReqsDelete              => state.unpickle[ReqsDelete]
          case KeyReqTagsPatch            => state.unpickle[ReqTagsPatch]
          case KeySavedViewCreateV1       => state.unpickle[SavedViewCreateV1]
          case KeySavedViewCreateV2       => state.unpickle[SavedViewCreate]
          case KeySavedViewDefaultSet     => state.unpickle[SavedViewDefaultSet]
          case KeySavedViewDelete         => state.unpickle[SavedViewDelete]
          case KeySavedViewUpdateV1       => state.unpickle[SavedViewUpdateV1]
          case KeySavedViewUpdateV2       => state.unpickle[SavedViewUpdate]
          case KeyTagDelete               => state.unpickle[TagDelete]
          case KeyTagGroupCreate          => state.unpickle[TagGroupCreate]
          case KeyTagGroupUpdate          => state.unpickle[TagGroupUpdate]
          case KeyTagRestore              => state.unpickle[TagRestore]
          case KeyUseCaseCreate           => state.unpickle[UseCaseCreate]
          case KeyUseCaseStepCreate       => state.unpickle[UseCaseStepCreate]
          case KeyUseCaseStepDelete       => state.unpickle[UseCaseStepDelete]
          case KeyUseCaseStepRestore      => state.unpickle[UseCaseStepRestore]
          case KeyUseCaseStepShiftLeft    => state.unpickle[UseCaseStepShiftLeft]
          case KeyUseCaseStepShiftRight   => state.unpickle[UseCaseStepShiftRight]
          case KeyUseCaseStepUpdate       => state.unpickle[UseCaseStepUpdate]
          case KeyUseCaseTitleSet         => state.unpickle[UseCaseTitleSet]
        }
    }

  implicit lazy val picklerActiveEvent: Pickler[ActiveEvent] =
    picklerEvent.narrow

  implicit lazy val picklerVerifiedEvent: Pickler[VerifiedEvent] =
    new Pickler[VerifiedEvent] {
      override def pickle(a: VerifiedEvent)(implicit state: PickleState): Unit = {
        state.pickle(a.ord)
        state.pickle(a.event)
        state.pickle(a.createdAt)
      }
      override def unpickle(implicit state: UnpickleState): VerifiedEvent = {
        val ord       = state.unpickle[EventOrd]
        val event     = state.unpickle[Event]
        val createdAt = state.unpickle[Instant]
        VerifiedEvent(ord, event, createdAt)
      }
    }

  implicit lazy val picklerVerifiedEventSeq: Pickler[VerifiedEvent.Seq] =
    iterablePickler

  implicit lazy val picklerVerifiedEventNonEmptySeq: Pickler[VerifiedEvent.NonEmptySeq] =
    new Pickler[VerifiedEvent.NonEmptySeq] {
      override def pickle(a: VerifiedEvent.NonEmptySeq)(implicit state: PickleState): Unit = {
        state.pickle(a.head)
        state.pickle(a.tail)
      }
      override def unpickle(implicit state: UnpickleState): VerifiedEvent.NonEmptySeq = {
        val head = state.unpickle[VerifiedEvent]
        val tail = state.unpickle[VerifiedEvent.Seq]
        VerifiedEvent.NonEmptySeq(head, tail)
      }
    }

  implicit lazy val picklerErrorMsgOrVerifiedEventSeq: Pickler[ErrorMsg \/ VerifiedEvent.Seq] =
    pickleDisj


  // ===================================================================================================================

  import Rev5.SavedViewPicklers._

  implicit lazy val picklerProject: Pickler[Project] =
    new Pickler[Project] {
      override def pickle(a: Project)(implicit state: PickleState): Unit = {
        state.pickle(a.name)
        state.pickle(a.config)
        state.pickle(a.content)
        state.pickle(a.manualIssues)
        state.pickle(a.savedViews)
        state.pickle(a.idCeilings)
      }
      override def unpickle(implicit state: UnpickleState): Project = {
        val name          = state.unpickle[Project.Name]
        val config        = state.unpickle[ProjectConfig]
        val content       = state.unpickle[ProjectContent]
        val manualIssues  = state.unpickle[ManualIssues]
        val savedViews    = state.unpickle[savedview.SavedViews.Optional]
        val idCeilings    = state.unpickle[IdCeilings]
        Project(name, config, content, manualIssues, savedViews, idCeilings)
      }
    }

  implicit lazy val picklerProjectAndOrd: Pickler[ProjectAndOrd] =
    new Pickler[ProjectAndOrd] {
      override def pickle(a: ProjectAndOrd)(implicit state: PickleState): Unit = {
        state.pickle(a.project)
        state.pickle(a.ord)
      }
      override def unpickle(implicit state: UnpickleState): ProjectAndOrd = {
        val project = state.unpickle[Project]
        val ord     = state.unpickle[Option[EventOrd.Latest]]
        ProjectAndOrd(project, ord)
      }
    }

}
