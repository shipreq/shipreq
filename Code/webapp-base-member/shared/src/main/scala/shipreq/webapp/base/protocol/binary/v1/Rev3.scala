package shipreq.webapp.base.protocol.binary.v1

import boopickle.{ConstPickler, DefaultBasic}
import japgolly.microlibs.nonempty.NonEmptySet
import scala.reflect.ClassTag
import shipreq.base.util.NonEmptyArraySeq
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.text.{AtomTC, Text}

/** v1.3 */
object Rev3 {
  import boopickle.DefaultBasic._
  import BaseData._
  import Events._
  import BaseMemberData1._
  import BaseMemberData2._

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
}