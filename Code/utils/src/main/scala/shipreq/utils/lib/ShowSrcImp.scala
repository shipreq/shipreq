package shipreq.utils.lib

import japgolly.microlibs.nonempty._
import nyaya.util.Multimap
import org.apache.commons.lang3.StringEscapeUtils.escapeJava
import scala.collection.GenTraversable
import scalaz.{-\/, \/, \/-}
import shipreq.base.util._
import ShowSrc.State

object ShowSrcGenericImp {
  import shipreq.base.util.TaggedTypes.TaggedType

  private val importISubset    = "import shipreq.base.util.ISubset"
  private val importNEV        = "import japgolly.microlibs.nonempty.NonEmptyVector"
  private val importNES        = "import japgolly.microlibs.nonempty.NonEmptySet"
  private val importScalaExt   = "import shipreq.base.util.ScalaExt._"
  private val importScalazDisj = """import scalaz.{\/-, -\/}"""

  implicit val unit: ShowSrc[Unit] =
    ShowSrc.const(_ append "()")

  implicit val bool: ShowSrc[Boolean] =
    ShowSrc(_ append _)

  implicit val int: ShowSrc[Int] =
    ShowSrc(_ append _)

  implicit val long: ShowSrc[Long] = {
    val min = Int.MinValue.toLong
    val max = Int.MaxValue.toLong
    ShowSrc { (sb, a) =>
      sb append a
      if (a >= max || a <= min)
        sb append 'L'
    }
  }

  implicit val char: ShowSrc[Char] =
    ShowSrc { (sb, a) =>
      sb append '\''
      sb append escapeJava(char.toString)
      sb append '\''
    }

  implicit val string: ShowSrc[String] =
    ShowSrc { (sb, a) =>
      sb append '"'
      sb append escapeJava(a)
      sb append '"'
    }

  implicit def option[A: ShowSrc]: ShowSrc[Option[A]] =
    ShowSrc.init(importScalaExt) { (s, o) =>
      o match {
        case None    => s append "none"
        case Some(a) => s <~ a; s append ".some"
      }
    }

  def seqLike[S[x] <: GenTraversable[x], A: ShowSrc](name: String, typ: Option[String] = None): ShowSrc[S[A]] = {
    val applyType: State => Unit =
      typ match {
        case None => _ => ()
        case Some(t) => s => {
          s append '['
          s append t
          s append ']'
        }
      }
    ShowSrc { (s, v) =>
      s append name
      if (v.isEmpty) {
        s append ".empty"
        applyType(s)
      } else {
        applyType(s)
        s.varargs(v)
      }
    }
  }

  implicit def vector[A: ShowSrc]: ShowSrc[Vector[A]] = seqLike("Vector")
  implicit def list  [A: ShowSrc]: ShowSrc[List  [A]] = seqLike("List")

  def set[A: ShowSrc](typ: String): ShowSrc[Set[A]] = seqLike("Set", Option(typ))

  /*
//  def map[K: ShowSrc, V: ShowSrc](): ShowSrc[Map[K, V]] =
//    map("Map.empty")

  def map[K: ShowSrc, V: ShowSrc](k: String, v: String): ShowSrc[Map[K, V]] =
    map(s"Map.empty[$k,$v]")

  def map[K: ShowSrc, V: ShowSrc](empty: String): ShowSrc[Map[K, V]] =
    mapImpl(_ append empty)

  def mapImpl[K: ShowSrc, V: ShowSrc](empty: State => Unit): ShowSrc[Map[K, V]] = {
    ShowSrc { (s, m) =>
      empty(s)
      if (m.nonEmpty)
        for ((k, v) <- m) {
          s append ".updated("
          s <~ k
          s append ','
          s <~ v
          s append ')'
        }
    }
  }
  */

  def map[K: ShowSrc, V: ShowSrc](): ShowSrc[Map[K, V]] =
    mapImpl(None)

  def map[K: ShowSrc, V: ShowSrc](k: String, v: String): ShowSrc[Map[K, V]] =
    mapImpl(Some(s"[$k,$v]"))

  def mapImpl[K: ShowSrc, V: ShowSrc](types: Option[String]): ShowSrc[Map[K, V]] = {
    implicit val pairs: ShowSrc[(K, V)] = ShowSrc { (s, p) =>
      s <~ p._1
      s append " -> "
      s <~ p._2
    }
    val mapfn = "Map" + types.getOrElse("")
    ShowSrc { (s, m) =>
      if (m.isEmpty) {
        s append "Map.empty"
        types foreach (s append _)
      } else
        s.fnN(mapfn, m.toSeq)
    }
  }

  implicit def scalazDisjunction[A: ShowSrc, B: ShowSrc]: ShowSrc[A \/ B] =
    ShowSrc.init(importScalazDisj) { (s, e) =>
      e match {
        case -\/(v) => s.fn1("-\\/", v)
        case \/-(v) => s.fn1("\\/-", v)
      }
    }

  implicit def nev[A: ShowSrc]: ShowSrc[NonEmptyVector[A]] =
    ShowSrc.init(importNEV)((s, a) => s.fnN("NonEmptyVector", a.whole))

  def nevV[A: ShowSrc]: ShowSrc[NonEmptyVector[A]] =
    ShowSrc.init(importNEV)((s, a) => s.fnN("NonEmptyVector.varargs", a.whole))

  implicit def nes[A: ShowSrc]: ShowSrc[NonEmptySet[A]] =
    ShowSrc.init(importNES)((s, a) => s.fnN("NonEmptySet", a.whole))

  def taggedType[T <: TaggedType](name: String)(implicit u: ShowSrc[T#U]): ShowSrc[T] = {
    val n = name.trim
    ShowSrc((s, t) => s.fn1(n, t.value)(u.narrow))
  }

  def isoBool[A](testPos: A => Boolean, pos: String, neg: String): ShowSrc[A] =
    ShowSrc((s, a) => s append (if (testPos(a)) pos else neg))

  def multimap[K, L[_], V](empty: String)(implicit K: ShowSrc[K], VS: ShowSrc[L[V]]): ShowSrc[Multimap[K, L, V]] = {
    implicit val ms: ShowSrc[Map[K, L[V]]] = map()
    ShowSrc { (s, a) =>
      s append empty
      if (a.nonEmpty) {
        s append " ++ "
        s <~ a.m
      }
    }
  }

  def imap[K, V: ShowSrc](empty: String): ShowSrc[IMap[K, V]] =
    ShowSrc { (s, imap) =>
      s append empty
      if (imap.nonEmpty)
        s.fnN(".addAll", imap.values)
    }

  def isubset[A: ShowSrc]: ShowSrc[ISubset[A]] = {
    implicit val anes = nes[A]
    ShowSrc.init(importISubset){(s, a) =>
      s append "ISubset."
      a match {
        case ISubset.All()    => s append "All()"
        case ISubset.Only(as) => s.fn1("Only", as)
        case ISubset.Not(as)  => s.fn1("Not", as)
      }
    }
  }

  def digraphUni[A](fix: String)(implicit A: ShowSrc[A], AS: ShowSrc[Set[A]]): ShowSrc[Digraph.UniDir[A]] =
    multimap(fix + ".emptyUniDir")(A, AS)

  def digraphBi[A](fix: String)(implicit B: ShowSrc[Digraph.UniDir[A]]): ShowSrc[Digraph.BiDir[A]] =
    ShowSrc((s, a) =>
      if (a.forwards.isEmpty) {
        s append fix
        s append ".emptyBiDir"
      } else
        s.cc1(fix + ".BiDir", Digraph.BiDir unapply a))

  def wrapAndType[A](ss: ShowSrc[A], typ: String): ShowSrc[A] =
    ShowSrc { (s, a) =>
      ss(s, a)
      s append ": "
      s append typ
    }
}

// =====================================================================================================================

object ShowSrcDataImp {
  import ShowSrcGenericImp._
  import shipreq.webapp.base.data._
  import shipreq.webapp.base.text.{Atom, Text}

  val importData     = "import shipreq.webapp.base.data._"
  val importDataI    = "import shipreq.webapp.base.data.DataImplicits._"
  val importRCTrie   = "import shipreq.webapp.base.data.ReqCode.Trie.{Branch => τb, Value => τv}"
  val importMnemonic = "import shipreq.webapp.base.data.ReqType.Mnemonic"
  val importUnivEq   = "import shipreq.base.util.UnivEq"

  private implicit class StringExt(val s: String) extends AnyVal {
    def @@[A](a: ShowSrc[A]): ShowSrc[A] = a.intoVar(s)
  }

  private def data[A](f: (State, A) => Unit): ShowSrc[A] =
    ShowSrc.init(importData)(f)

  private def dataBool[A](testPos: A => Boolean, pos: String, neg: String): ShowSrc[A] =
    isoBool(testPos, pos, neg) init importData

  private def imapI[K, V: ShowSrc](objName: String): ShowSrc[IMap[K, V]] =
    imap[K, V](s"emptyDataMap($objName)") init importDataI

  implicit val deletionReasonId         = taggedType[DeletionReasonId          ]("DeletionReasonId          ")
  implicit val useCaseId                = taggedType[UseCaseId                 ]("UseCaseId                 ")
  implicit val useCaseStepId            = taggedType[UseCaseStepId             ]("UseCaseStepId             ")
  implicit val genericReqId             = taggedType[GenericReqId              ]("GenericReqId              ")
  implicit val reqCodeId                = taggedType[ReqCodeId                 ]("ReqCodeId                 ")
  implicit val customReqTypeId          = taggedType[CustomReqTypeId           ]("CustomReqTypeId           ")
  implicit val customIssueTypeId        = taggedType[CustomIssueTypeId         ]("CustomIssueTypeId         ")
  implicit val applicableTagId          = taggedType[ApplicableTagId           ]("ApplicableTagId           ")
  implicit val tagGroupId               = taggedType[TagGroupId                ]("TagGroupId                ")
  implicit val customFieldTagId         = taggedType[CustomField.Tag.Id        ]("CustomField.Tag.Id        ")
  implicit val customFieldTextId        = taggedType[CustomField.Text.Id       ]("CustomField.Text.Id       ")
  implicit val customFieldImplicationId = taggedType[CustomField.Implication.Id]("CustomField.Implication.Id")
  implicit val hashRefKey               = taggedType[HashRefKey                ]("HashRefKey                ")
  implicit val reqTypePos               = taggedType[ReqTypePos                ]("ReqTypePos                ")
  implicit val fieldRefKey              = taggedType[FieldRefKey               ]("FieldRefKey               ")

  implicit val reqTypeMnemonic = taggedType[ReqType.Mnemonic]("Mnemonic") init importMnemonic

  implicit val tagId: ShowSrc[TagId] =
    data((s, a) => a match {
      case id: ApplicableTagId => s <~ id
      case id: TagGroupId      => s <~ id
    })

  implicit val reqId: ShowSrc[ReqId] =
    data((s, a) => a match {
      case id: GenericReqId => s <~ id
      case id: UseCaseId    => s <~ id
    })

  implicit val customFieldId: ShowSrc[CustomFieldId] =
    data((s, a) => a match {
      case id: CustomField.Implication.Id => s <~ id
      case id: CustomField.Tag        .Id => s <~ id
      case id: CustomField.Text       .Id => s <~ id
    })

  implicit val live = dataBool(Live.from, "Live", "Dead")

  implicit val implicationRequired = dataBool(ImplicationRequired.from, "ImplicationRequired", "ImplicationRequired.Not")

  implicit val mandatory = dataBool(Mandatory.from, "Mandatory", "Mandatory.Not")

  implicit val deletable = dataBool(Deletable.from, "Deletable", "Deletable.Not")

  implicit val mutexChildren = dataBool(MutexChildren.from, "MutexChildren", "MutexChildren.Not")

  implicit val setTagId = set("TagId")(tagId)
  implicit val setReqId = set("ReqId")(reqId)
  implicit val setUseCaseStepId = set("UseCaseStepId")(useCaseStepId)

  implicit val setReqCodeId       = set(null)(reqCodeId)
  implicit val setReqTypeMnemonic = set(null)(reqTypeMnemonic)
  implicit val setApplicableTagId = set(null)(applicableTagId)

  implicit val implicationsUni: ShowSrc[Implications.UniDir] =
    digraphUni("Implications")(reqId, setReqId)

  implicit val implications: ShowSrc[Implications.BiDir] =
    "implications" @@ digraphBi("Implications")

  implicit val reqDataTags: ShowSrc[ReqData.Tags] =
    "reqDataTags" @@ multimap("ReqData.emptyTags")

  def textAtom(prefix: String): ShowSrc[Atom.AnyAtom] = {
    import Atom._
    type A = AnyAtom
    lazy val atoms0: ShowSrc[Vector[A]] = vector(atom1)
    lazy val atoms10: ShowSrc[NonEmptyVector[Vector[A]]] = nevV(atoms0)
    lazy val atom1: ShowSrc[A] =
      ShowSrc { (s, atom) =>
        s append prefix
        atom match {
          case a: NewLine         # BlankLine      => s append "blankLine"
          case a: Literal         # Literal        => s.fn1("Literal", a.value)
          case a: ReqRef          # ReqRef         => s.fn1("ReqRef", a.value)
          case a: ReqRef          # CodeRef        => s.fn1("CodeRef", a.value)
          case a: UseCaseStepRef  # UseCaseStepRef => s.fn1("UseCaseStepRef", a.value)
          case a: Issue           # Issue          => s.fn2("Issue", a.typ, a.desc)(implicitly, inlineIssueDescZ)
          case a: PlainTextMarkup # WebAddress     => s.fn1("WebAddress", a.value)
          case a: PlainTextMarkup # EmailAddress   => s.fn1("EmailAddress", a.value)
          case a: PlainTextMarkup # MathTeX        => s.fn1("MathTeX", a.value)
          case a: TagRef          # TagRef         => s.fn1("TagRef", a.value)
          case a: ListMarkup      # UnorderedList  => s.fn1("UnorderedList", a.items)(atoms10.narrow)
        }
      }
    atom1
  }

  def textImport(t: Text.Generic)(abbrev: String): String =
    s"import shipreq.webapp.base.text.Text.{${t.getClass.getSimpleName.replace("$", "")} => $abbrev}"

  def text(t: Text.Generic)(abbrev: String): (ShowSrc[t.OptionalText], ShowSrc[t.NonEmptyText]) = {
    val `import` = textImport(t)(abbrev)
    val a = textAtom(abbrev + ".").narrow[t.Atom] // .asInstanceOf[ShowSrc[t.Atom]]
    val z = vector(a) init `import`
    val n = nev   (a) init `import`
    (z, n)
  }

  def text2(t: Text.Generic)(abbrev: String): (ShowSrc[t.OptionalText], ShowSrc[t.NonEmptyText]) = {
    val (z, n) = text(t)(abbrev)
    val name = abbrev.toLowerCase
    (z intoVar name, n intoVar name)
  }

  implicit      val (   useCaseTitleZ,    useCaseTitleN) = text2(Text.UseCaseTitle   )("UCT")
  implicit      val (    useCaseStepZ,     useCaseStepN) = text2(Text.UseCaseStep    )("UCST")
  implicit      val ( codeGroupTitleZ,  codeGroupTitleN) = text2(Text.CodeGroupTitle )("CGT")
  implicit      val (genericReqTitleZ, genericReqTitleN) = text2(Text.GenericReqTitle)("GRT")
  implicit      val (customTextFieldZ, customTextFieldN) = text2(Text.CustomTextField)("CTF")
  implicit      val ( deletionReasonZ,  deletionReasonN) = text2(Text.DeletionReason )("DR")
  implicit lazy val (inlineIssueDescZ, inlineIssueDescN) = text (Text.InlineIssueDesc)("IID")

  implicit lazy val reqDataText: ShowSrc[ReqData.Text] = {
    implicit val vs =
      map[ReqId, Text.CustomTextField.NonEmptyText]("ReqId", "CTF.NonEmptyText")
        .init(textImport(Text.CustomTextField)("CTF"))
        .intoVar("customTextForReq")
    "reqDataText" @@ wrapAndType(map(): ShowSrc[ReqData.Text], "ReqData.Text")
  }

  implicit val reqCodeNode: ShowSrc[ReqCode.Node] =
    data((s, n) => s.fn1("ReqCode.Node", n.value))

  implicit val deadCodeGroup: ShowSrc[DeadCodeGroup] =
    data((s, g) => s.cc2("DeadCodeGroup", DeadCodeGroup unapply g)(reqCodeId, codeGroupTitleZ))

  implicit val deadCodeGroupField: ShowSrc[ReqCode.DeadGroup] =
    option(deadCodeGroup)

  implicit val liveCodeGroup: ShowSrc[LiveCodeGroup] =
    data((s, g) => s.cc2("LiveCodeGroup", LiveCodeGroup unapply g)(reqCodeId, codeGroupTitleZ))

  implicit val codeGroup: ShowSrc[CodeGroup] =
    data((s, t) => t match {
      case g: LiveCodeGroup => s <~ g
      case g: DeadCodeGroup => s <~ g
    })

  implicit val reqCodeDataReqInactive: ShowSrc[ReqCode.ReqInactive] =
    multimap[ReqId, Set, ReqCodeId]("ReqCode.emptyReqInactive")

  implicit val reqCodeActiveReq: ShowSrc[ReqCode.ActiveReq] =
    data((s, d) => s.cc4("ReqCode.ActiveReq", ReqCode.ActiveReq unapply d))

  implicit val reqCodeActiveGroup: ShowSrc[ReqCode.ActiveGroup] =
    data((s, d) => s.cc2("ReqCode.ActiveGroup", ReqCode.ActiveGroup unapply d))

  implicit val reqCodeInactive: ShowSrc[ReqCode.Inactive] =
    data((s, d) => s.cc2("ReqCode.Inactive", ReqCode.Inactive unapply d))

  implicit val reqCodeData: ShowSrc[ReqCode.Data] =
    data((s, t) => t match {
      case d: ReqCode.ActiveReq   => s <~ d
      case d: ReqCode.ActiveGroup => s <~ d
      case d: ReqCode.Inactive    => s <~ d
    })

  def mtrie[K: ShowSrc, V: ShowSrc](branchCtor: String, valueCtor: String, trieType: String, trieVarname: String): ShowSrc[MTrie.Trie[K, V]] = {
    import MTrie.{Branch, Node, Trie, Value}
         val value : ShowSrc[Value[K, V]]  = ShowSrc((s, v) => s.cc1(valueCtor, Value unapply v))
         val valueO                        = option(value)
    lazy val branch: ShowSrc[Branch[K, V]] = ShowSrc((s, t) => s.cc2(branchCtor, Branch unapply t)(valueO, trie))
    lazy val node  : ShowSrc[Node[K, V]]   = ShowSrc((s, n) => n.fold(s.<~(_)(branch), s.<~(_)(value)))
    lazy val trie  : ShowSrc[Trie[K, V]]   = {
      val t: ShowSrc[Trie[K, V]] = map()(implicitly, node)
      wrapAndType(t, trieType) intoVar trieVarname
    }
    trie
  }

  implicit val reqCodeTrie: ShowSrc[ReqCode.Trie] =
    (mtrie("τb", "τv", "ReqCode.Trie", "reqCodeTrie"): ShowSrc[ReqCode.Trie]) init importRCTrie init importData

  implicit val reqCodes: ShowSrc[ReqCodes] =
    data((s, rc) => s.cc1("ReqCodes", ReqCodes unapply rc)(reqCodeTrie))

  implicit val staticReqType: ShowSrc[StaticReqType] =
    data((s, a) => a match {
      case StaticReqType.UseCase => s append "StaticReqType.UseCase"
    })

  implicit val reqTypeId: ShowSrc[ReqTypeId] =
    ShowSrc((s, a) => a match {
      case id: CustomReqTypeId => s <~ id
      case rt: StaticReqType   => s <~ rt
    })

  implicit def pubidT[T <: ReqTypeId: ShowSrc]: ShowSrc[PubidT[T]] =
    data((s, a) => s.cc2("PubidT", PubidT unapply a))

  implicit val pubidRegister: ShowSrc[PubidRegister] = {
    implicit val mm = multimap[ReqTypeId, Vector, ReqId]("PubidRegister.emptyMM")
    "pubidRegister" @@ data((s, r) =>
      if (r.value.isEmpty)
        s append "PubidRegister.empty"
      else
        s.cc1("PubidRegister", PubidRegister unapply r))
  }

  implicit val genericReq: ShowSrc[GenericReq] =
    "greq" @@ data((s, r) =>
      s.cc4("GenericReq", GenericReq unapply r)(implicitly, implicitly, genericReqTitleZ, implicitly))

  def vectorTree[A](implicit a: ShowSrc[A]): ShowSrc[VectorTree[A]] = {
    import VectorTree._
    val nodeAlias = "VTN"
    lazy val node    : ShowSrc[Node      [A]] = ShowSrc((s, n) => s.cc2(nodeAlias, Node unapply n)(a, children))
    lazy val children: ShowSrc[Children  [A]] = vector(node)
         val root    : ShowSrc[VectorTree[A]] = ShowSrc((s, t) => s.cc1("VectorTree", VectorTree unapply t)(children))
    root init s"import shipreq.base.util.VectorTree, VectorTree.{Node => $nodeAlias}"
  }

  implicit val useCaseStep: ShowSrc[UseCaseStep] =
    data((s, a) => s.cc3("UseCaseStep", UseCaseStep unapply a))

  implicit val useCaseStepsTree: ShowSrc[UseCaseSteps.Tree] =
    "useCaseStepTree" @@ vectorTree

  implicit val useCaseSteps: ShowSrc[UseCaseSteps] =
    data((s, a) => s.cc1("UseCaseSteps", UseCaseSteps unapply a))

  implicit val useCase: ShowSrc[UseCase] =
    data((s, a) => s.cc6("UseCase", UseCase unapply a))

  implicit val req: ShowSrc[Req] =
    ShowSrc((s, req) => req match {
      case gr: GenericReq => s <~ gr
      case uc: UseCase    => s <~ uc
    })

  implicit val genericReqIMap: ShowSrc[GenericReqIMap] =
    "genericReqs" @@ imapI("GenericReq")

  implicit val useCaseIMap: ShowSrc[UseCaseIMap] =
    "useCaseIMap" @@ imapI("UseCase")

  implicit val useCaseStepFlowUni: ShowSrc[UseCases.StepFlow.UniDir] =
    digraphUni[UseCaseStepId]("UseCases.StepFlow")

  implicit val useCaseStepFlow: ShowSrc[UseCases.StepFlow.BiDir] =
    "useCaseStepFlow" @@ digraphBi("UseCases.StepFlow")

  implicit val useCases: ShowSrc[UseCases] =
    "useCases" @@ data { (s, a) =>
      s.cc2("UseCases.Stateless", UseCases.Stateless unapply UseCases.statelessIso.reverseGet(a))
      s append ".withState"
    }

  implicit val requirements: ShowSrc[Requirements] =
    data((s, r) =>
      if (r.isEmpty)
        s append "Requirements.empty"
      else
        s.cc3("Requirements", Requirements unapply r))

  implicit val customIssueType: ShowSrc[CustomIssueType] =
    data((s, a) => s.cc4("CustomIssueType", CustomIssueType unapply a))

  implicit val customIssueTypeIMap: ShowSrc[CustomIssueTypeIMap] =
    "customIssueTypes" @@ imapI("CustomIssueType")

  implicit val customReqType: ShowSrc[CustomReqType] =
    data((s, a) => s.cc6("CustomReqType", CustomReqType unapply a))

  implicit val customReqTypeIMap: ShowSrc[ReqTypes.Custom] =
    "customReqTypes" @@ imapI("CustomReqType")

  implicit val reqTypes: ShowSrc[ReqTypes] =
    data((s, a) => s.cc1("ReqTypes", ReqTypes unapply a))

  implicit val applicableReqTypes: ShowSrc[Field.ApplicableReqTypes] = isubset

  implicit val staticField: ShowSrc[StaticField] =
    data((s, a) => a match {
      case StaticField.NormalAltStepTree => s append "StaticField.NormalAltStepTree"
      case StaticField.ExceptionStepTree => s append "StaticField.ExceptionStepTree"
      case StaticField.StepGraph         => s append "StaticField.StepGraph"
      case StaticField.ImplicationGraph  => s append "StaticField.ImplicationGraph"
    })

  implicit val fieldId: ShowSrc[FieldId] =
    ShowSrc((s, a) => a match {
      case id: CustomFieldId => s <~ id
      case sf: StaticField   => s <~ sf
    })

  implicit val customFieldTag: ShowSrc[CustomField.Tag] =
    data((s, a) => s.cc5("CustomField.Tag", CustomField.Tag unapply a))

  implicit val customFieldText: ShowSrc[CustomField.Text] =
    data((s, a) => s.cc6("CustomField.Text", CustomField.Text unapply a))

  implicit val customFieldImplication: ShowSrc[CustomField.Implication] =
    data((s, a) => s.cc5("CustomField.Implication", CustomField.Implication unapply a))

  implicit val customField: ShowSrc[CustomField] =
    ShowSrc((s, a) => a match {
      case f: CustomField.Tag         => s <~ f
      case f: CustomField.Text        => s <~ f
      case f: CustomField.Implication => s <~ f
    })

  implicit val customFieldIMap: ShowSrc[IMap[CustomFieldId, CustomField]] =
    "customFields" @@ imapI("CustomField")

  implicit val fieldSet: ShowSrc[FieldSet] =
    "fieldSet" @@ data((s, a) => s.cc2("FieldSet", FieldSet unapply a))

  implicit val tagGroup: ShowSrc[TagGroup] =
    data((s, a) => s.cc5("TagGroup", TagGroup unapply a))

  implicit val applicableTag: ShowSrc[ApplicableTag] =
    data((s, a) => s.cc5("ApplicableTag", ApplicableTag unapply a))

  implicit val tag: ShowSrc[Tag] =
    ShowSrc((s, a) => a match {
      case f: ApplicableTag => s <~ f
      case f: TagGroup      => s <~ f
    })

  implicit val tagInTree: ShowSrc[TagInTree] =
    data((s, a) => s.cc2("TagInTree", TagInTree unapply a))

  implicit val tagTree: ShowSrc[TagTree] =
    "tagTree" @@ imap("TagTree.empty")

  implicit val tags: ShowSrc[Tags] =
    data((s, a) => s.cc1("Tags", Tags unapply a))

  implicit val idCeilings: ShowSrc[IdCeilings] =
    data((s, a) => s.cc8("IdCeilings", IdCeilings unapply a))

  implicit val deletionReasonsReqApplication: ShowSrc[DeletionReasons.ReqApplication] =
    multimap[ReqId, Vector, Option[DeletionReasonId]]("DeletionReasons.emptyReqApplication")

  implicit val deletionReasons: ShowSrc[DeletionReasons] =
    data((s, a) => s.cc2("DeletionReasons", DeletionReasons unapply a, "\n    "))

  implicit val projectConfig: ShowSrc[ProjectConfig] =
    data((s, a) => s.cc4("ProjectConfig", ProjectConfig unapply a, "\n    "))
//    source((s, a) => s.cc4("ProjectConfig", ProjectConfig unapply a))

  implicit val projectContent: ShowSrc[ProjectContent] =
    data((s, a) => s.cc6("ProjectContent", ProjectContent unapply a, "\n    "))

  implicit val project: ShowSrc[Project] = {
    implicit val x: ShowSrc[reqtable.SavedViews.Optional] = ShowSrc.const(_ append "None") // TODO
    data((s, a) => s.cc5("Project", Project unapply a, "\n  "))
//    source((s, a) => s.cc6("Project", Project unapply a))
  }
}
