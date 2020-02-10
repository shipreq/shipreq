package shipreq.webapp.base

import japgolly.microlibs.adt_macros.AdtMacros._
import japgolly.microlibs.nonempty._
import japgolly.microlibs.recursion._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import java.time.Instant
import nyaya.gen._
import nyaya.util._
import monocle._
import monocle.function.Field1.first
import monocle.function.Field2.second
import scala.annotation.tailrec
import scalaz.{-\/, Need, \/-}
import scalaz.std.list._
import scalaz.std.option.{none => _, _}
import scalaz.std.set._
import scalaz.std.stream._
import scalaz.std.vector._
import shipreq.base.test.BaseUtilGen._
import shipreq.base.util._
import shipreq.base.util.Debug._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.TaggedTypes.TaggedInt
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.ProjectAndOrd
import shipreq.webapp.base.issue.IssueCategory
import shipreq.webapp.base.sort.SortMethod
import shipreq.webapp.base.test._
import shipreq.webapp.base.text.{Grammar, GrammarSpec, Text}
import shipreq.webapp.base.util.{GenericData, PreProcessor}
import shipreq.webapp.base.user._

// TODO RandomData is inaccurate in that CorrectionParts aren't applied.

object RandomData {
  import DataImplicits._
  import Field.ApplicableReqTypes
  import MTrie.Ops
  import Optics.Implicits._
  import ReqType.Mnemonic
  import TestOptics.{customReqTypesLive => _, _}
  import WebappBaseGen._

  /*
  def genmodL[A, B](l: Lens[A, B])(g: B => Gen[B])(a: A): Gen[A] =
    g(l get a) map (l.set(_)(a))
  */

//    val trimLeftR = "^\\s+".r
//    def trimLeft(s: String) = trimLeftR.replaceAllIn(s, "")
//    val trimRightR = "\\s+$".r
//    def trimRight(s: String) = trimRightR.replaceAllIn(s, "")

  @tailrec def dropHead[A](v: Vector[A])(f: A => Boolean): Vector[A] =
    if (v.nonEmpty && f(v.head))
      dropHead(v.tail)(f)
    else
      v

  @tailrec def dropLast[A](v: Vector[A])(f: A => Boolean): Vector[A] =
    if (v.nonEmpty && f(v.last))
      dropLast(v.init)(f)
    else
      v

  private def disableUnicode = false

  val unicodeChar: Gen[Char] =
    if (disableUnicode)
      Gen.ascii
    else {
      val a = new Array[Char](1)
      val chars = (0 to 65535)
        .iterator
        .map(_.toChar)
        .filter { c =>
          a(0) = c
          PreProcessor.fixCharMultiLine(a, 0)
          a(0) ==* c
        }
        .toVector
      Gen.choose_!(chars)
    }

  val unicodeString : Gen[String] = unicodeChar.string
  val unicodeString1: Gen[String] = unicodeChar.string1

//  private val _charPredAllChars = ('\u0001' to '\ud7ff').seq
  private val _charPredAllChars = ('\u0001' to '\u0100').seq
//  private val _charPredAllChars = ('\u0020' to '\u0100').seq
//  private val _charPredAllChars = ('\u0020' to '\u0080').seq
  def charPred(p: org.parboiled2.CharPredicate): Gen[Char] =
    //Gen.choose_!(_charPredAllChars filter p.apply)
    Gen.chooseArray_!((_charPredAllChars filter p.apply).toArray)

  def grammarChars(c: GrammarSpec.Chars): Gen[Char] =
    Gen.chooseChar(c.ch1, c.chn, c.rs: _*)

  def grammarStr1[G](g: G)(f: G => GrammarSpec.Chars, w: G => GrammarSpec.Chars, l: G => GrammarSpec.Length): Gen[String] =
    for {
      h <- grammarChars(f(g))
      t <- grammarChars(w(g)).list(0 to l(g).minus1.max)
    } yield (h :: t).mkString

  class CaseInsensitive(val norm: String, val str: String) {
    override def hashCode = norm.##
    override def equals(o: Any) = o match {
      case x: CaseInsensitive => norm == x.norm
      case _ => false
    }
  }
  def CaseInsensitive(s: String): CaseInsensitive =
    new CaseInsensitive(s.toLowerCase, s)

  def legalGrammar[G](g: G)(first: G => GrammarSpec.Chars, rest: G => GrammarSpec.Chars): Stream[String] = {
    val g1 = first(g).toStream.map(_.toString)
    val gn = rest(g).toStream.map(_.toString)
    def grow(ss: Stream[String]): Stream[String] = {
      val x = ss append ss.flatMap(s => gn.map(s + _))
      x append grow(x)
    }
    grow(g1)
  }

  def grammarFixer[G](g: G)(first: G => GrammarSpec.Chars, rest: G => GrammarSpec.Chars) = {
    val all = legalGrammar(g)(first, rest)
    def fix(used: Set[String]): String =
      all.filter(!used.contains(_)).head
    Distinct.Fixer.lift(fix)
  }

  def grammarFixerIgnoreCase[G](g: G)(first: G => GrammarSpec.Chars, rest: G => GrammarSpec.Chars) = {
    val all = legalGrammar(g)(first, rest) map CaseInsensitive
    def fix(used: Set[CaseInsensitive]): CaseInsensitive =
      all.filter(!used.contains(_)).head
    Distinct.Fixer.lift(fix).xmap(_.str)(CaseInsensitive)
  }

  def someOfWithDups[A, B](as: Seq[A])(f: A => Gen[B]): Gen[Vector[B]] =
    Gen.tryGenChoose(as).fold[Gen[Vector[B]]](Gen pure Vector.empty)(
      _.vector.flatMap(Gen.traverse(_)(f)))

  val id =
    Gen.chooseInt(1, 1024 * 64)

  val shortText1        = unicodeChar.string(1 to WebappConfig.shortTextMaxLength)
  val shortText         = unicodeChar.string(0 to WebappConfig.shortTextMaxLength)
  val optionalLargeText = unicodeChar.string(1 to WebappConfig.largeTextMaxLength).option

  def revAndIMap[D, I <: TaggedInt](r: Gen[List[D]])
                                    (implicit i: DataIdAux[D, I], j: TestDataIdAux[D, I]): Gen[IMap[I, D]] = {
    val d = distinctId[D, I].lift[List]
    val g = d.run andThen (i.emptyIMap ++ _)
    r map g
  }

  def distinctId[D, I <: TaggedInt](implicit i: DataIdAux[D, I], j: TestDataIdAux[D, I]) =
    Distinct.fint.xmap(j.mkId)(_.value).distinct.contramap[D](i.id, j.setId)

  def imapToMapLens[K, V] = Lens((_: IMap[K, V]).underlyingMap)(v => _ replaceUnderlying v)

  val live =
    Gen.choose[Live](Live, Live, Live, Dead)

  val on =
    Gen.boolean.map(On.when)

//  val liveUsually =
//    Gen.int.map(i => if ((i & 7) == 0) Dead else Live)

  val implicationRequired =
    Gen.choose[ImplicationRequired](ImplicationRequired, ImplicationRequired.Not)

  val mandatory =
    Gen.choose[Mandatory](Mandatory, Mandatory.Not)

  val hashRefKey: Gen[HashRefKey] =
    grammarStr1(Grammar.hashRefKey)(_.firstChar, _.tailChars, _.length) map HashRefKey

  val dir =
    Gen.choose[Direction](Forwards, Backwards)

  val filterDead =
    Gen.choose[FilterDead](ShowDead, HideDead)

  def obfuscated[A]: Gen[Obfuscated[A]] =
    Gen.alphaNumeric.string(4 to 12).map(Obfuscated.apply[A])

  lazy val username: Gen[Username] = {
    val x = WebappConfig.usernameLength.min - 2
    val y = WebappConfig.usernameLength.max - 2
    for {
      a <- Gen.lower
      b <- Gen.chooseChar('_', 'a' to 'z', '0' to '9').string(x to y)
      c <- Gen.chooseChar('a', 'b' to 'z', '0' to '9')
    } yield Username(a + b + c)
  }

  lazy val errorMsg: Gen[ErrorMsg] =
    Gen.ascii.string(1 to 6).map(ErrorMsg.apply)

  lazy val emailAddr: Gen[EmailAddr] =
    Gen.ascii.string(0 to 6).map(EmailAddr.apply)

  lazy val plainTextPassword: Gen[PlainTextPassword] =
    unicodeString.map(PlainTextPassword.apply)

  lazy val personName: Gen[PersonName] =
    unicodeString1.map(PersonName.apply)

  lazy val verificationToken: Gen[VerificationToken] =
    Gen.ascii.string(1 to 6).map(VerificationToken.apply)

  // -------------------------------------------------------------------------------------------------------------------
  // Custom issue types

  val customIssueTypeId =
    id map CustomIssueTypeId

  val customIssueType =
    Gen.apply4(CustomIssueType.apply)(customIssueTypeId, hashRefKey, optionalLargeText, live)

  /** HashRefKey uniqueness enforced in Project, not here */
  val customIssueTypes =
    revAndIMap(customIssueType.list)

  // -------------------------------------------------------------------------------------------------------------------
  // ReqTypes

  val reqTypeMnemonic =
    grammarStr1(Grammar.reqTypeMnemonic)(_.chars, _.chars, _.length) map ReqType.Mnemonic

  val reqTypeMnemonicFixer =
    grammarFixer(Grammar.reqTypeMnemonic)(_.chars, _.chars)
      .xmap(ReqType.Mnemonic.apply)(_.value)
      .addhs(StaticReqType.mnemonics)

  val customReqTypeId =
    id map CustomReqTypeId

  val staticReqType: Gen[StaticReqType] =
    Gen.chooseNE(StaticReqType.values)

  val reqTypeId: Gen[ReqTypeId] =
    Gen.chooseGen(staticReqType, customReqTypeId)

  def customReqTypeName =
    shortText1

  val customReqType =
    for {
      id <- customReqTypeId
      n  <- customReqTypeName
      mn <- reqTypeMnemonic
      om <- reqTypeMnemonic.set(0 to 10)
      ir <- implicationRequired
      a  <- live
    } yield CustomReqType(id, mn, om - mn, n, ir, a)

  def genCustomReqTypes(g: Gen[List[CustomReqType]]) = {
    def dname = Distinct.str.at(CustomReqType.name)
    def dmnemonic = {
      val distm = reqTypeMnemonicFixer.distinct
      val cur = distm.at(CustomReqType.mnemonic(false))
      val old = distm.lift[Set].at(CustomReqType.oldMnemonics)
      cur + old
    }
    val d = (dname * dmnemonic).lift[List]
    revAndIMap(g map d.run)
  }

  val customReqTypes =
    genCustomReqTypes(customReqType.list)

  val staticReqTypeIdSet = StaticReqType.values.toNES[ReqTypeId]

  lazy val issueCategory: Gen[IssueCategory] =
    Gen.chooseNE(IssueCategory.values)

  // -------------------------------------------------------------------------------------------------------------------
  // Tags

  val tagGroupId =
    id map TagGroupId

  val applicableTagId =
    id map ApplicableTagId

  val tagId: Gen[TagId] =
    Gen.chooseGen(tagGroupId, applicableTagId)

  val mutexChildren =
    Gen.choose[MutexChildren](MutexChildren, MutexChildren.Not)

  def tagName =
    shortText1

  val tagGroup =
    Gen.apply5(TagGroup.apply)(tagGroupId, tagName, optionalLargeText, mutexChildren, live)

  val applicableTag =
    Gen.apply5(ApplicableTag.apply)(applicableTagId, tagName, optionalLargeText, hashRefKey, live)

  val tag =
    Gen.chooseGen[Tag](tagGroup, applicableTag, applicableTag, applicableTag)

  val tagAndRels: Gen[(Tag, TagInTree.Relations)] =
    for {
      t      ← tag
      (p, c) ← tagId.set.pair
    } yield {
      val children = (c - t.id -- p).toVector
      val parents  = (p - t.id -- c).toStream.map(_ -> none[TagId]).toMap
      (t, MMTree.Relations(parents, children))
    }

  /** HashRefKey uniqueness enforced in Project, not here */
  val tags: Gen[List[Tag]] = {
    val di = distinctId[Tag, TagId]
    val dn = Distinct.str.at(Tag.name)
    val d = (di * dn).lift[List]
    tag.list map d.run
  }

  type TagTreeStructure = Map[TagId, Vector[TagId]]

  def tagTreeStructure(tags: Set[TagId]): Gen[TagTreeStructure] =
    if (tags.isEmpty)
      Gen.pure(Map.empty)
    else {
      val idset = Gen.subset(tags)
      idset.map(_.toStream)
        .flatMap(ks => Gen sequence ks.map(k => idset.map(ids => (k, (ids - k).toVector))))
        .map(s => preventCycles(Tag.CycleDetectors.multimap)(s.toMap))
    }

  val tagTree: Gen[TagTree] =
    for {
      l ← tags
      m = Tag.IdAccess.mapById(l)
      s ← tagTreeStructure(m.keySet)
    } yield
      m.values.foldLeft(TagTree.empty)((q, t) =>
        q.add(TagInTree(t, s.getOrElse(t.id, Vector.empty))))

  def setTagKey(tt: Tag, kk: Option[HashRefKey]): Tag = tt match {
    case t: ApplicableTag => kk.fold(t)(k => t.copy(key = k))
    case t: TagGroup      => t
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Fields

  val staticField: Gen[StaticField] =
    Gen.chooseNE(StaticField.values)

  def applicableReqTypes(r: Set[CustomReqTypeId]): Gen[ApplicableReqTypes] = {
    val all = StaticReqType.values.whole ++ r
    val nes = Gen.subset1(all).map(NonEmptySet force _.toSet)
    genISubset(nes)
  }

  val customFieldTextId =
    id map CustomField.Text.Id

  val customFieldTagId =
    id map CustomField.Tag.Id

  val customFieldImplicationId =
    id map CustomField.Implication.Id

  val customFieldId: Gen[CustomFieldId] =
    Gen.chooseGen(customFieldTextId, customFieldTagId, customFieldImplicationId)

  val fieldRefKey =
    grammarStr1(Grammar.fieldRefKey)(_.firstChar, _.tailChars, _.length) map FieldRefKey

  def customFieldType =
    Gen.chooseNE(CustomFieldType.values)

  def customFieldText(art: Gen[ApplicableReqTypes]): Gen[CustomField.Text] =
    Gen.apply6(CustomField.Text.apply)(customFieldTextId, shortText1, fieldRefKey, mandatory, art, live)

  def customFieldTag(tagId: Gen[TagId], art: Gen[ApplicableReqTypes]): Gen[CustomField.Tag] =
    Gen.apply5(CustomField.Tag.apply)(customFieldTagId, tagId, mandatory, art, live)

  def customFieldTagSome(tagIds: Set[TagId], art: Gen[ApplicableReqTypes]): Gen[Vector[CustomField.Tag]] =
    Gen.subset(tagIds.toVector).flatMap(ids =>
      Gen sequence ids.map(id =>
        customFieldTag(Gen pure id, art)))

  def customFieldImplication(reqTypeId: Gen[ReqTypeId], art: Gen[ApplicableReqTypes]): Gen[CustomField.Implication] =
    Gen.apply5(CustomField.Implication.apply)(customFieldImplicationId, reqTypeId, mandatory, art, live)

  def customFieldImplicationSome(reqTypeIds: Set[ReqTypeId], art: Gen[ApplicableReqTypes]): Gen[Vector[CustomField.Implication]] =
    Gen.subset(reqTypeIds.toVector).flatMap(ids =>
      Gen sequence ids.map(id =>
        customFieldImplication(Gen pure id, art)))

  def customField(art: Gen[ApplicableReqTypes],
                  impFields: Boolean,
                  tagFields: Boolean): Gen[CustomField] = {
    lazy val txt: Gen[CustomField] = customFieldText(art)
    customFieldType.flatMap {
      case CustomFieldType.Text        => txt
      case CustomFieldType.Tag         => if (tagFields) customFieldTag(tagId, art) else txt
      case CustomFieldType.Implication => if (impFields) customFieldImplication(reqTypeId, art) else txt
    }
  }

  def customFields(reqTypeIds: Set[ReqTypeId], tagIds: Set[TagId], art: Gen[ApplicableReqTypes]): Gen[IMap[CustomFieldId, CustomField]] = {
    val cf = for {
      f1 <- customField(art, false, false).list
      f2 <- customFieldTagSome(tagIds, art)
      f3 <- customFieldImplicationSome(reqTypeIds, art)
    } yield f3.toList ::: f2.toList ::: f1
    def id   = distinctId(CustomField.IdAccess, CustomFieldId_T)
    def name = Distinct.str.at(CustomField.independentName)
    def key  = Distinct.fstr.xmap(FieldRefKey.apply)(_.value).distinct.at(CustomField.key)
    val dist = (id * name * key).lift[Stream]
    cf.map(fs => emptyDataMap(CustomField) ++ dist.run(fs.toStream))
  }

  def fieldSet(reqTypeIds: Set[ReqTypeId], tagIds: Set[TagId], r: Set[CustomReqTypeId]): Gen[FieldSet] =
    for {
      cf           ← customFields(reqTypeIds, tagIds, applicableReqTypes(r))
      mandatoryIds = cf.keySet.map(f => f: FieldId) ++ StaticField.notDeletable
      optionalIds  ← Gen.chooseIndexed_!(StaticField.deletable).set
      order        ← Gen.shuffle((mandatoryIds ++ optionalIds).toVector)
    } yield FieldSet(cf, order)

  def fieldSet2(reqTypeIds: Set[ReqTypeId], tagIds: Set[TagId], cReqTypeIds: Set[CustomReqTypeId]): Gen[FieldSet] =
    Gen.chooseSize flatMap { sz =>
      def subset[A](as: Set[A]): Gen[Set[A]] =
        Gen.subset(as).map(_ take sz)
      for {
        r <- subset(reqTypeIds)
        t <- subset(tagIds)
        c <- subset(cReqTypeIds)
        x <- fieldSet(r, t, c)
      } yield x
    }

  // -------------------------------------------------------------------------------------------------------------------
  // Text

  object TextGen {
    import scalaz.Name
    import shipreq.webapp.base.text.{MultiLine => _, SingleLine => _, _}
    import Atom._
    import Text.{ReqTitle => _, _}

    private val asciiSL     = (32.toChar to 127.toChar).toArray
    private val asciiML     = ('\n' :: '\r' :: asciiSL.toList).toArray
    private val highChars   = Gen.chooseInt(128, 255).map(_.toChar) // Gen.unicode // TODO Disabled due to PhantomJS-2.1.1-8 crashing
            val genCharSL   = Gen.chooseGen(Gen chooseArray_! asciiSL, highChars)
            val genCharML   = Gen.chooseGen(Gen chooseArray_! asciiML, highChars)
    private val literalStr  = genCharSL                       .string(1 to 100)
    private val texStr      = genCharSL                       .string(1 to  20)
    private val webAddressR = charPred(Parsers.webAddressChar).string(1 to  40)
    private val emailL      = charPred(Parsers.emailCharL)    .string(1 to  20)
    private val emailR      = charPred(Parsers.emailCharR)    .string(1 to  14)

    def literal(implicit t: Literal): Gen[t.Literal] =
      literalStr.map(t.Literal)

    def blankLine(implicit t: NewLine): Gen[t.BlankLine] =
      Gen.pure(t.blankLine)

    def listItem(t: ListMarkup)(g: Name[Gen[t.Atom]]): Gen[t.ListItem] =
      Gen.pure(g).flatMap(_.value).vector(MaxTextAtoms)

    def listItems(t: ListMarkup)(g: Name[Gen[t.Atom]]): Gen[NonEmptyVector[t.ListItem]] =
      listItem(t)(g).nev(0 to 10)

    def unorderedList(t: ListMarkup)(g: Name[Gen[t.Atom]]): Gen[t.UnorderedList] =
      listItems(t)(g) map t.UnorderedList

    def webAddress(implicit t: PlainTextMarkup): Gen[t.WebAddress] =
      for {
        a <- Gen.choose("http", "https", "ftp", "ftps", "sftp")
        b <- webAddressR
      } yield t.WebAddress(a + "://" + b)

    def emailAddress(implicit t: PlainTextMarkup): Gen[t.EmailAddress] =
      for {
        l <- emailL
        r <- emailR.list(2 to 5)
      } yield t.EmailAddress(l + "@" + r.mkString("."))

    def tex(implicit t: PlainTextMarkup): Gen[t.TeX] =
      texStr.map(_.replace(s"</${Grammar.texTag}>", "x") |> noWhitespaceLeft |> noWhitespaceRight |> t.TeX)

    def plainTextMarkup(implicit t: PlainTextMarkup): Gen[t.Atom] =
      Gen.chooseGen(webAddress, emailAddress, tex)

    private[this] def singleLineGens(implicit t: SingleLine): NonEmptyVector[Gen[t.Atom]] =
      NonEmptyVector(literal, plainTextMarkup)

    /** Probability [0,9] of an increase in recursive depth. */
    val DepthIncrease: Array[Int] = Array(5, 1, 1, 1) `JVM|JS` Array(3, 1)

    private[this] def multiLine(t: MultiLine, depth: Int)(g: Name[Gen[t.Atom]]): NonEmptyVector[Gen.Freq[t.Atom]] = {
      var gs = singleLineGens(t).map(g => (9, g))
      gs :+= ((9, blankLine(t)))
      if (depth < DepthIncrease.length)
        gs :+= ((DepthIncrease(depth), unorderedList(t)(g)))
      gs
    }

    private[this] def multiLinePlusI(t: MultiLine)(plus: Gen.Freq[t.Atom]*): Gen[t.Atom] = {
      type G  = Gen[t.Atom]
      type IG = Gen.Freq[G]

      lazy val lvls: Vector[Need[G]] =
        (0 to DepthIncrease.length)
          .toVector
          .map(i => Need[G](Gen.frequencyNE(
            multiLine(t, i)(Name(lvls(i + 1).value)) ++ plus
        )))

      lvls.head.value
    }

    private[this] def multiLinePlus(t: MultiLine)(plus: Option[Gen[t.Atom]]*): Gen[t.Atom] =
      multiLinePlusI(t)(
        plus.foldLeft[List[Gen.Freq[t.Atom]]](Nil)((q, o) =>
          o.fold(q)(g => (9, g) :: q)): _*)

    def reqRefs(r: Option[Gen[ReqId]], c: Option[Gen[ReqCodeId]])(implicit t: ContentRef): List[Gen[t.Atom]] = {
      var result = List.empty[Gen[t.Atom]]
      r.foreach(result ::= _ map t.ReqRef)
      c.foreach(result ::= _ map t.CodeRef)
      result
    }

    def useCaseStepRef(u: Gen[UseCaseStepId])(implicit t: ContentRef): Gen[t.Atom] =
      u map t.UseCaseStepRef

    def tagRef(g: Gen[ApplicableTagId])(implicit t: TagRef): Gen[t.TagRef] =
      g map t.TagRef

    def issue(i: Gen[CustomIssueTypeId],
              r: Option[Gen[ReqId]],
              u: Option[Gen[UseCaseStepId]],
              c: Option[Gen[ReqCodeId]])(implicit t: Issue): Gen[t.Issue] =
      Gen.apply2(t.Issue)(i, inlineIssueDescAtom(r, u, c).vector)

    val isBlankLine: AnyAtom => Boolean = {
      case _: NewLine#BlankLine => true
      case _ => false
    }

    def trimBlankLines[T <: Atom.Base](as: Vector[T#Atom]): Vector[T#Atom] =
      dropLast(dropHead(as)(isBlankLine))(isBlankLine)

    val legalListItemAtom: AnyAtom => Boolean = {
      case _: Literal         # Literal
         | _: ContentRef      # ReqRef
         | _: ContentRef      # CodeRef
         | _: ContentRef      # UseCaseStepRef
         | _: Issue           # Issue
         | _: PlainTextMarkup # WebAddress
         | _: PlainTextMarkup # EmailAddress
         | _: PlainTextMarkup # TeX
         | _: TagRef          # TagRef
         | _: NewLine         # BlankLine     => true
      case _: ListMarkup      # UnorderedList => false
    }

    sealed trait AtomCtx
    case object InIssueDesc  extends AtomCtx
    case object InListItem   extends AtomCtx
    case object TopLevelAtom extends AtomCtx

    def noWhitespaceLeft(a: String) = "l" + a
    def noWhitespaceRight(a: String) = a + "r"

    val removeFromLiteralsR = {
      val reqOrStepRefInside = """(?:[a-zA-Z]+\s*(?:-\s*)?(?:\d+|X)(?:\s*\.\s*(?:\d+|X))*)"""
      val codeNode = """(?:[a-zA-Z0-9][a-zA-Z0-9_]*)"""
      val codeRefInside = s"(?:$codeNode(?:\\.$codeNode)*)"
      (s"<${Grammar.texTag}>" + """|[#@]+|[a-z]://|\*( )|\[\s*(?:""" + s"$reqOrStepRefInside|$codeRefInside)\\s*\\]").r
    }
    def removeFromLiterals[L <: Literal#Literal](l: L): L =
      l.map(removeFromLiteralsR.replaceAllIn(_, "*$1"))

    def postProcessAtoms[T <: Atom.Base](ctx: AtomCtx)(as0: Vector[T#Atom]): Vector[T#Atom] = {
      type Blank = NewLine#BlankLine
      type UL    = ListMarkup#UnorderedList
      type Lit   = Literal#Literal

      def fixHead: T#Atom => T#Atom = {
        case l: Lit => l map noWhitespaceLeft
        case o => o
      }

      def fixLast: T#Atom => T#Atom = {
        case l: Lit => l map noWhitespaceRight
        case o => o
      }

      def fix2: T#Atom => T#Atom = {
        case l: Lit => removeFromLiterals(l)
        case o => o
      }

      // Trim multiline
      val as = {
        val v = trimBlankLines(as0)
        if (v.isEmpty) v else v.init :+ fixLast(v.last)
      }

      as.foldLeft(Vector.empty[T#Atom])((q, a0) => {
        import Atom.{PlainTextMarkup => PTM}

        val a: T#Atom = a0 match {
          case l: Lit if ctx == InIssueDesc => l.map(_.replace('}', 'x'))
          case i: Issue#Issue               => i.copy(desc = postProcessAtoms(InIssueDesc)(i.desc))
          case ul: UL                       => ul.filterAtoms(legalListItemAtom).map(postProcessAtoms(InListItem))
          case o => o
        }

        def i = q.init
        def drop = q
        if (q.isEmpty)
          q :+ fixHead(a)
        else
          (q.last, a) match {

          case (x: Lit             , y: Lit             ) => i :+ x.map(_ + y.value)
          case (x: Lit             , y: PTM#EmailAddress) => i :+ x.map(_ + " ") :+ y
          case (x: Lit             , y: PTM#WebAddress  ) => i :+ x.map(_ + " ") :+ y
        //case (x: Lit             , y: Issue#Issue     ) => i :+ x.map(_ + " ") :+ y
        //case (x: Lit             , y: TagRef#Tagref   ) => i :+ x.map(_ + " ") :+ y
          case (x: Lit             , y: Blank           ) => i :+ x.map(noWhitespaceRight) :+ y
          case (x: Lit             , y: UL              ) => i :+ x.map(noWhitespaceRight) :+ y

          case (x: Blank           , y: Lit             ) => i :+ x :+ y.map(noWhitespaceLeft)
          case (_: Blank           , _: Blank           ) => drop
          case (_: Blank           , _: UL              ) => drop

          case (x: PTM#EmailAddress, y: Lit             ) => i :+ x :+ y.map(" " + _)
          case (_: PTM#EmailAddress, _: PTM#EmailAddress) => drop
          case (_: PTM#EmailAddress, _: PTM#WebAddress  ) => drop

          case (x: PTM#WebAddress  , y: Lit             ) => i :+ x :+ y.map(" " + _)
          case (_: PTM#WebAddress  , _: PTM#EmailAddress) => drop
          case (_: PTM#WebAddress  , _: PTM#WebAddress  ) => drop
          case (_: PTM#WebAddress  , _: Issue#Issue     ) => drop
          case (_: PTM#WebAddress  , _: TagRef#TagRef   ) => drop

          case (x: TagRef#TagRef   , y: Lit             ) => i :+ x :+ y.map(" " + _)
          case (_: TagRef#TagRef   , _: PTM#EmailAddress) => drop
          case (_: TagRef#TagRef   , _: PTM#WebAddress  ) => drop

          case (x: Issue#Issue     , y: Lit             ) if x.desc.isEmpty => i :+ x :+ y.map(" i" + _)
          case (x: Issue#Issue     , _: PTM#EmailAddress) if x.desc.isEmpty => drop
          case (x: Issue#Issue     , _: PTM#WebAddress  ) if x.desc.isEmpty => drop

          case (x: UL              , y: Lit             ) => i :+ x :+ y.map(noWhitespaceLeft)
          case (_: UL              , _: Blank           ) => drop
          case (_: UL              , _: UL              ) => drop //.copy(items = x.items ++ y.items)

          case _ => q :+ a
        }
      })
      .map(fix2) |> trimBlankLines
    }

    def postProcessAtoms1[T <: Atom.Literal](t: T)(as: NonEmptyVector[T#Atom]): NonEmptyVector[T#Atom] = {
      val r = postProcessAtoms(TopLevelAtom)(as.whole)
      NonEmptyVector.maybe(r, NonEmptyVector[T#Atom](t.Literal("a")))(identity)
    }

    // Specific text types

    def reqTitle(t: ReqTitle)(r: Option[Gen[ReqId]],
                              u: Option[Gen[UseCaseStepId]],
                              c: Option[Gen[ReqCodeId]],
                              i: Option[Gen[CustomIssueTypeId]],
                              a: Option[Gen[ApplicableTagId]]): Gen[t.Atom] = {
      @inline implicit def tt: t.type = t
      val gs = NonEmptyVector newBuilderNE singleLineGens(t)
      gs ++= reqRefs(r, c)
      gs ++= u.map(useCaseStepRef(_))
      gs ++= i.map(issue(_, r, u, c))
      gs ++= a.map(tagRef(_))
      Gen.chooseGenNE(gs.result())
    }

    def codeGroupTitleAtom(r: Option[Gen[ReqId]],
                              u: Option[Gen[UseCaseStepId]],
                              c: Option[Gen[ReqCodeId]],
                              i: Option[Gen[CustomIssueTypeId]]): Gen[CodeGroupTitle.Atom] = {
      @inline implicit def t: CodeGroupTitle.type = CodeGroupTitle
      val gs = NonEmptyVector newBuilderNE singleLineGens(t)
      gs ++= reqRefs(r, c)
      gs ++= u.map(useCaseStepRef(_))
      gs ++= i.map(issue(_, r, u, c))
      Gen.chooseGenNE(gs.result())
    }

    def genericReqTitleAtom = reqTitle(GenericReqTitle) _

    def useCaseTitleAtom = reqTitle(UseCaseTitle) _

    def inlineIssueDescAtom(r: Option[Gen[ReqId]],
                            u: Option[Gen[UseCaseStepId]],
                            c: Option[Gen[ReqCodeId]]): Gen[InlineIssueDesc.Atom] = {
      @inline implicit def t: InlineIssueDesc.type = InlineIssueDesc
      val gs = NonEmptyVector newBuilderNE singleLineGens(t)
      gs ++= reqRefs(r, c)
      gs ++= u.map(useCaseStepRef(_))
      Gen.chooseGenNE(gs.result())
    }

    def customTextFieldAtom(r: Option[Gen[ReqId]],
                            u: Option[Gen[UseCaseStepId]],
                            c: Option[Gen[ReqCodeId]],
                            i: Option[Gen[CustomIssueTypeId]],
                            a: Option[Gen[ApplicableTagId]]): Gen[CustomTextField.Atom] = {
      implicit val t: CustomTextField.type = CustomTextField
      var gs: List[Option[Gen[t.Atom]]]
           = reqRefs(r, c).map(_.some)
      gs ::= u.map(useCaseStepRef(_))
      gs ::= i.map(issue(_, r, u, c))
      gs ::= a.map(tagRef(_))
      multiLinePlus(t)(gs: _*)
    }

    def deletionReasonAtom(r: Option[Gen[ReqId]],
                           u: Option[Gen[UseCaseStepId]],
                           c: Option[Gen[ReqCodeId]],
                           a: Option[Gen[ApplicableTagId]]): Gen[DeletionReason.Atom] = {
      implicit val t: DeletionReason.type = DeletionReason
      var gs: List[Option[Gen[t.Atom]]]
           = reqRefs(r, c).map(_.some)
      gs ::= u.map(useCaseStepRef(_))
      gs ::= a.map(tagRef(_))
      multiLinePlus(t)(gs: _*)
    }

    def manualIssueAtom(r: Option[Gen[ReqId]],
                        u: Option[Gen[UseCaseStepId]],
                        c: Option[Gen[ReqCodeId]],
                        a: Option[Gen[ApplicableTagId]]): Gen[ManualIssue.Atom] = {
      implicit val t: ManualIssue.type = ManualIssue
      var gs: List[Option[Gen[t.Atom]]]
           = reqRefs(r, c).map(_.some)
      gs ::= u.map(useCaseStepRef(_))
      gs ::= a.map(tagRef(_))
      multiLinePlus(t)(gs: _*)
    }

    def useCaseStepAtom(r: Option[Gen[ReqId]],
                        u: Option[Gen[UseCaseStepId]],
                        c: Option[Gen[ReqCodeId]],
                        i: Option[Gen[CustomIssueTypeId]],
                        a: Option[Gen[ApplicableTagId]]): Gen[UseCaseStep.Atom] = {
      @inline implicit def t: UseCaseStep.type = UseCaseStep
      val gs = NonEmptyVector newBuilderNE singleLineGens(t)
      gs ++= reqRefs(r, c)
      gs ++= u.map(useCaseStepRef(_))
      gs ++= i.map(issue(_, r, u, c))
      gs ++= a.map(tagRef(_))
      Gen.chooseGenNE(gs.result())
    }
  }

  val MaxTextAtoms: SizeSpec = 0 to (15 `JVM|JS` 5)

  val MaxTextAtomsInProject: SizeSpec = 0 to (6 `JVM|JS` 2)

  implicit def TextGenExt[T <: text.Atom.Literal](g: Gen[T#Atom]) = new TextGenExt[T](g.run)
  class TextGenExt[T <: text.Atom.Literal](private val _g: Gen.Run[T#Atom]) extends AnyVal {
    private def g = Gen(_g)

    def text       : Gen[T#OptionalText] = g.vector(MaxTextAtoms) map TextGen.postProcessAtoms(TextGen.TopLevelAtom)
    def text1(t: T): Gen[T#NonEmptyText] = g.nev   (MaxTextAtoms) map TextGen.postProcessAtoms1(t)

    def ptext       : Gen[T#OptionalText] = g.vector(MaxTextAtomsInProject) map TextGen.postProcessAtoms(TextGen.TopLevelAtom)
    def ptext1(t: T): Gen[T#NonEmptyText] = g.nev   (MaxTextAtomsInProject) map TextGen.postProcessAtoms1(t)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Requirements

  val genericReqId  = id map GenericReqId
  val useCaseId     = id map UseCaseId
  val useCaseStepId = id map UseCaseStepId

  val reqId: Gen[ReqId] =
    Gen.chooseGen(genericReqId, genericReqId, useCaseId)

  def useCaseSteps(g: Gen[UseCaseStep], f: StaticField.UseCaseStepTree)(implicit ss: SizeSpec): Gen[UseCaseSteps] = {
    val gt = genVectorTree(g, f.maxDepth)
    val gt2 =
      f match {
        case StaticField.NormalAltStepTree =>
          // Root step is required
          Gen { ctx =>
            var t = gt run ctx
            if (t.isEmpty)
              t = VectorTree.single(g run ctx)
            if (t.children.head.value.liveExplicitly is Dead)
              t.modifyValueAt(VectorTree.root)(UseCaseStep.liveExplicitly set Live) foreach (t = _)
            t
          }
        case _ => gt
      }
    gt2 map UseCaseSteps.apply
  }

  class PubidRegisterBuilder {
    private[PubidRegisterBuilder] var pr = PubidRegister.empty

    def apply[A](f: PubidRegister => (PubidRegister, A)): A = {
      val r = f(pr)
      pr = r._1
      r._2
    }

    def result() = pr
  }

  object PubidRegisterBuilder {
    def apply(): PubidRegisterBuilder =
      new PubidRegisterBuilder

    def apply(init: PubidRegister): PubidRegisterBuilder = {
      val b = apply()
      b.pr = init
      b
    }
  }

  case class PRAndIds(grIds: Vector[GenericReqId], ucIds: Vector[UseCaseId], pr: PubidRegister)

  def pubidRegisterAndIds(rtIds: Vector[CustomReqTypeId], genericReqCount: Int, useCaseCount: Int): Gen[PRAndIds] = {
    val ucIdsG = useCaseId.vector(useCaseCount)

    Gen { ctx =>
      val pr = PubidRegisterBuilder()

      val grs: Vector[GenericReqId] =
        NonEmptyVector.option(rtIds) match {

          case Some(rtIdNev) if genericReqCount > 0 =>
            val ids = genericReqId.unique_!.vector(genericReqCount) run ctx
            val rtG = Gen.chooseNE(rtIdNev)
            for (id <- ids) {
              val rt = rtG run ctx
              pr(_.allocC(rt)(id))
            }
            ids

          case _ => Vector.empty
        }

      val ucs: Vector[UseCaseId] = {
        val idFixer = Distinct.fint.addhs(grs.iterator.map(_.value)).xmap(UseCaseId)(_.value).distinct.lift[Vector]
        val ids     = idFixer run ucIdsG.run(ctx)
        for (id <- ids)
          pr(_.allocUC(id))
        ids
      }

      PRAndIds(grs, ucs, pr.result())
    }
  }

  def reqsWithoutText(cfg: ProjectConfig, genericReqCount: Int, useCaseCount: Int): Gen[Requirements] =
    reqsWithoutText(cfg.reqTypes.custom.keysIterator.toVector, genericReqCount, useCaseCount)

  def reqsWithoutText(rtIds: Vector[CustomReqTypeId], genericReqCount: Int, useCaseCount: Int): Gen[Requirements] = {
    Gen { ctx =>
      val prAndIds = pubidRegisterAndIds(rtIds, genericReqCount, useCaseCount) run ctx
      val pr = prAndIds.pr

      val grs: GenericReqIMap =
        rtIds.foldLeft(emptyDataMap(GenericReq))((m0, rt) =>
          pr.value(rt).iterator.zipWithIndex.foldLeft(m0) { (m, x) =>
            val id    = x._1.asInstanceOf[GenericReqId]
            val pubid = PubidT(rt, ReqTypePos(x._2 + 1))
            val grG   = live map (GenericReq(id, pubid, Vector.empty, _))
            m + grG.run(ctx)
          }
        )

      val ucs: UseCaseIMap = {
        val uniqueIds = useCaseStepId.unique_!

        def stepGL(l: Live) =
          uniqueIds.map(UseCaseStep(_, Vector.empty, l))

        val stepG = live flatMap stepGL

        def stepsG(f: StaticField.UseCaseStepTree) =
          useCaseSteps(stepG, f)(0 to 4)

        pr.value(StaticReqType.UseCase).iterator.zipWithIndex.foldLeft(emptyDataMap(UseCase)) { (m, x) =>
          val id = x._1.asInstanceOf[UseCaseId]
          val pos = ReqTypePos(x._2 + 1)
          val ucG = for {
            stepsNA <- stepsG(StaticField.NormalAltStepTree)
            stepsE  <- stepsG(StaticField.ExceptionStepTree)
            l       <- live
          } yield
            // Root existence guaranteed in useCaseSteps()
            UseCase(id, pos, Vector.empty, stepsNA, stepsE, l)
          m + ucG.run(ctx)
        }
      }

      val ucStepIds: Vector[UseCaseStepId] =
        ucs.valuesIterator.flatMap(_.stepIterator).map(_.id).toVector

      val stepFlow: UseCases.StepFlow =
        genDigraphBiO(Gen.tryGenChoose(ucStepIds))(implicitly, 0 to 4) run ctx

      Requirements(grs, UseCases.Stateless(ucs, stepFlow).withState, pr)
    }
  }

  def setReqText(reqs: Requirements,
                 u   : Option[Gen[UseCaseStepId]],
                 c   : Option[Gen[ReqCodeId]],
                 i   : Option[Gen[CustomIssueTypeId]],
                 a   : Option[Gen[ApplicableTagId]]): Gen[Requirements] = {
    val r = Gen.tryGenChoose(reqs.idIterator.toIndexedSeq)
    setReqText(reqs, r, u, c, i, a)
  }

  def setReqText(reqs: Requirements,
                 r   : Option[Gen[ReqId]],
                 u   : Option[Gen[UseCaseStepId]],
                 c   : Option[Gen[ReqCodeId]],
                 i   : Option[Gen[CustomIssueTypeId]],
                 a   : Option[Gen[ApplicableTagId]]): Gen[Requirements] =
    r match {
      case Some(g) =>
        setReqText(reqs,
          TextGen.genericReqTitleAtom(Some(g), u, c, i, a).text,
          TextGen.useCaseTitleAtom   (Some(g), u, c, i, a).text,
          TextGen.useCaseStepAtom    (Some(g), u, c, i, a).text)
      case None =>
        Gen pure reqs
    }

  def setReqText(reqs: Requirements,
                 grG : Gen[Text.GenericReqTitle.OptionalText],
                 ucG : Gen[Text.UseCaseTitle.OptionalText],
                 usG : Gen[Text.UseCaseStep.OptionalText]): Gen[Requirements] = {

    val updateGRs: Requirements => Gen[Requirements] =
      TestOptics.genericReqTitlesInReqs.setF(grG)

    val updateUCs: Requirements => Gen[Requirements] =
      TestOptics.useCasesInReqs.modifyF(
        useCaseStepTextsInUseCase.setF(usG)(_) flatMap UseCase.title.setF(ucG))

    updateGRs(reqs) flatMap updateUCs
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Req Data

  def reqFieldDataText(cols: Set[CustomField.Text.Id], reqs: Set[ReqId], txt: Gen[Text.CustomTextField.NonEmptyText]): Gen[ReqData.Text] =
    txt mapByKeySubset reqs mapByKeySubset cols

  private[this] val emptyATagIdSet = Gen.pure(Set.empty[ApplicableTagId])
  def reqFieldDataTags(reqs: Traversable[ReqId], tags: Set[ApplicableTagId]): Gen[ReqData.Tags] = {
    val rndTags = Gen.chooseGen(Gen.subset(tags), emptyATagIdSet)
    (rndTags mapByKeySubset reqs.toIterable).map(Multimap(_))
//    subset2(reqs, 1, 0).flatMap(rndTags.mapByEachKey).map(Multimap(_))
  }

  type ImplicationsUM = Map[ReqId, Set[ReqId]]
  @tailrec def preventImplicationCycles(m: ImplicationsUM): ImplicationsUM =
    Implications.cycleDetector.findCycle(m) match {
      case None         => m
      case Some((_, b)) => preventImplicationCycles(m - b)
    }

  val MaxImplicationPairs: SizeSpec = 0 to (100 `JVM|JS` 40)
  // val MaxImplicationsPerSrc = 2  `JVM|JS` 4
  // val MaxImplicationKeys    = 10 `JVM|JS` 4

  type ImpMethod = (Gen[ReqId], ImplicationsUM => Implications) => Gen[Implications]

  val implicationsMethodDefault: ImpMethod = (g, fix) =>
    g.pair
      .list(MaxImplicationPairs)
      .map(kvs => Implications.emptyUniDir.addPairs(kvs: _*).m |> fix)

  def implicationsMethod2(maxImpsPerSrc: Int, maxImpKeys: Int): ImpMethod = (g, fix) =>
    Gen.tuple2(g, g.set1(1 to maxImpsPerSrc))
      .list(0 to maxImpKeys)
      .map(_.toMap |> fix)

  def reqFieldDataImplications(reqIds: Set[ReqId], method: ImpMethod = implicationsMethodDefault): Gen[Implications] = {
    def fix(m: ImplicationsUM): Implications = {
      val m2 = preventImplicationCycles(m)
      // println(m2); println()
      Implications.BiDir(Multimap(m2))
    }

    Gen.tryGenChoose(reqIds.toSeq) match {
      case Some(g) => method(g, fix)
      case None    => Gen pure Implications.emptyBiDir
    }
  }

//  def customTextFieldText(reqIdG  : Option[Gen[ReqId]],
//                          reqCodeG: Option[Gen[ReqCodeId]],
//                          cissueG : Option[Gen[CustomIssueTypeId]],
//                          tagG    : Option[Gen[ApplicableTagId]]) = {
//    TextGen.customTextFieldAtom(reqIdG, reqCodeG, cissueG, tagG).ptext1(Text.CustomTextField)
//  }

  def reqFieldDataText2(reqs   : Set[ReqId],
                        txtCols: Set[CustomField.Text.Id],
                        u      : Option[Gen[UseCaseStepId]],
                        c      : Option[Gen[ReqCodeId]],
                        i      : Option[Gen[CustomIssueTypeId]],
                        a      : Option[Gen[ApplicableTagId]]) = {
    val gr = Gen.tryGenChoose(reqs.toSeq)
    reqFieldDataText(txtCols, reqs, TextGen.customTextFieldAtom(gr, u, c, i, a).ptext1(Text.CustomTextField))
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Req Codes

  object reqCode {
    import ReqCode._

    val node: Gen[Node] =
      grammarStr1(Grammar.reqCode)(_.firstChar, _.tailChars, _.nodeLength) map Node.applyFn

    val value: Gen[Value] =
      node.nev(1 to Grammar.reqCode.maxNodes)

    val apId =
      RandomData.id map ApReqCodeId.apply

    val groupId =
      RandomData.id map ReqCodeGroupId

    val id: Gen[ReqCodeId] =
      Gen.chooseGen(apId, groupId)

    val distinctIds =
      Distinct.fint.xmap(ApReqCodeId.apply)(_.value).distinct

    val distinctReqCodeTrie = {
      val reqIdIso: Iso[ReqCodeGroupId, ApReqCodeId] =
        Iso[ReqCodeGroupId, ApReqCodeId](a => ApReqCodeId(a.value))(a => ReqCodeGroupId(a.value))

      val reqCodeDataActiveId = Optional[Data, ApReqCodeId]({
        case d: ActiveReq   => Some(d.id)
        case d: ActiveGroup => Some(ApReqCodeId(d.id.value))
        case _: Inactive    => None
      })(n => {
        case d: ActiveReq   => d.copy(id = n)
        case d: ActiveGroup => reqCodeActiveGroupId.set(ReqCodeGroupId(n.value))(d)
        case d: Inactive    => d
      })


      val ids1      = distinctIds at reqCodeDataActiveId
      val ids2      = distinctIds at reqCodeDataDeadGroupId.composeIso(reqIdIso)
      val ids3      = distinctIds.lift[Set].liftMultimapValues[ReqId, Set, ApReqCodeId, ApReqCodeId] at reqCodeDataReqInactive
      val id        = ids1 + ids2 + ids3
      val idsInTrie = id traversal reqCodeTrieValueTraversal
      idsInTrie
    }

    val smallApIdSet = apId.set(0 to 3)

    val gEmptyReqInactive: Gen[ReqInactive] =
      Gen pure emptyReqInactive

    private val gEmptyText: Gen[Text.CodeGroupTitle.OptionalText] =
      Gen pure Vector.empty

    def data(ogLiveReqId: Option[Gen[ReqId]], ogReqId: Option[Gen[ReqId]],
             gGroupText: Gen[Text.CodeGroupTitle.OptionalText] = gEmptyText)(implicit ss: SizeSpec): Gen[Data] =

      ss.gen flatMap { sz =>
        val gReqInactive: Gen[ReqInactive] =
          ogReqId match {
            case Some(g) => g.mapTo(smallApIdSet)(0 to sz).map(Multimap(_))
            case None    => gEmptyReqInactive
          }

        val gLiveCodeGroup: Gen[LiveCodeGroup] =
          Gen.apply2(LiveCodeGroup.apply)(groupId, gGroupText)

        val gDeadCodeGroup: Gen[DeadCodeGroup] =
          Gen.apply2(DeadCodeGroup.apply)(groupId, gGroupText)

        val gDeadGroup: Gen[DeadGroup] =
          gDeadCodeGroup.option

        val gInactive: Gen[Inactive] =
          Gen.apply2(Inactive.apply)(gDeadGroup, gReqInactive)
            .flatMap(i =>
              if (i.deadGroup.isEmpty && i.reqInactive.isEmpty)
                gDeadCodeGroup.map(d => Inactive(Some(d), i.reqInactive))
              else
                Gen.pure(i)
            )

        val gActiveGroup: Gen[ActiveGroup] =
          Gen.apply2(ActiveGroup.apply)(gLiveCodeGroup, gReqInactive)

        val gActiveReq: Option[Gen[ActiveReq]] =
          ogLiveReqId map (gReqId =>
            Gen.apply4(ActiveReq.apply)(apId, gReqId, gDeadGroup, gReqInactive))

        gActiveReq match {
          case None    => Gen.chooseGen(gInactive, gActiveGroup)
          case Some(g) => Gen.chooseGen(gInactive, gActiveGroup, g, g)
        }
    }

    def trieValue(d: Gen[Data]): Gen[Trie.Value] = d map Trie.Value

    def trie(d: Gen[Data], maxDepth: Int): Gen[Trie] =
      genMTrie(node, d, maxDepth) map distinctReqCodeTrie.run

    def codeSet(maxDepth: Int): Gen[CodeSet] =
      genMTrie(node, Gen.unit, maxDepth)

    def updateGroupText(gt: Gen[Text.CodeGroupTitle.OptionalText])(src: Trie): Gen[Trie] = {
      type F = EndoFn[Trie]
      type G = Gen[F]

      val vecOfGens = src.cataV(Vector.empty[G])((q, code, data) =>
        reqCodeDataGroupTitle.getOption(data) match {
          case Some(_) => q :+ gt.map[F](txt => _.put(code, reqCodeDataGroupTitle.set(txt)(data)))
          case None    => q
        }
      )

      val genVec = Gen.sequence(vecOfGens)
      genVec.map(_.foldLeft(src)((q, f) => f(q)))
    }
  }

  def reqCodes(g: Gen[ReqCode.Trie]) =
    g map ReqCodes.apply

  // -------------------------------------------------------------------------------------------------------------------
  // Project

  lazy val hashRefFixer =
    grammarFixerIgnoreCase(Grammar.hashRefKey)(_.firstChar, _.tailChars)
      .xmap(HashRefKey.apply)(_.value)

  def distinctHashRefKeys = {
    type A = CustomIssueTypeIMap
    type B = TagTree
    type T = (A, B)
    val keyDist = hashRefFixer.distinct
    val issues = keyDist
      .at(CustomIssueType.key).liftMapValues[CustomIssueTypeId]
      .at(first[T, A] ^|-> imapToMapLens)
    val tags = keyDist
      .lift[Option].contramap[Tag](_.keyO, setTagKey)
      .at(TagInTree.tag).liftMapValues[TagId]
      .at(second[T, B] ^|-> imapToMapLens)
    issues + tags
  }

  def deletionReasons(gReqId: Option[Gen[ReqId]], gText: Gen[Text.DeletionReason.NonEmptyText]): Gen[DeletionReasons] =
    //Gen insert DeletionReasons.empty/*
    gReqId match {
      case None => Gen pure DeletionReasons.empty
      case Some(g) =>
        for {
          reasons  ← gText.vector
          ids      = reasons.indices.toStream.map(i => Option(DeletionReasonId(i)))
          idToReqs ← g.set1.fill(reasons.length).map(ids zip _)
        } yield {
          var ra = DeletionReasons.emptyReqApplication
          idToReqs.foreach { t =>
            val t1h = t._1.hashCode
            t._2.foreach { reqId =>
              val h = reqId.hashCode ^ t1h
              if ((h &  3) == 0) ra = ra.add(reqId, None) // 25% chance
                                 ra = ra.add(reqId, t._1)
              if ((h & 12) == 0) ra = ra.add(reqId, None) // 25% chance
            }
          }
          DeletionReasons(reasons, ra)
        }
    }

  def genManualIssues(gText: Gen[Text.ManualIssue.NonEmptyText]): Gen[ManualIssues] =
    for {
      len <- Gen.chooseInt(0, 2)
      ts <- gText.list(len)
    } yield ts.foldLeft(ManualIssues.empty)(_ add _)

  lazy val projectConfig: Gen[ProjectConfig] =
    for {
      (issues, tags) ← Gen.tuple2(customIssueTypes, tagTree) map distinctHashRefKeys.run
      reqtypes       ← customReqTypes
      reqTypeIds     = StaticReqType.values ++ reqtypes.keys
      reqTypeIdSet   = reqTypeIds.whole.toSet
      fields         ← fieldSet(reqTypeIdSet, tags.keySet, reqtypes.keySet)
    } yield ProjectConfig(issues, ReqTypes(reqtypes), fields, Tags(tags))

  def genProject(cfg            : ProjectConfig,
                 reqsWithoutText: Requirements,
                 reqCodes1      : ReqCodes,
                 reqTags        : ReqData.Tags,
                 reqImps        : Implications): Gen[Project] = {
    val cissueIds       = cfg.customIssueTypes.keySet
    val cissueIdG       = Gen tryGenChoose cissueIds.toSeq
    val activeCodeIds   = reqCodes1.trie.allValues.flatMap(_.activeId.toStream)
    val activeCodeIdG   = Gen tryGenChoose activeCodeIds
    val atagIds         = cfg.tags.tree.valuesIterator.map(_.tag).filterSubType[ApplicableTag].map(_.id).toSet
    val atagIdG         = Gen.tryGenChoose(atagIds.toSeq)
    val textColIds      = cfg.fields.customFields.valuesIterator.filterSubType[CustomField.Text].map(_.id).toSet
    val reqIdSet        = reqsWithoutText.idIterator.toSet
    val reqIdG          = Gen tryGenChoose reqIdSet.toIndexedSeq
    def ucStepIds       = reqsWithoutText.useCases.stepIterator.map(_.id)
    val ucStepIdG       = Gen tryGenChoose ucStepIds.toIndexedSeq
    val rcgTitleText    = TextGen.codeGroupTitleAtom(reqIdG, ucStepIdG, activeCodeIdG, cissueIdG).text
    val delReasonText   = TextGen.deletionReasonAtom(reqIdG, ucStepIdG, activeCodeIdG, atagIdG).text1(Text.DeletionReason)
    val manualIssueText = TextGen.manualIssueAtom(reqIdG, ucStepIdG, activeCodeIdG, atagIdG).text1(Text.ManualIssue)
    for {
      name       ← projectName
      reqText    ← reqFieldDataText2(reqIdSet, textColIds, ucStepIdG, activeCodeIdG, cissueIdG, atagIdG)
      reqs       ← setReqText(reqsWithoutText, reqIdG, ucStepIdG, activeCodeIdG, cissueIdG, atagIdG)
      reqCodes2  ← reqCode.updateGroupText(rcgTitleText)(reqCodes1.trie)
      dr         ← deletionReasons(reqIdG, delReasonText)
      mis        ← genManualIssues(manualIssueText)
      p1         = Project(
                     name,
                     cfg,
                     ProjectContent(
                       reqs,
                       ReqCodes(reqCodes2),
                       reqText,
                       reqTags,
                       reqImps,
                       dr),
                     mis,
                     reqtable.SavedViews.empty,
                     IdCeilings.zero)
      savedViews ← reqtableData.savedViewsForProject(p1)
    } yield IdCeilings.supply(ic => p1.copy(reqtableViews = savedViews, idCeilings = ic))
  }

  lazy val project: Gen[Project] =
    for {
      cfg             ← projectConfig
      atagIds         = cfg.tags.tree.valuesIterator.map(_.tag).filterSubType[ApplicableTag].map(_.id).toSet
      reqCount        ← Gen.chooseSize
      ucCount         ← Gen.chooseSize map (_ >> 1)
      reqsWithoutText ← reqsWithoutText(cfg, reqCount, ucCount)
      reqIdSet        = reqsWithoutText.idIterator.toSet
      reqIdG          = Gen tryGenChoose reqIdSet.toIndexedSeq
      liveReqIds      = reqsWithoutText.reqIterator.filter(_.live(cfg.reqTypes) is Live).map(_.id)
      liveReqIdG      = Gen tryGenChoose liveReqIds.toIndexedSeq
      reqCodeDataG    = reqCode.data(liveReqIdG, reqIdG)(0 to (3 `JVM|JS` 2))
      reqCodes        ← reqCodes(reqCode.trie(reqCodeDataG, 2 `JVM|JS` 2))
      reqTags         ← reqFieldDataTags(reqIdSet, atagIds)
      reqImps         ← reqFieldDataImplications(reqIdSet)
      p               ← genProject(cfg, reqsWithoutText, reqCodes, reqTags, reqImps)
    } yield p

  lazy val projectAndOrd: Gen[ProjectAndOrd] =
    for {
      o <- events.eventOrd.map(_.asLatest).option
      p <- project
    } yield ProjectAndOrd(p, o)

  def projectIdPublic: Gen[ProjectId.Public] =
    obfuscated

  def projectName: Gen[Project.Name] =
    shortText1

  lazy val instantPast: Gen[Instant] = {
    val now = Instant.now()
    val secPerDay = 86400
    Gen.chooseLong(0, 365 * 5 * secPerDay).map(now.minusSeconds)
  }

  lazy val projectMetaData: Gen[ProjectMetaData] =
    for {
      id            <- projectIdPublic
      name          <- projectName
      eventsInit    <- Gen.chooseInt(3)
      eventsTotal   <- Gen.chooseInt(30000)
      reqsTotal     <- if (eventsTotal == 0) Gen.pure(0) else Gen.chooseInt(eventsTotal min 2000)
      reqsLive      <- if (reqsTotal == 0) Gen.pure(0) else Gen.chooseInt(reqsTotal)
      (t1, t2)      <- instantPast.pair
      createdAt      = if (t1 isBefore t2) t1 else t2
      accessedAt     = if (t1 isBefore t2) t2 else t1
      lastUpdatedAt <- instantPast.option.map(_.filter(_ isAfter createdAt))
    } yield
    ProjectMetaData(
      id            = id,
      name          = name,
      eventsInit    = eventsInit.min(eventsTotal),
      eventsTotal   = eventsTotal,
      reqsLive      = reqsLive,
      reqsTotal     = reqsTotal,
      createdAt     = createdAt,
      accessedAt    = accessedAt,
      lastUpdatedAt = lastUpdatedAt)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object reqtableData {
    import reqtable._

    def visibleColumns(p: Project): Gen[NonEmptyVector[Column]] =
      for {
        long ← Gen.long
        all  = Column all p.config
        cols ← Gen shuffle all.whole.toVector
      } yield {
        var i = long
        val vs = cols.filter(c =>
          if (Column isMandatory c)
            true
          else {
            val j = i
            i = i >>> 1
            if (i == 0) i = System.currentTimeMillis()
            (j & 1) == 1
          }
        )
        NonEmptyVector force vs
    }

    def sortMethodI: Gen[SortMethod.IgnoreBlanks] =
      Gen.chooseNE(SortMethod.ignoreBlanks)

    def sortMethodB: Gen[SortMethod.ConsiderBlanks] =
      Gen.chooseNE(SortMethod.considerBlanks)

    def columnC: Gen[Column.SortConclusive] =
      Gen pure Column.Pubid

    protected def `change ↖columnCon↖ if more conclusive criteria added`: Column.SortConclusive => Unit = {
      case Column.Pubid => ()
    }

    def sortCriteriaC: Gen[SortCriterion.Conclusive] =
      Gen.apply2(SortCriterion.Conclusive)(columnC, sortMethodI)

    val builtInColumnIs: NonEmptyVector[Column.SortInconclusive] =
      NonEmptyVector force
        (Column.builtInValues.whole: Vector[Column])
          .iterator
          .filterSubType[Column.SortInconclusive].toVector

    val builtInColumnIsG: Gen[Column.SortInconclusive] =
      Gen.chooseNE(builtInColumnIs)

    case class ColumnIGen(legalCustomFieldColumns: Vector[Column.CustomField]) {
      val legalCustomFieldColumnNEV: Option[NonEmptyVector[Column.CustomField]] =
        NonEmptyVector option legalCustomFieldColumns

      val legalColumnIs: NonEmptyVector[Column.SortInconclusive] =
        builtInColumnIs ++ legalCustomFieldColumns

      val legalColumns: NonEmptyVector[Column] =
        Column.builtInValues ++ legalCustomFieldColumns

      def columnI: Gen[Column.SortInconclusive] =
        Gen.chooseNE(legalColumnIs)

      def colIs: Gen[Vector[Column.SortInconclusive]] =
        Gen.subset(legalColumnIs.whole).shuffle

      def sortCriIs: Gen[Vector[SortCriterion.Inconclusive]] =
        colIs flatMap reqtableData.sortCriIs

      def columnNEV: Gen[NonEmptyVector[Column]] =
        Gen.subset1(legalColumns.whole).shuffle[Vector, Column].map(NonEmptyVector.force)

    } // end ColumnIGen

    def customFieldColumn: Gen[Column.CustomField] =
      RandomData.customFieldId.map(Column.CustomField)

    def sortCriI(colI: Column.SortInconclusive): Gen[SortCriterion.Inconclusive] =
      Gen.chooseNE(SortCriterion possibilitiesI colI)

    def sortCriIsFromAllCols(allCols: NonEmptyVector[Column]): Gen[Vector[SortCriterion.Inconclusive]] = {
      val icols = allCols.iterator.filterSubType[Column.SortInconclusive].toVector
      Gen.subset(icols).shuffle flatMap sortCriIs
    }

    def sortCriIs(colIs: Vector[Column.SortInconclusive]): Gen[Vector[SortCriterion.Inconclusive]] =
      Gen.sequence(colIs map sortCriI)

    def sortCriteria(allCols: NonEmptyVector[Column]): Gen[SortCriteria] =
      sortCriIsFromAllCols(allCols).flatMap(sortCriteria)

    def sortCriteria(scIs: Vector[SortCriterion.Inconclusive]): Gen[SortCriteria] =
      sortCriteriaC.map(SortCriteria(scIs, _))

    val savedViewId: Gen[SavedView.Id] =
      id.map(SavedView.Id)

    val savedViewName: Gen[SavedView.Name] =
      for {
        a <- Gen.alpha
        b <- Gen.ascii.vector(SavedView.Name.lengthRange.map(_ - 1))
        c <- Gen.shuffle(b :+ a)
        d  = String.valueOf(c.toArray)
      } yield SavedView.Name.validator.stateless.unnamed(d)
        .valueOr(e => sys error s"$e: '${SavedView.Name.validator.stateless.corrector.full(d)}' ← '$d'")

    def viewForProject(p: Project): Gen[View] =
      for {
        d <- filterDead
        c <- visibleColumns(p)
        o <- sortCriteria(c)
        f <- filter.valid.forProject(p).option
      } yield View(c, o, d, f)

    def savedViewForProject(p: Project): Gen[SavedView] =
      for {
        i <- savedViewId
        n <- savedViewName
        f <- viewForProject(p)
      } yield SavedView(i, n, f)

    def nonEmptySavedViewsForProject(p: Project): Gen[SavedViews.NonEmpty] = {
      val gen = savedViewForProject(p)
      for {
        d  <- gen
        vs <- gen.vector(0 to 4).map(_.filter(v => v.id !=* d.id && !v.name.value.equalsIgnoreCase(d.name.value)))
      } yield SavedViews(d) ++ vs
    }

    def savedViewsForProject(p: Project): Gen[SavedViews.Optional] =
      nonEmptySavedViewsForProject(p).option
  }

  // ===================================================================================================================
  // Protocol
  object protocol {
    import shipreq.webapp.base.protocol._

    val reqTypeId: Gen[ReqTypeId] =
      Gen.chooseGen(customReqTypeId, staticReqType)

    val fieldId: Gen[FieldId] =
      Gen.chooseGen(customFieldId, staticField)

    val applicableReqTypes: Gen[ApplicableReqTypes] =
      genISubset(reqTypeId.nes)

    val fieldPosition =
      fieldId.option

    val textFieldValues =
      Gen.apply4(UpdateConfigCmd.TextFieldValues.apply)(shortText1, fieldRefKey, mandatory, applicableReqTypes)

    val customFieldValues: Gen[UpdateConfigCmd.CustomFieldValues] =
      Gen.chooseGen(textFieldValues)

//    object updateConfigCmd {
//      import UpdateConfigCmd._
//      val create      : Gen[CustomFieldCreate]       = customFieldValues map Create
//      val updateValues: Gen[CustomFieldUpdateValues] = Gen.apply2(UpdateValues)(customFieldId, customFieldValues)
//      val updateOrder : Gen[CustomFieldUpdateOrder]  = Gen.apply2(UpdateOrder)(fieldId, fieldPosition)
//      val delete      : Gen[CustomFieldDelete]       = fieldId map Delete
//      val restore     : Gen[CustomFieldRestore]      = fieldId map Restore
//      val any         : Gen[UpdateConfigCmd]    = Gen.chooseGen(create, updateValues, updateOrder, delete)
//    }

//    def tagProtocolValues: Tag => TagCrud.Values = {
//      case TagGroup(_, n, d, mc, _)     => TagCrud.TagGroupValues(n, mc, d)
//      case ApplicableTag(_, n, d, k, _) => TagCrud.ApplicableTagValues(n, k, d)
//    }
//
//    val tagCrudInput =
//      tagAndRels.flatMap(t => {
//        val a = Gen pure tagProtocolValues(t._1)
//        val b = Gen pure t._2
//        a \&/ b
//      })
  }

  // ===================================================================================================================
  object routines {
    import shipreq.webapp.base.protocol._
    import RandomData.protocol._

    def projectSpaInitAppData: Gen[ProjectSpaProtocols.InitAppData] =
      for {
        a <- projectAndOrd
        b <- projectMetaData
      } yield ProjectSpaProtocols.InitAppData(a, b)

    val projectSpaInitPageData: Gen[ProjectSpaEntryPoint.InitData] =
      for {
        u <- username
        i <- projectIdPublic
        n <- projectName
      } yield ProjectSpaEntryPoint.InitData(u, i, n)

//    class CrudActionGens[I, V](idG: Gen[I], vG: Gen[V]) {
//      lazy val create  = vG.map(CrudAction.Create[I, V])
//      lazy val update  = Gen.apply2(CrudAction.Update[I, V])(idG, vG)
//      lazy val delete  = idG map CrudAction.Delete[I, V]
//      lazy val restore = idG map CrudAction.Restore[I, V]
//      lazy val any     = Gen.chooseGen[CrudAction[I, V]](create, update, delete, restore)
//    }

//    val customIssueTypeCrud = new CrudActionGens(
//      RandomData.customIssueTypeId,
//      Gen.tuple2(hashRefKey, optionalLargeText))

//    val customReqTypeCrud = new CrudActionGens(
//      RandomData.customReqTypeId,
//      Gen.tuple3(reqTypeMnemonic, customReqTypeName, implicationRequired))

//    val tagCrud =
//      new CrudActionGens(RandomData.tagId, tagCrudInput)
  }

  // ===================================================================================================================
  object filter {
    import shipreq.webapp.base.filter._
    import shipreq.webapp.base.filter.Filter._
    import shipreq.webapp.base.filter.Filter.Implicits._
    import shipreq.webapp.base.filter.IntensionalReqSet._

    val quotedText =
      for {
        q <- Gen.choose('\'', '"', '`')
        s <- unicodeString1
      } yield {
        val s2 = s.map {
          case `q` => '_'
          case '\n' | '\r' => ' '
          case x => x
        }
        FilterAst.Text(s2, Some(q))
      }

    private val illegalSimpleTextStart = "/-#(){}'`\"".toCharArray.toSet
    private def fixSimpleText(s0: String): String = {
      val s = FilterParser.preProcessor(s0).asString.replace(' ', '_')
      if (s.isEmpty)
        "_"
      else if (s.headOption exists illegalSimpleTextStart.contains)
        "!" + s
      else if (DataValidators.reqType.mnemonic.stateless.validity(s) is shipreq.base.util.Valid)
        s + "?"
      else
        s
    }

    /** An odd number of backslashes cannot precede a slash */
    private val fixSlashEscaping = """(^|[^\\])(?:\\(?:\\\\)*)/""".r

    private def fixRegex(s0: String): String = {
      val s = FilterParser.preProcessor(s0).asString
      if (s endsWith "\\")
        s + "d"
      else
        fixSlashEscaping.replaceAllIn(s, "$1/")
    }

    val simpleText =
      charPred(FilterParser.simpleTextChar).string1
        .map(s => FilterAst.Text(fixSimpleText(s), None))

    val regex =
      unicodeString1.map(s => FilterAst.Regex(fixRegex(s)))

    def fixRoot[A, B, C, D, E](f: FilterAst.Fixed[A, B, C, D, E]): FilterAst.Fixed[A, B, C, D, E] =
      f.unfix match {
        case FilterAst.AllOf(a) if a.tail.isEmpty => a.head
        case _ => f
      }

    // -----------------------------------------------------------------------------------------------------------------
    object potential {

      val wholeType: Gen[WholeType[Potential.ReqType]] =
        reqTypeMnemonic.map(WholeType(_))

      val numberRange: Gen[NonEmptySet[Int]] =
        Gen.chooseInt(1,10000).nes(0 to (20 `JVM|JS` 6), implicitly)

      val someOfType: Gen[SomeOfType[Potential.ReqType]] =
        for {
          rt <- reqTypeMnemonic
          ns <- numberRange
        } yield SomeOfType(rt, ns)

      val reqsSpec: Gen[Potential.ReqSubset] =
        Gen.chooseGen(wholeType, someOfType)

      val reqSpecs: Gen[Potential.ReqSet] =
        reqsSpec.nev(0 to 5)

      val attr: Gen[String] =
        Gen.chooseGen(
          Gen.alpha.string1(1 to 4),
          valid.attr.map(_.name))

      val hasIssue = {
        val ic = Gen.chooseGen(
          Gen.alpha.string1(1 to 4),
          issueCategory.map(FilterAst.issueCategoryToStr))
        Gen.lift2(on, ic.nev(1 to 3))(FilterAst.HasIssue(_, _))
      }

      val reqType    = reqTypeMnemonic.map(FilterAst.ReqType(_))
      val hashRef    = hashRefKey     .map(FilterAst.HashRef(_))
      val reqs       = someOfType     .map(s => FilterAst.Reqs(NonEmptyVector.one(s)))
      val implies    = reqSpecs       .map(FilterAst.ImpliesAnyOf(_))
      val impliedBy  = reqSpecs       .map(FilterAst.ImpliedByAnyOf(_))
      val presence   = attr           .map(FilterAst.Presence(_))

      private val flatGens: NonEmptyVector[Gen[PotentialF[Nothing]]] =
        NonEmptyVector(quotedText, simpleText, regex, reqs, reqType, hashRef, implies, impliedBy, presence, hasIssue)

      private val flatGen: Gen[PotentialF[Nothing]] =
        Gen.chooseGenNE(flatGens)

      private val coalgebra: FCoalgebraM[Gen, PotentialF, Int] =
        remainingDepth => {
          if (remainingDepth <= 0)
            flatGen
          else {
            val next = remainingDepth - 1
            val genNEV = Gen.pure(next).nev(1 to (5 `JVM|JS` 3))
            var gens: NonEmptyVector[Gen[PotentialF[Int]]] = flatGens
            gens :+= Gen.pure(FilterAst.Not(next))
            gens :+= genNEV.map(FilterAst.AllOf(_))
            gens :+= genNEV.map(FilterAst.AnyOf(next, _))
            Gen.chooseGenNE(flatGens)
          }
        }

      val gen: Gen[Potential] =
        Recursion.anaM(coalgebra)(4 `JVM|JS` 3).map(fixRoot)
    }

    // -----------------------------------------------------------------------------------------------------------------
    object valid {
      import FilterAst.Attr

      def wholeType(g: Gen[Valid.ReqType]): Gen[WholeType[Valid.ReqType]] =
        g.map(WholeType(_))

      def numberRange: Gen[NonEmptySet[Int]] =
        potential.numberRange

      def someOfType(g: Gen[Valid.ReqType]): Gen[SomeOfType[Valid.ReqType]] =
        for {
          rt <- g
          ns <- numberRange
        } yield SomeOfType(rt, ns)

      def reqsSpec(g: Gen[Valid.ReqType]): Gen[Valid.ReqSubset] =
        Gen.chooseGen(wholeType(g), someOfType(g))

      def reqSpecs(g: Gen[Valid.ReqType]): Gen[Valid.ReqSet] =
        reqsSpec(g).nev(0 to 5)

      val attr: Gen[Attr] =
        Gen.chooseNE(Attr.values)

      val hasIssue =
        Gen.lift2(on, issueCategory.nev(1 to 3))(FilterAst.HasIssue(_, _))

      val presence = attr.map(FilterAst.Presence(_))

      def reqs       (g: Gen[Valid.ReqType])    : Gen[ValidF[Nothing]] = someOfType(g).map(s => FilterAst.Reqs(NonEmptyVector.one(s)))
      def implies    (g: Gen[Valid.ReqSet])     : Gen[ValidF[Nothing]] = g.map(FilterAst.ImpliesAnyOf(_))
      def impliedBy  (g: Gen[Valid.ReqSet])     : Gen[ValidF[Nothing]] = g.map(FilterAst.ImpliedByAnyOf(_))
      def reqType    (g: Gen[Valid.ReqType])    : Gen[ValidF[Nothing]] = g.map(FilterAst.ReqType(_))
      def tag        (g: Gen[ApplicableTagId])  : Gen[ValidF[Nothing]] = g.map(i => FilterAst.HashRef(\/-(i)))
      def customIssue(g: Gen[CustomIssueTypeId]): Gen[ValidF[Nothing]] = g.map(i => FilterAst.HashRef(-\/(i)))

      type FlatGens = NonEmptyVector[Gen[ValidF[Nothing]]]

      def flatGens(gy: Option[Gen[ReqTypeId]],
                   gt: Option[Gen[ApplicableTagId]],
                   gi: Option[Gen[CustomIssueTypeId]]): FlatGens = {
        val greqs = gy.map(reqSpecs)
        NonEmptyVector[Gen[ValidF[Nothing]]](quotedText, simpleText, regex, presence, hasIssue) ++
          gy.map(reqType) ++
          gt.map(tag) ++
          gi.map(customIssue) ++
          gy.map(reqs) ++
          greqs.map(implies) ++
          greqs.map(impliedBy)
      }

      private def coalgebra(flatGens: FlatGens): FCoalgebraM[Gen, ValidF, Int] = {
        val flatGen = Gen.chooseGenNE(flatGens)
        remainingDepth =>
          if (remainingDepth <= 0)
            flatGen
          else {
            val next = remainingDepth - 1
            val genNEV = Gen.pure(next).nev(1 to (5 `JVM|JS` 3))
            var gens: NonEmptyVector[Gen[ValidF[Int]]] = flatGens
            gens :+= Gen.pure(FilterAst.Not(next))
            gens :+= genNEV.map(FilterAst.AllOf(_))
            gens :+= genNEV.map(FilterAst.AnyOf(next, _))
            Gen.chooseGenNE(flatGens)
          }
      }

      private def gen(f: FlatGens): Gen[Valid] =
        Recursion.anaM(coalgebra(f))(4 `JVM|JS` 3).map(fixRoot)

      def forProject(p: Project): Gen[Valid] = {
        val gy: Option[Gen[ReqTypeId]]         = Gen tryGenChoose p.config.reqTypes.all.whole.map(_.reqTypeId)
        val gt: Option[Gen[ApplicableTagId]]   = Gen tryGenChoose p.config.tags.atagIterator().map(_.id)
        val gi: Option[Gen[CustomIssueTypeId]] = Gen tryGenChoose p.config.customIssueTypes.keys.toVector
        gen(flatGens(gy, gt, gi))
      }

      lazy val arbitrary: Gen[Valid] = {
        val gy: Option[Gen[ReqTypeId]]         = Some(reqTypeId)
        val gt: Option[Gen[ApplicableTagId]]   = Some(applicableTagId)
        val gi: Option[Gen[CustomIssueTypeId]] = Some(customIssueTypeId)
        gen(flatGens(gy, gt, gi))
      }
    }
  }

  // ===================================================================================================================
  object events {
    import shipreq.webapp.base.event._
    import Event._

    val tagChildren: Gen[TagInTree.Children] =
      tagId.vector

    val tagParents: Gen[TagInTree.Parents] =
      tagId.option mapBy tagId

    val anyApplicableReqTypes: Gen[ApplicableReqTypes] =
      customReqTypeId.set flatMap applicableReqTypes

    val reqCodeIdAndValue: Gen[ApReqCodeId.AndValue] =
      Gen.apply2(ApReqCodeId.AndValue)(reqCode.apId, reqCode.value)

    val fieldId: Gen[FieldId] =
      Gen.chooseGen(staticField, customFieldId)

    val useCaseStepTreeField: Gen[StaticField.UseCaseStepTree] =
      Gen.chooseNE(StaticField.useCaseStepTrees)

    private[this] val r = Some(reqId)
    private[this] val u = Some(useCaseStepId)
    private[this] val c = Some(reqCode.id)
    private[this] val i = Some(customIssueTypeId)
    private[this] val a = Some(applicableTagId)

    val customTextFieldAtom = TextGen.customTextFieldAtom(r, u, c, i, a)
    val customTextField     = customTextFieldAtom.text
    val customTextField1    = customTextFieldAtom.text1(Text.CustomTextField)
    val codeGroupTitle      = TextGen.codeGroupTitleAtom(r, u, c, i).text
    val genericReqTitleAtom = TextGen.genericReqTitleAtom(r, u, c, i, a)
    val genericReqTitle     = genericReqTitleAtom.text
    val genericReqTitle1    = genericReqTitleAtom.text1(Text.GenericReqTitle)
    val useCaseTitleAtom    = TextGen.useCaseTitleAtom(r, u, c, i, a)
    val useCaseTitle        = useCaseTitleAtom.text
    val useCaseTitle1       = useCaseTitleAtom.text1(Text.UseCaseTitle)
    val useCaseStepTextAtom = TextGen.useCaseStepAtom(r, u, c, i, a)
    val useCaseStepText     = useCaseStepTextAtom.text

    val stepFlowSetDiff =
      genNonEmptySetDiff(useCaseStepId)

    val manualIssueText: Gen[Text.ManualIssue.NonEmptyText] =
      TextGen.manualIssueAtom(r, u, c, a).text1(Text.ManualIssue)

    val deletionReason: Gen[Text.DeletionReason.OptionalText] =
      TextGen.deletionReasonAtom(r, u, c, a).text

    val nonEmptyCustomTextMap: Gen[Event.NonEmptyCustomTextMap] =
      customTextField1.mapBy(customFieldTextId)(1 to 3).map(NonEmpty.force)

    object customIssueTypeGD extends GenericDataGen(CustomIssueTypeGD) {
      import gd._
      override def valueFor(a: Attr): Gen[Value] = a match {
        case Key  => hashRefKey            map Key .apply
        case Desc => unicodeString1.option map Desc.apply
      }
    }

    object customReqTypeGD extends GenericDataGen(CustomReqTypeGD) {
      import gd._
      override def valueFor(a: Attr): Gen[Value] = a match {
        case Name        => unicodeString1      map Name       .apply
        case Imp         => implicationRequired map Imp        .apply
        case gd.Mnemonic => reqTypeMnemonic     map gd.Mnemonic.apply
      }
    }

    object customTextFieldGD extends GenericDataGen(CustomTextFieldGD) {
      import gd._
      override def valueFor(a: Attr): Gen[Value] = a match {
        case Name      => unicodeString1        map Name     .apply
        case Key       => fieldRefKey           map Key      .apply
        case Mandatory => mandatory             map Mandatory.apply
        case ReqTypes  => anyApplicableReqTypes map ReqTypes .apply
      }
    }

    object customTagFieldGD extends GenericDataGen(CustomTagFieldGD) {
      import gd._
      override def valueFor(a: Attr): Gen[Value] = a match {
        case TagId     => tagId                 map TagId    .apply
        case Mandatory => mandatory             map Mandatory.apply
        case ReqTypes  => anyApplicableReqTypes map ReqTypes .apply
      }
    }

    object customImpFieldGD extends GenericDataGen(CustomImpFieldGD) {
      import gd._
      override def valueFor(a: Attr): Gen[Value] = a match {
        case ReqTypeId => reqTypeId             map ReqTypeId.apply
        case Mandatory => mandatory             map Mandatory.apply
        case ReqTypes  => anyApplicableReqTypes map ReqTypes .apply
      }
    }

    object genericReqGD extends GenericDataGen(GenericReqGD) {
      import gd._
      override def valueFor(a: Attr): Gen[Value] = a match {
        case Codes      => reqCodeIdAndValue.nes map Codes     .apply
        case CustomText => nonEmptyCustomTextMap map CustomText.apply
        case ImpSrcs    => reqId.nes             map ImpSrcs   .apply
        case ImpTgts    => reqId.nes             map ImpTgts   .apply
        case Tags       => applicableTagId.nes   map Tags      .apply
        case Title      => genericReqTitle1      map Title     .apply
      }
    }

    object useCaseGD extends GenericDataGen(UseCaseGD) {
      import gd._
      override def valueFor(a: Attr): Gen[Value] = a match {
        case Codes      => reqCodeIdAndValue.nes map Codes     .apply
        case CustomText => nonEmptyCustomTextMap map CustomText.apply
        case ImpSrcs    => reqId.nes             map ImpSrcs   .apply
        case ImpTgts    => reqId.nes             map ImpTgts   .apply
        case Tags       => applicableTagId.nes   map Tags      .apply
        case Title      => useCaseTitle1         map Title     .apply
      }
    }

    object codeGroupGD extends GenericDataGen(CodeGroupGD) {
      import gd._
      override def valueFor(a: Attr): Gen[Value] = a match {
        case Code  => reqCode.value     map Code .apply
        case Title => codeGroupTitle map Title.apply
      }
    }

    object applicableTagGD extends GenericDataGen(ApplicableTagGD) {
      import gd._
      override def valueFor(a: Attr): Gen[Value] = a match {
        case Name     => unicodeString1        map Name    .apply
        case Desc     => unicodeString1.option map Desc    .apply
        case Key      => hashRefKey            map Key     .apply
        case Children => tagChildren           map Children.apply
        case Parents  => tagParents            map Parents .apply
      }
    }

    object tagGroupGD extends GenericDataGen(TagGroupGD) {
      import gd._
      override def valueFor(a: Attr): Gen[Value] = a match {
        case Name          => unicodeString1        map Name         .apply
        case Desc          => unicodeString1.option map Desc         .apply
        case MutexChildren => mutexChildren         map MutexChildren.apply
        case Children      => tagChildren           map Children     .apply
        case Parents       => tagParents            map Parents      .apply
      }
    }

    object useCaseStepGD extends GenericDataGen(UseCaseStepGD) {
      import gd._
      override def valueFor(a: Attr): Gen[Value] = a match {
        case Title   => useCaseStepText map Title  .apply
        case FlowIn  => stepFlowSetDiff map FlowIn .apply
        case FlowOut => stepFlowSetDiff map FlowOut.apply
      }
    }

    object savedViewGD extends GenericDataGen(SavedViewGD) {
      import gd._
      import reqtableData._
      private val colNev = customFieldColumn.vector.map(ColumnIGen).flatMap(_.columnNEV)

      val genColumns      = colNev
      val genFilter       = filter.valid.arbitrary.option
      val genFilterDead   = filterDead
      val genName         = savedViewName
      val genSortCriteria = colNev.flatMap(sortCriteria)

      override def valueFor(a: Attr): Gen[Value] = a match {
        case Columns    => genColumns      map Columns   .apply
        case Filter     => genFilter       map Filter    .apply
        case FilterDead => genFilterDead   map FilterDead.apply
        case Name       => genName         map Name      .apply
        case Order      => genSortCriteria map Order     .apply
      }
    }

    val genUseCaseStepCreate: Gen[UseCaseStepCreate] =
      Gen.apply4(UseCaseStepCreate)(useCaseStepId, useCaseId, useCaseStepTreeField, genVectorTreeParLoc)

    val genFieldStaticAdd: Gen[FieldStaticAdd] =
      staticField map FieldStaticAdd

    val genProjectTemplate: Gen[ProjectTemplate] =
      Gen.chooseNE(ProjectTemplate.values)

    val genProjectTemplateApply: Gen[ProjectTemplateApply] =
      genProjectTemplate map ProjectTemplateApply

    val genApplicableTagCreate: Gen[ApplicableTagCreate] =
      Gen.apply2(ApplicableTagCreate)(applicableTagId, applicableTagGD.nonEmptyValues)

    val genFieldCustomImpCreate: Gen[FieldCustomImpCreate] =
      Gen.apply2(FieldCustomImpCreate)(customFieldImplicationId, customImpFieldGD.nonEmptyValues)

    val genCustomIssueTypeCreate: Gen[CustomIssueTypeCreate] =
      Gen.apply2(CustomIssueTypeCreate)(customIssueTypeId, customIssueTypeGD.nonEmptyValues)

    val genCustomReqTypeCreate: Gen[CustomReqTypeCreate] =
      Gen.apply2(CustomReqTypeCreate)(customReqTypeId, customReqTypeGD.nonEmptyValues)

    val genFieldCustomTagCreate: Gen[FieldCustomTagCreate] =
      Gen.apply2(FieldCustomTagCreate)(customFieldTagId, customTagFieldGD.nonEmptyValues)

    val genFieldCustomTextCreate: Gen[FieldCustomTextCreate] =
      Gen.apply2(FieldCustomTextCreate)(customFieldTextId, customTextFieldGD.nonEmptyValues)

    val genGenericReqCreate: Gen[GenericReqCreate] =
      Gen.apply3(GenericReqCreate)(genericReqId, customReqTypeId, genericReqGD.values)

    val genCodeGroupCreate: Gen[CodeGroupCreate] =
      Gen.apply2(CodeGroupCreate)(reqCode.groupId, codeGroupGD.nonEmptyValues)

    val genTagGroupCreate: Gen[TagGroupCreate] =
      Gen.apply2(TagGroupCreate)(tagGroupId, tagGroupGD.nonEmptyValues)

    val genUseCaseCreate: Gen[UseCaseCreate] =
      Gen.apply3(UseCaseCreate)(useCaseId, useCaseStepId, useCaseGD.values)

    val genFieldCustomDelete: Gen[FieldCustomDelete] =
      customFieldId map FieldCustomDelete

    val genFieldCustomRestore: Gen[FieldCustomRestore] =
      customFieldId map FieldCustomRestore

    val genCustomIssueTypeDelete: Gen[CustomIssueTypeDelete] =
      customIssueTypeId map CustomIssueTypeDelete

    val genCustomIssueTypeRestore: Gen[CustomIssueTypeRestore] =
      customIssueTypeId map CustomIssueTypeRestore

    val genCustomReqTypeDelete: Gen[CustomReqTypeDelete] =
      customReqTypeId map CustomReqTypeDelete

    val genCustomReqTypeRestore: Gen[CustomReqTypeRestore] =
      customReqTypeId map CustomReqTypeRestore

    val genCodeGroupsDelete: Gen[CodeGroupsDelete] =
      reqCode.groupId.nes map CodeGroupsDelete

    val genReqsDelete: Gen[ReqsDelete] =
      Gen.apply3(ReqsDelete)(reqId.nes, reqCode.groupId.set, deletionReason)

    val genFieldStaticRemove: Gen[FieldStaticRemove] =
      staticField map FieldStaticRemove

    val genTagDelete: Gen[TagDelete] =
      tagId map TagDelete

    val genTagRestore: Gen[TagRestore] =
      tagId map TagRestore

    val genUseCaseStepDelete: Gen[UseCaseStepDelete] =
      useCaseStepId map UseCaseStepDelete

    val genUseCaseStepRestore: Gen[UseCaseStepRestore] =
      useCaseStepId map UseCaseStepRestore

    val genReqImplicationsPatch: Gen[ReqImplicationsPatch] =
      Gen.apply3(ReqImplicationsPatch)(reqId, dir, genNonEmptySetDiff(reqId))

    val genReqCodesPatch: Gen[ReqCodesPatch] = {
      val codes = reqCode.apId.set(0 to 3)
      for {
        id      ← reqId
        add     ← codes.mapBy(reqCode.value).map(Multimap(_)) // TODO Could have same ID with different codes
        addIds  = add.valueIterator.toSet
        remove  ← codes.map(_ -- addIds)
        restore ← codes.map(_ -- addIds -- remove)
      } yield ReqCodesPatch(id, remove, restore, add)
    }

    val genReqTagsPatch: Gen[ReqTagsPatch] =
      Gen.apply2(ReqTagsPatch)(reqId, genNonEmptySetDiff(applicableTagId))

    val genFieldReposition: Gen[FieldReposition] =
      Gen.apply2(FieldReposition)(fieldId, fieldId.option)

    val genContentRestore: Gen[ContentRestore] =
      Gen.apply2(ContentRestore)(reqId.set, reqCode.groupId.set(0 to 3))

    val genReqFieldCustomTextSet: Gen[ReqFieldCustomTextSet] =
      Gen.apply3(ReqFieldCustomTextSet)(reqId, customFieldTextId, customTextField)

    val genGenericReqTitleSet: Gen[GenericReqTitleSet] =
      Gen.apply2(GenericReqTitleSet)(genericReqId, genericReqTitle)

    val genGenericReqTypeSet: Gen[GenericReqTypeSet] =
      Gen.apply2(GenericReqTypeSet)(genericReqId, customReqTypeId)

    val genUseCaseTitleSet: Gen[UseCaseTitleSet] =
      Gen.apply2(UseCaseTitleSet)(useCaseId, useCaseTitle)

    val genUseCaseStepShiftLeft: Gen[UseCaseStepShiftLeft] =
      useCaseStepId map UseCaseStepShiftLeft

    val genUseCaseStepShiftRight: Gen[UseCaseStepShiftRight] =
      useCaseStepId map UseCaseStepShiftRight

    val genApplicableTagUpdate: Gen[ApplicableTagUpdate] =
      Gen.apply2(ApplicableTagUpdate)(applicableTagId, applicableTagGD.nonEmptyValues)

    val genFieldCustomImpUpdate: Gen[FieldCustomImpUpdate] =
      Gen.apply2(FieldCustomImpUpdate)(customFieldImplicationId, customImpFieldGD.nonEmptyValues)

    val genCustomIssueTypeUpdate: Gen[CustomIssueTypeUpdate] =
      Gen.apply2(CustomIssueTypeUpdate)(customIssueTypeId, customIssueTypeGD.nonEmptyValues)

    val genCustomReqTypeUpdate: Gen[CustomReqTypeUpdate] =
      Gen.apply2(CustomReqTypeUpdate)(customReqTypeId, customReqTypeGD.nonEmptyValues)

    val genFieldCustomTagUpdate: Gen[FieldCustomTagUpdate] =
      Gen.apply2(FieldCustomTagUpdate)(customFieldTagId, customTagFieldGD.nonEmptyValues)

    val genFieldCustomTextUpdate: Gen[FieldCustomTextUpdate] =
      Gen.apply2(FieldCustomTextUpdate)(customFieldTextId, customTextFieldGD.nonEmptyValues)

    val genCodeGroupUpdate: Gen[CodeGroupUpdate] =
      Gen.apply2(CodeGroupUpdate)(reqCode.groupId, codeGroupGD.nonEmptyValues)

    val genTagGroupUpdate: Gen[TagGroupUpdate] =
      Gen.apply2(TagGroupUpdate)(tagGroupId, tagGroupGD.nonEmptyValues)

    val genUseCaseStepUpdate: Gen[UseCaseStepUpdate] =
      Gen.apply2(UseCaseStepUpdate)(useCaseStepId, useCaseStepGD.nonEmptyValues)

    val genSavedViewCreate: Gen[SavedViewCreate] =
      Gen.apply6(SavedViewCreate)(
        reqtableData.savedViewId,
        savedViewGD.genName,
        savedViewGD.genColumns,
        savedViewGD.genSortCriteria,
        savedViewGD.genFilterDead,
        savedViewGD.genFilter)

    val genSavedViewDefaultSet: Gen[SavedViewDefaultSet] =
      reqtableData.savedViewId map SavedViewDefaultSet

    val genSavedViewDelete: Gen[SavedViewDelete] =
      reqtableData.savedViewId map SavedViewDelete

    val genSavedViewUpdate: Gen[SavedViewUpdate] =
      Gen.apply2(SavedViewUpdate)(reqtableData.savedViewId, savedViewGD.nonEmptyValues)

    val genProjectNameSet: Gen[ProjectNameSet] =
      projectName map ProjectNameSet

    val manualIssueId = id map ManualIssueId

    val genManualIssueCreate = Gen.apply2(ManualIssueCreate)(manualIssueId, manualIssueText)
    val genManualIssueUpdate = Gen.apply2(ManualIssueUpdate)(manualIssueId, manualIssueText)
    val genManualIssueDelete = manualIssueId.map(ManualIssueDelete)

    val activeEventGens: NonEmptyVector[Gen[ActiveEvent]] =
      valuesForAdt[ActiveEvent, Gen[ActiveEvent]] {
        case _: ApplicableTagCreate    => genApplicableTagCreate
        case _: ApplicableTagUpdate    => genApplicableTagUpdate
        case _: ContentRestore         => genContentRestore
        case _: CustomIssueTypeCreate  => genCustomIssueTypeCreate
        case _: CustomIssueTypeDelete  => genCustomIssueTypeDelete
        case _: CustomIssueTypeRestore => genCustomIssueTypeRestore
        case _: CustomIssueTypeUpdate  => genCustomIssueTypeUpdate
        case _: CustomReqTypeCreate    => genCustomReqTypeCreate
        case _: CustomReqTypeDelete    => genCustomReqTypeDelete
        case _: CustomReqTypeRestore   => genCustomReqTypeRestore
        case _: CustomReqTypeUpdate    => genCustomReqTypeUpdate
        case _: FieldCustomDelete      => genFieldCustomDelete
        case _: FieldCustomImpCreate   => genFieldCustomImpCreate
        case _: FieldCustomImpUpdate   => genFieldCustomImpUpdate
        case _: FieldCustomRestore     => genFieldCustomRestore
        case _: FieldCustomTagCreate   => genFieldCustomTagCreate
        case _: FieldCustomTagUpdate   => genFieldCustomTagUpdate
        case _: FieldCustomTextCreate  => genFieldCustomTextCreate
        case _: FieldCustomTextUpdate  => genFieldCustomTextUpdate
        case _: FieldReposition        => genFieldReposition
        case _: FieldStaticAdd         => genFieldStaticAdd
        case _: FieldStaticRemove      => genFieldStaticRemove
        case _: GenericReqCreate       => genGenericReqCreate
        case _: GenericReqTitleSet     => genGenericReqTitleSet
        case _: GenericReqTypeSet      => genGenericReqTypeSet
        case _: ManualIssueCreate      => genManualIssueCreate
        case _: ManualIssueDelete      => genManualIssueDelete
        case _: ManualIssueUpdate      => genManualIssueUpdate
        case _: ProjectNameSet         => genProjectNameSet
        case _: ProjectTemplateApply   => genProjectTemplateApply
        case _: CodeGroupCreate        => genCodeGroupCreate
        case _: CodeGroupsDelete       => genCodeGroupsDelete
        case _: CodeGroupUpdate        => genCodeGroupUpdate
        case _: ReqCodesPatch          => genReqCodesPatch
        case _: ReqFieldCustomTextSet  => genReqFieldCustomTextSet
        case _: ReqImplicationsPatch   => genReqImplicationsPatch
        case _: ReqsDelete             => genReqsDelete
        case _: ReqTagsPatch           => genReqTagsPatch
        case _: SavedViewCreate        => genSavedViewCreate
        case _: SavedViewDefaultSet    => genSavedViewDefaultSet
        case _: SavedViewDelete        => genSavedViewDelete
        case _: SavedViewUpdate        => genSavedViewUpdate
        case _: TagDelete              => genTagDelete
        case _: TagGroupCreate         => genTagGroupCreate
        case _: TagGroupUpdate         => genTagGroupUpdate
        case _: TagRestore             => genTagRestore
        case _: UseCaseCreate          => genUseCaseCreate
        case _: UseCaseStepCreate      => genUseCaseStepCreate
        case _: UseCaseStepDelete      => genUseCaseStepDelete
        case _: UseCaseStepRestore     => genUseCaseStepRestore
        case _: UseCaseStepShiftLeft   => genUseCaseStepShiftLeft
        case _: UseCaseStepShiftRight  => genUseCaseStepShiftRight
        case _: UseCaseStepUpdate      => genUseCaseStepUpdate
        case _: UseCaseTitleSet        => genUseCaseTitleSet
      }

    val activeEvent: Gen[ActiveEvent] =
      Gen.chooseGenNE(activeEventGens)

    val event: Gen[Event] = {
      val gens = valuesForAdt[Event, NonEmptyVector[Gen[Event]]] {
        case _: ActiveEvent => activeEventGens
      }
      Gen.chooseGenNE(gens flatMap identity)
    }

    val eventOrd: Gen[EventOrd] =
      Gen.chooseInt(100000).map(i => EventOrd(i + 1))

    val verifiedEvent: Gen[VerifiedEvent] =
      Gen.apply3(VerifiedEvent.apply)(eventOrd, event, instantPast)
  }
}
