package shipreq.webapp.base

import japgolly.nyaya.util._
import japgolly.nyaya.test.{Distinct, Gen, GenS}
import monocle.{Lens, Traversal, PTraversal}
import monocle.function.{first, second, third}
import monocle.std.{some => atSome}
import monocle.std.tuple2._
import monocle.std.tuple3._
import scala.annotation.tailrec
import scala.collection.GenTraversable
import scalaz.{NonEmptyList, OneAnd, State, StateT, Need}
import scalaz.std.list._
import scalaz.std.option.{none => _, _}
import scalaz.std.set._
import scalaz.std.stream._
import scalaz.std.vector._

import shipreq.base.util._, MTrie.Ops
import shipreq.base.util.ScalaExt._
import shipreq.base.util.TaggedTypes.TaggedInt
import shipreq.base.util.Debug._
import shipreq.webapp.base.data._, ReqType.Mnemonic, Field.ApplicableReqTypes
import shipreq.webapp.base.event.DeletionAction
import shipreq.webapp.base.test._
import shipreq.webapp.base.text.{Text, Grammar}
import shipreq.webapp.base.util.GenericData
import shipreq.base.util.UtilMacros._
import DataImplicits._

// TODO RandomData is inaccurate in that CorrectionParts aren't applied.

object RandomData {

  type StateG[S, A] = StateT[Gen, S, A]
  implicit def gliftS[S, A](g: Gen[A]): StateG[S, A] = StateT(s => g.map(a => (s,a)))

  def stateGen[S, A](g: S => Gen[A]): StateG[S, A] = StateT(s => g(s).map(a => (s,a)))

  implicit class CustomGenExt[A](val g: Gen[A]) extends AnyVal {
    def nev: GenS[NonEmptyVector[A]] = for {t <- g.vector; h <- g} yield NonEmptyVector(h, t)
    def nes(implicit ev: UnivEq[A]): GenS[NonEmptySet[A]] = for {t <- g.set; h <- g} yield NonEmptySet(h, t)
  }

  implicit class LGExt[A](val _l: List[Gen[A]]) extends AnyVal {
    def <+(o: Option[Gen[A]]): List[Gen[A]] = o.fold(_l)(_ :: _l)
  }
  implicit class NELGExt[A](val _nel: NonEmptyList[Gen[A]]) extends AnyVal {
    def <+(o: Option[Gen[A]]): NonEmptyList[Gen[A]] = o.fold(_nel)(_ <:: _nel)
    def <++(l: List[Gen[A]]) : NonEmptyList[Gen[A]] = l <::: _nel
  }

  // Nyaya's subset is too extreme. Maybe this should be the impl
  def subset2[A](as: TraversableOnce[A], ns: Int, zs: Int): Gen[Vector[A]] = {
    val n = Gen subset as
    val z = Gen insert Vector.empty[A]
    val g = Vector.fill(ns)(n) ++ Vector.fill(zs)(z)
    Gen.oneofG(g.head, g.tail: _*)
  }

  def genmodL[A, B](l: Lens[A, B])(g: B => Gen[B])(a: A): Gen[A] =
    g(l get a) map (l.set(_)(a))

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

  // JS + Parboiled2 no like \u001e
  val mkChar: Int => Char =
    ((_: Int).toChar) `JVM|JS` ((i: Int) => if (i == 30) ' ' else i.toChar)

  def asciiChar   : Gen[Char]   = Gen.chooseint(1, 255) map mkChar
  def asciiString : GenS[String] = asciiChar.string
  def asciiString1: GenS[String] = asciiChar.string1

  def unicodeChar   : Gen[Char]   = Gen.chooseint(0, 0xd7ff) map mkChar
  def unicodeString : GenS[String] = unicodeChar.string
  def unicodeString1: GenS[String] = unicodeChar.string1

//  private val _charPredAllChars = ('\u0001' to '\ud7ff').seq
  private val _charPredAllChars = ('\u0001' to '\u0100').seq
//  private val _charPredAllChars = ('\u0020' to '\u0100').seq
//  private val _charPredAllChars = ('\u0020' to '\u0080').seq
  def charPred(p: org.parboiled2.CharPredicate): Gen[Char] =
    Gen.oneofO(_charPredAllChars filter p.apply).get

  def oneofV[A](as: NonEmptyVector[A]): Gen[A] =
    Gen.oneof(as.head, as.tail: _*)

  def oneofS[A](as: NonEmptySet[A]): Gen[A] =
    oneofV(as.toNEV)

  def oneofVG[A](as: NonEmptyVector[Gen[A]]): Gen[A] =
    Gen.oneofG(as.head, as.tail: _*)

  def grammarChars(c: Grammar.Chars): Gen[Char] =
    Gen.charof(c.ch1, c.chn, c.rs: _*)

  def grammarStr1[G](g: G)(f: G => Grammar.Chars, w: G => Grammar.Chars, l: G => Grammar.Length): Gen[String] =
    for {
      h <- grammarChars(f(g))
      t <- grammarChars(w(g)).list.lim(l(g).minus1.max)
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

  def legalGrammar[G](g: G)(first: G => Grammar.Chars, rest: G => Grammar.Chars): Stream[String] = {
    val g1 = first(g).toStream.map(_.toString)
    val gn = rest(g).toStream.map(_.toString)
    def grow(ss: Stream[String]): Stream[String] = {
      val x = ss append ss.flatMap(s => gn.map(s + _))
      x append grow(x)
    }
    grow(g1)
  }

  def grammarFixer[G](g: G)(first: G => Grammar.Chars, rest: G => Grammar.Chars) = {
    val all = legalGrammar(g)(first, rest)
    def fix(used: Set[String]): String =
      all.filter(!used.contains(_)).head
    Distinct.Fixer.lift(fix)
  }

  def grammarFixerIgnoreCase[G](g: G)(first: G => Grammar.Chars, rest: G => Grammar.Chars) = {
    val all = legalGrammar(g)(first, rest) map CaseInsensitive
    def fix(used: Set[CaseInsensitive]): CaseInsensitive =
      all.filter(!used.contains(_)).head
    Distinct.Fixer.lift(fix).xmap(_.str)(CaseInsensitive)
  }

  def someOfWithDups[A, B](as: Seq[A])(f: A => Gen[B]): Gen[Vector[B]] =
    Gen.oneofO(as).fold[Gen[Vector[B]]](Gen insert Vector.empty)(
      _.vector.flatMap(Gen.traverse(_)(f)))

  lazy val id =
    Gen.int.map(i => if (i == 0) 1 else Math.abs(i))

  def shortText1 =
    unicodeString1.lim(AppConsts.shortTextMaxLength)

  def shortText =
    unicodeString.lim(AppConsts.shortTextMaxLength)

  lazy val optionalLargeText =
    shortText1.lim(AppConsts.largeTextMaxLength).option

  def revAndIMap[D, I <: TaggedInt](r: Gen[List[D]])
                                    (implicit i: DataIdAux[D, I], j: TestDataIdAux[D, I]): Gen[IMap[I, D]] = {
    val d = distinctId[D, I].lift[List]
    val g = d.run andThen (i.emptyIMap ++ _)
    r map g
  }

  def distinctId[D, I <: TaggedInt](implicit i: DataIdAux[D, I], j: TestDataIdAux[D, I]) =
    Distinct.fint.xmap(j.mkId)(_.value).distinct.contramap[D](i.id, j.setId)

  def isubset[A: UnivEq](g: Gen[NonEmptySet[A]]): Gen[ISubset[A]] = {
    Gen.oneofG(
      Gen insert ISubset.All(),
      g map ISubset.Only.apply,
      g map ISubset.Not.apply)
  }

  def imapToMapLens[K, V] = Lens((_: IMap[K, V]).underlyingMap)(v => _ replaceUnderlying v)

  val live =
    Gen.oneof[Live](Live, Dead)

  val implicationRequired =
    Gen.oneof[ImplicationRequired](ImplicationRequired, ImplicationRequired.Not)

  val mandatory =
    Gen.oneof[Mandatory](Mandatory, Mandatory.Not)

  val hashRefKey: Gen[HashRefKey] =
    grammarStr1(Grammar.hashRefKey)(_.firstChar, _.allChars, _.length) map HashRefKey

  val deletionAction =
    oneofV(DeletionAction.values)

  // -------------------------------------------------------------------------------------------------------------------
  // Custom issue types

  lazy val customIssueTypeId =
    id map CustomIssueTypeId

  lazy val customIssueType =
    Gen.apply4(CustomIssueType.apply)(customIssueTypeId, hashRefKey, optionalLargeText, live)

  /** HashRefKey uniqueness enforced in Project, not here */
  lazy val customIssueTypes =
    revAndIMap(customIssueType.list)

  // -------------------------------------------------------------------------------------------------------------------
  // ReqTypes

  lazy val reqTypeMnemonic =
    grammarStr1(Grammar.reqTypeMnemonic)(_.chars, _.chars, _.length) map ReqType.Mnemonic

  lazy val reqTypeMnemonicFixer =
    grammarFixer(Grammar.reqTypeMnemonic)(_.chars, _.chars)
      .xmap(ReqType.Mnemonic.apply)(_.value)
      .addhs(StaticReqType.mnemonics)

  lazy val customReqTypeId =
    id map CustomReqTypeId

  lazy val staticReqType: Gen[StaticReqType] =
    oneofV(StaticReqType.values)

  lazy val reqTypeId: Gen[ReqTypeId] =
    Gen.oneofG(staticReqType.subst, customReqTypeId.subst)

  def customReqTypeName =
    shortText1

  lazy val customReqType =
    for {
      id <- customReqTypeId
      n  <- customReqTypeName
      mn <- reqTypeMnemonic
      om <- reqTypeMnemonic.set.lim(16)
      ir <- implicationRequired
      a  <- live
    } yield CustomReqType(id, mn, om - mn, n, ir, a)

  def genCustomReqTypes(g: Gen[List[CustomReqType]]) = {
    def dname = Distinct.str.at(CustomReqType.name)
    def dmnemonic = {
      val distm = reqTypeMnemonicFixer.distinct
      val cur = distm.at(CustomReqType.mnemonic)
      val old = distm.lift[Set].at(CustomReqType.oldMnemonics)
      cur + old
    }
    val d = (dname * dmnemonic).lift[List]
    revAndIMap(g map d.run)
  }

  lazy val customReqTypes =
    genCustomReqTypes(customReqType.list)

  val staticReqTypeIdSet = StaticReqType.values.toNES[ReqTypeId]

  // -------------------------------------------------------------------------------------------------------------------
  // Tags

  lazy val tagGroupId =
    id map TagGroupId

  lazy val applicableTagId =
    id map ApplicableTagId

  lazy val tagId: Gen[TagId] = {
    import Gen.Covariance._
    Gen.oneofG(tagGroupId, applicableTagId)
  }

  lazy val mutexChildren =
    Gen.oneof[MutexChildren](MutexChildren, MutexChildren.Not)

  def tagName =
    shortText1

  lazy val tagGroup =
    Gen.apply5(TagGroup.apply)(tagGroupId, tagName, optionalLargeText, mutexChildren, live)

  lazy val applicableTag =
    Gen.apply5(ApplicableTag.apply)(applicableTagId, tagName, optionalLargeText, hashRefKey, live)

  lazy val tag = {
    val atag = applicableTag.subst[Tag]
    Gen.oneofG[Tag](tagGroup.subst, atag, atag, atag)
  }

  lazy val tagAndRels: Gen[(Tag, TagInTree.Relations)] =
    for {
      t      ← tag
      (p, c) ← tagId.set.pair
    } yield {
      val children = (c - t.id -- p).toVector
      val parents  = (p - t.id -- c).toStream.map(_ -> none[TagId]).toMap
      (t, MMTree.Relations(parents, children))
    }

  /** HashRefKey uniqueness enforced in Project, not here */
  lazy val tags: Gen[List[Tag]] = {
    val di = distinctId[Tag, TagId]
    val dn = Distinct.str.at(Tag.name)
    val d = (di * dn).lift[List]
    tag.list map d.run
  }

  type TagTreeStructure = Map[TagId, Vector[TagId]]

  @tailrec
  def preventTagTreeCycles(m: TagTreeStructure /*, i: Int = 0*/): TagTreeStructure =
    Tag.CycleDetectors.multimap.findCycle(m) match {
      case None     =>
        // println(s"No cycles after $i attempts @ size ${m.keyCount}→${m.valueCount}")
        m
      case Some((a, b)) =>
//        println(s"Found cycle #$i [$a→$b] in ${m.m}")
//        preventCycles(m.del(a, b).del(b, a), i + 1) // better but slowwwwww
        preventTagTreeCycles(m - b /*, i + 1*/)
    }

  def tagTreeStructure(tags: Set[TagId]): Gen[TagTreeStructure] =
    if (tags.isEmpty)
      Gen.insert(Map.empty)
    else {
      val tagsSeq = tags.toSeq
      val idset = Gen.oneof(tagsSeq.head, tagsSeq.tail: _*).set
      idset.map(_.toStream)
        .flatMap(ks => Gen sequence ks.map(k => idset.map(ids => (k, (ids - k).toVector)).sup))
        .map(s => preventTagTreeCycles(s.toMap))
    }

  lazy val tagTree: Gen[TagTree] =
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

  lazy val staticField: Gen[StaticField] =
    oneofV(StaticField.values)

  def applicableReqTypes(r: Set[CustomReqTypeId]): Gen[ApplicableReqTypes] = {
    val all = StaticReqType.values.foldLeft(r.map(a => a: ReqTypeId))(_ + _).toList
    val a = Gen.oneof(all.head, all.tail: _*)
    isubset(a.nes)
  }

  lazy val customFieldTextId =
    id map CustomField.Text.Id

  lazy val customFieldTagId =
    id map CustomField.Tag.Id

  lazy val customFieldImplicationId =
    id map CustomField.Implication.Id

  lazy val customFieldId: Gen[CustomFieldId] = {
    import Gen.Covariance._
    Gen.oneofG(customFieldTextId, customFieldTagId, customFieldImplicationId)
  }

  lazy val fieldRefKey =
    grammarStr1(Grammar.fieldRefKey)(_.firstChar, _.allChars, _.length) map FieldRefKey

  def customFieldType =
    oneofV(CustomFieldType.values)

  def customFieldText(art: Gen[ApplicableReqTypes]): Gen[CustomField.Text] =
    Gen.apply6(CustomField.Text.apply)(customFieldTextId, shortText1, fieldRefKey, mandatory, art, live)

  def customFieldTag(tagId: Gen[TagId], art: Gen[ApplicableReqTypes]): Gen[CustomField.Tag] =
    Gen.apply5(CustomField.Tag.apply)(customFieldTagId, tagId, mandatory, art, live)

  def customFieldTagSome(tagIds: Set[TagId], art: Gen[ApplicableReqTypes]): Gen[Vector[CustomField.Tag]] =
    Gen.subset(tagIds).flatMap(ids =>
      Gen sequence ids.map(id =>
        customFieldTag(Gen insert id, art)))

  def customFieldImplication(reqTypeId: Gen[ReqTypeId], art: Gen[ApplicableReqTypes]): Gen[CustomField.Implication] =
    Gen.apply5(CustomField.Implication.apply)(customFieldImplicationId, reqTypeId, mandatory, art, live)

  def customFieldImplicationSome(reqTypeIds: Set[ReqTypeId], art: Gen[ApplicableReqTypes]): Gen[Vector[CustomField.Implication]] =
    Gen.subset(reqTypeIds).flatMap(ids =>
      Gen sequence ids.map(id =>
        customFieldImplication(Gen insert id, art)))

  def customField(art: Gen[ApplicableReqTypes],
                  impFields: Boolean,
                  tagFields: Boolean): Gen[CustomField] = {
    import Gen.Covariance._
    lazy val txt: Gen[CustomField] = customFieldText(art)
    customFieldType.flatMap {
      case CustomFieldType.Text        => txt
      case CustomFieldType.Tag         => if (tagFields) customFieldTag(tagId, art) else txt
      case CustomFieldType.Implication => if (impFields) customFieldImplication(reqTypeId, art) else txt
    }
  }

  def customFields(reqTypeIds: Set[ReqTypeId], tagIds: Set[TagId], art: Gen[ApplicableReqTypes]): Gen[IMap[CustomFieldId, CustomField]] = {
    val cf = for {
      f1 <- customField(art, false, false).stream
      f2 <- customFieldTagSome(tagIds, art)
      f3 <- customFieldImplicationSome(reqTypeIds, art)
    } yield f3.toStream #::: f2.toStream #::: f1
    def id   = distinctId(CustomField.IdAccess, CustomFieldId_T)
    def name = Distinct.str.at(CustomField.independentName)
    def key  = Distinct.fstr.xmap(FieldRefKey.apply)(_.value).distinct.at(CustomField.key)
    val dist = (id * name * key).lift[Stream]
    cf.map(fs => emptyDataMap(CustomField) ++ dist.run(fs))
  }

  def fieldSet(reqTypeIds: Set[ReqTypeId], tagIds: Set[TagId], r: Set[CustomReqTypeId]): Gen[FieldSet] =
    for {
      cf           ← customFields(reqTypeIds, tagIds, applicableReqTypes(r))
      mandatoryIds = cf.keySet.map(f => f: FieldId) ++ StaticField.notDeletable
      optionalIds  ← Gen.oneof(StaticField.deletable.head, StaticField.deletable.tail: _*).set
      order        ← Gen.shuffle((mandatoryIds ++ optionalIds).toVector)
    } yield FieldSet(cf, order)

  def fieldSet2(reqTypeIds: Set[ReqTypeId], tagIds: Set[TagId], cReqTypeIds: Set[CustomReqTypeId]): GenS[FieldSet] =
    GenS { sz =>
      def subset[A](as: TraversableOnce[A]): Gen[Set[A]] =
        Gen.subset(as).map(_.take(sz.value).toSet)
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
    import shipreq.webapp.base.text._
    import Atom._
    import Text.{ReqTitle => _, _}
    import Gen.Covariance._

    lazy val webAddressR = charPred(Parsers.webAddressChar).string1
    lazy val emailL = charPred(Parsers.emailCharL).string1
    lazy val emailR = charPred(Parsers.emailCharR).string1

    val strchr  = Gen.oneofG(Gen.chooseint(32, 127), Gen.chooseint(128, 0xd7ff)).map(_.toChar)
//    val strchr = unicodeChar
    val genstr  = strchr.string
    val genstr1 = strchr.string1

    def literal(implicit t: Literal): Gen[t.Literal] =
      genstr1.map(t.Literal)

    def blankLine(implicit t: NewLine): Gen[t.BlankLine] =
      Gen.insert(t.blankLine)

    def listItem[T <: ListMarkup](g: Name[Gen[T#Atom]]): Gen[T#ListItem] =
      Gen.insert(g).flatMap(_.value).vector.lim(MaxTextAtoms)

    def listItems[T <: ListMarkup](g: Name[Gen[T#Atom]]): Gen[NonEmptyVector[T#ListItem]] =
      listItem(g).nev.lim(20)

    def unorderedList(t: ListMarkup)(g: Name[Gen[t.Atom]]): Gen[t.UnorderedList] =
      listItems(g) map t.UnorderedList

    def webAddress(implicit t: PlainTextMarkup): Gen[t.WebAddress] =
      for {
        a <- Gen.oneof("http", "https", "ftp", "ftps", "sftp")
        b <- webAddressR
      } yield t.WebAddress(a + "://" + b)

    def emailAddress(implicit t: PlainTextMarkup): Gen[t.EmailAddress] =
      for {
        l <- emailL
        ra <- emailR
        rb <- emailR.list1.lim(5)
      } yield t.EmailAddress(l + "@" + (ra :: rb.list).mkString("."))

    def mathTex(implicit t: PlainTextMarkup): Gen[t.MathTeX] =
      genstr1.map(_.replace("</math>", "x") |> noWhitespaceLeft |> noWhitespaceRight |> t.MathTeX)

    def plainTextMarkup(implicit t: PlainTextMarkup): Gen[t.Atom] =
      Gen.oneofG(webAddress, emailAddress, mathTex)

    private[this] def singleLineGens(implicit t: SingleLine): NonEmptyList[Gen[t.Atom]] =
      NonEmptyList(literal, plainTextMarkup)

    /** Probability [0,9] of an increase in recursive depth. */
    val DepthIncrease: Array[Int] = Array(5, 1, 1, 1) `JVM|JS` Array(3, 1)

    private[this] def multiLine(t: MultiLine, depth: Int)(g: Name[Gen[t.Atom]]): NonEmptyList[(Int, Gen[t.Atom])] = {
      type G  = Gen[t.Atom]
      type IG = (Int, G)
      var gs = singleLineGens(t).map[IG]((9, _)) :::> List[IG](
                 (9, blankLine(t)))
      if (depth < DepthIncrease.length)
        gs = (DepthIncrease(depth), unorderedList(t)(g): G) <:: gs
      gs
    }

    private[this] def multiLinePlusI(t: MultiLine)(plus: (Int, Gen[t.Atom])*): Gen[t.Atom] = {
      type G  = Gen[t.Atom]
      type IG = (Int, G)

      val plusL = plus.toList

      lazy val lvls: Vector[Need[G]] =
        (0 to DepthIncrease.length)
          .toVector
          .map(i => Need[G](
            Gen.frequencyL(
              multiLine(t, i)(Name(lvls(i + 1).value)) :::> plusL
        )))

      lvls(0).value
    }

    private[this] def multiLinePlus(t: MultiLine)(plus: Option[Gen[t.Atom]]*): Gen[t.Atom] =
      multiLinePlusI(t)(
        plus.foldLeft[List[(Int, Gen[t.Atom])]](Nil)((q, o) =>
          o.fold(q)(g => (9, g) :: q)): _*)

    def reqRefs(r: Option[Gen[ReqId]], c: Option[Gen[ReqCodeId]])(implicit t: ReqRef): List[Gen[t.Atom]] = {
      import Gen.Covariance._
      var v = List.empty[Gen[t.Atom]]
      v = v <+ r.map(_ map t.ReqRef)
      v = v <+ c.map(_ map t.CodeRef)
      v
    }

    def tagRef(g: Gen[ApplicableTagId])(implicit t: TagRef): Gen[t.TagRef] =
      g map t.TagRef

    def issue(i: Gen[CustomIssueTypeId], r: Option[Gen[ReqId]], c: Option[Gen[ReqCodeId]])(implicit t: Issue): Gen[t.Issue] =
      Gen.apply2(t.Issue)(i, inlineIssueDescAtom(r, c).vector)

    val isBlankLine: AnyAtom => Boolean = {
      case _: NewLine#BlankLine => true
      case _ => false
    }

    def trimBlankLines[T <: Atom.Base](as: Vector[T#Atom]): Vector[T#Atom] =
      dropLast(dropHead(as)(isBlankLine))(isBlankLine)

    val legalListItemAtom: AnyAtom => Boolean = {
      case _: Literal         # Literal
         | _: ReqRef          # ReqRef
         | _: ReqRef          # CodeRef
         | _: Issue           # Issue
         | _: PlainTextMarkup # WebAddress
         | _: PlainTextMarkup # EmailAddress
         | _: PlainTextMarkup # MathTeX
         | _: TagRef          # TagRef        => true
      case _: NewLine         # BlankLine
         | _: ListMarkup      # UnorderedList => false
    }

    sealed trait AtomCtx
    case object InIssueDesc  extends AtomCtx
    case object InListItem   extends AtomCtx
    case object TopLevelAtom extends AtomCtx

    def noWhitespaceLeft(a: String) = "l" + a
    def noWhitespaceRight(a: String) = a + "r"

    val removeFromLiteralsR = {
      val reqRefInside = """(?:[a-zA-Z]+\s*(?:-\s*)?\d+)"""
      val codeNode = """(?:[a-zA-Z0-9][a-zA-Z0-9_]*)"""
      val codeRefInside = s"(?:$codeNode(?:\\.$codeNode)*)"
      ("""[#@]+|[a-z]://|\*( )|<math>|\[\s*(?:""" + s"$reqRefInside|$codeRefInside)\\s*\\]").r
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
          case (x: Blank           , _: Blank           ) => drop
          case (x: Blank           , _: UL              ) => drop

          case (x: PTM#EmailAddress, y: Lit             ) => i :+ x :+ y.map(" " + _)
          case (x: PTM#EmailAddress, _: PTM#EmailAddress) => drop
          case (x: PTM#EmailAddress, _: PTM#WebAddress  ) => drop

          case (x: PTM#WebAddress  , y: Lit             ) => i :+ x :+ y.map(" " + _)
          case (x: PTM#WebAddress  , _: PTM#EmailAddress) => drop
          case (x: PTM#WebAddress  , _: PTM#WebAddress  ) => drop
          case (x: PTM#WebAddress  , _: Issue#Issue     ) => drop
          case (x: PTM#WebAddress  , _: TagRef#TagRef   ) => drop

          case (x: TagRef#TagRef   , y: Lit             ) => i :+ x :+ y.map(" " + _)
          case (x: TagRef#TagRef   , _: PTM#EmailAddress) => drop
          case (x: TagRef#TagRef   , _: PTM#WebAddress  ) => drop

          case (x: Issue#Issue     , y: Lit             ) if x.desc.isEmpty => i :+ x :+ y.map(" i" + _)
          case (x: Issue#Issue     , _: PTM#EmailAddress) if x.desc.isEmpty => drop
          case (x: Issue#Issue     , _: PTM#WebAddress  ) if x.desc.isEmpty => drop

          case (x: UL              , y: Lit             ) => i :+ x :+ y.map(noWhitespaceLeft)
          case (x: UL              , _: Blank           ) => drop
          case (x: UL              , y: UL              ) => drop //.copy(items = x.items ++ y.items)

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
                              c: Option[Gen[ReqCodeId]],
                              i: Option[Gen[CustomIssueTypeId]],
                              a: Option[Gen[ApplicableTagId]]): Gen[t.Atom] = {
      @inline implicit def tt: t.type = t
      val x = singleLineGens(t)
      val gs = (x append x) <++ reqRefs(r, c) <+ a.map(tagRef(_)) <+ i.map(issue(_, r, c))
      Gen oneofGL gs
    }

    def reqCodeGroupTitleAtom(r: Option[Gen[ReqId]],
                              c: Option[Gen[ReqCodeId]],
                              i: Option[Gen[CustomIssueTypeId]]): Gen[ReqCodeGroupTitle.Atom] = {
      @inline implicit def t: ReqCodeGroupTitle.type = ReqCodeGroupTitle
      val x = singleLineGens(t)
      val gs = (x append x) <++ reqRefs(r, c) <+ i.map(issue(_, r, c))
      Gen oneofGL gs
    }

    def genericReqTitleAtom   = reqTitle(GenericReqTitle) _

    def inlineIssueDescAtom(r: Option[Gen[ReqId]], c: Option[Gen[ReqCodeId]]): Gen[InlineIssueDesc.Atom] = {
      @inline implicit def t: InlineIssueDesc.type = InlineIssueDesc
      val gs = singleLineGens(t) <++ reqRefs(r, c)
      Gen oneofGL gs
    }

    def customTextFieldAtom(r: Option[Gen[ReqId]],
                            c: Option[Gen[ReqCodeId]],
                            i: Option[Gen[CustomIssueTypeId]],
                            a: Option[Gen[ApplicableTagId]]): Gen[CustomTextField.Atom] = {
      implicit val t: CustomTextField.type = CustomTextField
      val gs: List[Option[Gen[t.Atom]]] =
        i.map(issue(_, r, c).subst[t.Atom]) :: a.map(tagRef(_).subst[t.Atom]) :: reqRefs(r, c).map(_.some)
      multiLinePlus(t)(gs: _*)
    }
  }

  val MaxTextAtoms = 30 `JVM|JS` 8

  val MaxTextAtomsInProject = 6 `JVM|JS` 2

  implicit class TextGenExt[T <: text.Atom.Literal](val g: Gen[T#Atom]) extends AnyVal {
    def text       : GenS[T#OptionalText] = g.vector lim MaxTextAtoms map TextGen.postProcessAtoms(TextGen.TopLevelAtom)
    def text1(t: T): GenS[T#NonEmptyText] = g.nev    lim MaxTextAtoms map TextGen.postProcessAtoms1(t)

    def ptext       : GenS[T#OptionalText] = g.vector lim MaxTextAtomsInProject map TextGen.postProcessAtoms(TextGen.TopLevelAtom)
    def ptext1(t: T): GenS[T#NonEmptyText] = g.nev    lim MaxTextAtomsInProject map TextGen.postProcessAtoms1(t)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Requirements

  lazy val genericReqId =
    id map GenericReqId

  lazy val reqId: Gen[ReqId] = {
    import Gen.Covariance._
    Gen.oneofG(genericReqId)
  }

  def sAllocPubidC(possibleReqTypeIds: NonEmptyVector[CustomReqTypeId])(reqId: ReqIdC): StateG[PubidRegister, PubidC] =
    StateT(register =>
      oneofV(possibleReqTypeIds).map(reqTypeId =>
        register.allocC(reqTypeId)(reqId)))

  def sGenericReqId(pubidS: ReqIdC => StateG[PubidRegister, PubidC]): StateG[PubidRegister, ReqIdC] =
    for {
      id <- genericReqId |> gliftS[PubidRegister, GenericReqId]
      _  <- pubidS(id)
    } yield id

  def sGenericReq(pubidS: ReqIdC => StateG[PubidRegister, PubidC]): StateG[PubidRegister, GenericReq] =
    for {
      id     ← genericReqId |> gliftS[PubidRegister, GenericReqId]
      pubid  ← pubidS(id)
      desc   = Vector.empty
      l      ← live
    } yield
      GenericReq(id, pubid, desc, l)

  def pubidRegisterAnd[A, B](reqCount: Int, inita: A, genb: StateG[PubidRegister, B])(f: (A, B) => A): Gen[(PubidRegister, A)] = {
    val init = StateT.stateT[Gen, PubidRegister, A](inita)
    val prog = Stream.fill(reqCount)(genb).foldLeft(init)((sn, ga) =>
      for {
        b <- sn
        a <- ga
      } yield f(b, a)
    )
    prog(PubidRegister.empty)
  }

  def pubidRegisterAndIds(reqCount: Int, customReqTypeIds: NonEmptyVector[CustomReqTypeId]): Gen[(PubidRegister, Set[ReqIdC])] =
    pubidRegisterAnd(reqCount, Set.empty[ReqIdC], sGenericReqId(sAllocPubidC(customReqTypeIds)))(_ + _)

  def requirements(reqCount: Int, customReqTypeIds: Vector[CustomReqTypeId]): Gen[Requirements] =
    NonEmptyVector.maybe(customReqTypeIds,
      Gen insert Requirements.empty)( // ← This will change when UseCases are added
      customReqTypeIdNev =>
        pubidRegisterAnd(reqCount, emptyDataMap(GenericReq), sGenericReq(sAllocPubidC(customReqTypeIdNev)))(_ + _)
          .map { case (pr, reqs) => Requirements(reqs, pr) }
      )

  def reqsWithoutText(reqCount: Int, cfg: ProjectConfig): Gen[Requirements] =
    requirements(reqCount, cfg.customReqTypes.keys.toVector)

//  /**
//   * I mistakenly thought reqsWithoutText was really slow, so I wrote this faster replacement.
//   *
//   * reqsWithoutText() is fast enough & simpler. Disabling this.
//   */
//  def reqsWithoutText_Quick(reqCount: Int, cfg: ProjectConfig): Gen[Requirements] = {
//    val reqTypeIdsV    = cfg.customReqTypes.data.keys.toVector
//    val deadReqtypeIds = cfg.customReqTypes.data.values.toStream.filter(_.live :: Dead).map(_.id)
//
//    type A = Vector[(CustomReqTypeId, Int)]
//
//    val scale: EndoFn[A] = v => {
//      val sum = v.foldLeft(0)(_ + _._2)
//      val c   = reqCount.toDouble / sum.toDouble
//      val v2  = v.map(_.map2(n => (c * n).toInt))
//      val off = reqCount - v2.foldLeft(0)(_ + _._2)
//      if (off != 0 && v2.nonEmpty)
//        v2.updated(0, v2(0).map2(_ + off))
//      else
//        v2
//    }
//
//    val chooseQty = Gen.chooseint(0, reqCount)
//    val qtyPerReqType = Gen.sequence(reqTypeIdsV.map(id => chooseQty.map((id, _)))) map scale
//
//    val idVector = qtyPerReqType flatMap { a =>
//      val ids = a.foldLeft(Vector.empty[CustomReqTypeId])((q, t) => q ++ Vector.fill(t._2)(t._1))
//      assert(ids.length == reqCount, s"Expect ${reqCount} ids, got: ${ids.length}")
//      Gen.shuffle(ids)
//    }
//
//    val justDead: Gen[Live] = Gen insert Dead
//    val genLive: Gen[Live] = Gen.oneof[Live](Live, Live, Live, Live, Live, Dead)
//    val rtLive: ReqTypeId => Live = id => Dead <~ (deadReqtypeIds contains id)
//
//    type SF = EndoFn[(PubidRegister, Requirements.ById)]
//    def mksf(sf: SF): SF = sf
//
//    def genOne(id: GenericReqId, reqTypeId: CustomReqTypeId): Gen[SF] =
//      for {
//        live <- if (rtLive(reqTypeId) :: Dead) justDead else genLive
//      } yield mksf {state =>
//        val (pr0, rs0)  = state
//        val (pr, pubid) = pr0.allocC(reqTypeId)(id)
//        val req = GenericReq(id, pubid, Vector.empty, live)
//        val rs = rs0 + req
//        (pr, rs)
//      }
//
//    val __gsf =
//    genericReqId.flatMap { startId =>
//      idVector.flatMap { reqTypeVec =>
//
//        val _gSF: Gen[SF] =
//        (0 until reqCount).foldLeft(Gen.insert(identity: SF)) { (sf1, i) =>
//
//            val id        = GenericReqId(startId.value + i)
//            val reqTypeId = reqTypeVec(i)
//
//            val sf2 = genOne(id, reqTypeId)
//            val sf3 = for {a <- sf1; b <- sf2} yield b compose a
//            sf3
//          }
//
//        _gSF
//      }
//    }
//
//    __gsf.map { sf =>
//      val (pr, rs) = sf(PubidRegister.empty, Requirements.emptyById)
//      Requirements(rs, pr)
//    }
//  }

  def updateRequirementText(gt: Gen[Text.GenericReqTitle.OptionalText])(data: GenericReqIMap): Gen[GenericReqIMap] = {
    val streamOfGens = data.vstream {
        case v: GenericReq => gt.map(t => v.copy(title = t))
      }
    val genStream = Gen.sequence(streamOfGens)
    genStream.map(emptyDataMap(GenericReq) ++ _)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Req Data

  def reqFieldDataText(cols: Set[CustomField.Text.Id], reqs: Set[ReqId], txt: Gen[Text.CustomTextField.NonEmptyText]): Gen[ReqData.Text] =
    txt mapByKeySubset reqs mapByKeySubset cols

  def reqFieldDataTags(reqs: TraversableOnce[ReqId], tags: Set[ApplicableTagId]): Gen[ReqData.Tags] = {
    val rndTags = subset2(tags, 1, 6).map(_.toSet)
    (rndTags mapByKeySubset reqs).map(Multimap(_))
//    subset2(reqs, 1, 0).flatMap(rndTags.mapByEachKey).map(Multimap(_))
  }

  type ImplicationsUM = Map[ReqId, Set[ReqId]]
  @tailrec def preventImplicationCycles(m: ImplicationsUM): ImplicationsUM =
    Implications.cycleDetector.findCycle(m) match {
      case None         => m
      case Some((a, b)) => preventImplicationCycles(m - b)
    }

  val emptyImplicationsU = Implications.emptyUni

  val MaxImplicationPairs = 100 `JVM|JS` 40
  // val MaxImplicationsPerSrc = 2  `JVM|JS` 4
  // val MaxImplicationKeys    = 10 `JVM|JS` 4

  type ImpMethod = (Gen[ReqId], ImplicationsUM => Implications) => Gen[Implications]

  val implicationsMethodDefault: ImpMethod = (g, fix) =>
    g.pair
      .list.lim(MaxImplicationPairs)
      .map(kvs => emptyImplicationsU.addPairs(kvs: _*).m |> fix)

  def implicationsMethod2(maxImpsPerSrc: Int, maxImpKeys: Int): ImpMethod = (g, fix) =>
    Gen.tuple2(g, g.set1 lim maxImpsPerSrc)
      .list.lim(maxImpKeys)
      .map(_.toMap |> fix)

  def reqFieldDataImplications(reqIds: Set[ReqId], method: ImpMethod = implicationsMethodDefault): Gen[Implications] = {
    def fix(m: ImplicationsUM): Implications = {
      val m2 = preventImplicationCycles(m)
      // println(m2); println()
      Implications(Multimap(m2))
    }

    Gen.oneofO(reqIds.toSeq) match {
      case Some(g) => method(g, fix)
      case None    => Gen insert Implications(emptyImplicationsU)
    }
  }

//  def customTextFieldText(reqIdG  : Option[Gen[ReqId]],
//                          reqCodeG: Option[Gen[ReqCodeId]],
//                          cissueG : Option[Gen[CustomIssueTypeId]],
//                          tagG    : Option[Gen[ApplicableTagId]]) = {
//    TextGen.customTextFieldAtom(reqIdG, reqCodeG, cissueG, tagG).ptext1(Text.CustomTextField)
//  }

  def reqFieldDataText2(reqs    : Set[ReqId],
                        txtCols : Set[CustomField.Text.Id],
                        reqCodeG: Option[Gen[ReqCodeId]],
                        cissueG : Option[Gen[CustomIssueTypeId]],
                        tagG    : Option[Gen[ApplicableTagId]]) = {
    val gr = Gen.oneofO(reqs.toSeq)
    reqFieldDataText(txtCols, reqs, TextGen.customTextFieldAtom(gr, reqCodeG, cissueG, tagG).ptext1(Text.CustomTextField))
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Req Codes

  object reqCode {
    import ReqCode._

    val node: Gen[Node] =
      grammarStr1(Grammar.reqCode)(_.firstChar, _.allChars, _.nodeLength) map Node.applyFn

    lazy val value: GenS[Value] =
      node.nev

    lazy val id =
      RandomData.id map ReqCodeId

    val distinctIds =
      Distinct.fint.xmap(ReqCodeId)(_.value).distinct

    val reqCodeTrieFixK = Trie.fixk
    val reqCodeTrieValueTraversal: Traversal[Trie, Data] =
      PTraversal.fromTraverse[reqCodeTrieFixK.Trie, Data, Data](reqCodeTrieFixK.traverseTrie)

    val distinctReqCodeTrie = {
      val dataActive = Data.active ^<-? atSome

      val ids1 = distinctIds at ActiveData.id at dataActive
      val ids2 = distinctIds.lift[Set] at Data.refsToGroup
      val ids3 = distinctIds.lift[Set]
        .liftMapValues.contramap[Multimap[ReqId, Set, ReqCodeId]](_.m, (_, m: Map[ReqId, Set[ReqCodeId]]) => Multimap(m))
        // TODO ↑ Use liftMultimapValues when Nyaya gets it
        .at(Data.refsToReqs)
      val id = ids1 + ids2 + ids3

      // TODO add to Nyaya ↓
      val idsInTrie =
        id.dimaps[Trie, Trie](t => d_sd => State { h0 =>
          var h = h0
          val t2 =
            reqCodeTrieValueTraversal.modify({ data =>
              val (h2, d2) = d_sd(data).run(h)
              h = h2
              d2
            })(t)
          (h, t2)
        })

      idsInTrie
    }

    val smallIdSet = id.set.lim(3)

    val gEmptyRefsToReqs: Gen[Multimap[ReqId, Set, ReqCodeId]] =
      Gen.insert(Multimap.empty)

    def data(ogLiveReqId: Option[Gen[ReqId]], ogReqId: Option[Gen[ReqId]], gGroup: Gen[ReqCodeGroup]): GenS[Data] =
      GenS { sz =>
        import Gen.Covariance._

        val gTarget: Gen[Target] =
          ogLiveReqId match {
            case Some(g) => Gen.oneofG(g, g, g, g, gGroup)
            case None    => gGroup
          }

        val gRefsToReqs: Gen[Multimap[ReqId, Set, ReqCodeId]] =
          ogReqId match {
            case Some(g) => g.mapTo(smallIdSet).lim(sz.value).map(Multimap(_))
            case None    => gEmptyRefsToReqs
          }

        for {
          i           <- id
          target      <- gTarget
          refsToGroup <- smallIdSet
          refsToReqs  <- gRefsToReqs
          x           <- Gen.chooseint(0, 9)
        } yield {
          if (x == 0)
            target match {
              case t: ReqId        => Data(None, refsToGroup, refsToReqs.add(t, i))
              case _: ReqCodeGroup => Data(None, refsToGroup + i, refsToReqs)
            }
          else
            Data(Some(ActiveData(i, target)), refsToGroup, refsToReqs)
        }
    }

    def trieValue(d: Gen[Data]): Gen[Trie.Value] = d map Trie.Value

    val emptyTrie: Gen[Trie] = Gen insert Trie.empty

    def trie(maxDepth: Int, d: Gen[Data]): Gen[Trie] = {
      import ReqCode.Trie._
      val value    = trieValue(d)
      val valueO   = value.option
      val valueN   = value.subst[Node]
      val midDepth = maxDepth / 2

      def level(depth: Int): Gen[Trie] =
        if (depth <= 1)
          emptyTrie
        else {
          val branch  = Gen.apply2(Branch)(valueO, level(depth - 1))
          val branchN = branch.flatMap[Node](b => if (b.next.nonEmpty) Gen.insert(b) else b.value.fold(valueN)(Gen.insert))
          val node    = Gen.oneofG(branchN, valueN, if (depth > midDepth) branchN else valueN)
          node.mapBy(reqCode.node)
        }

      level(maxDepth) map distinctReqCodeTrie.run
    }

    val emptyReqCodeGroup = ReqCodeGroup(Vector.empty)
    val gEmptyReqCodeGroup = Gen insert emptyReqCodeGroup

    val activeGroup = Data.active ^<-? atSome ^|-> ActiveData.target ^<-? Target.reqCodeGroup

    def updateGroupText(gt: Gen[Text.ReqCodeGroupTitle.OptionalText])(src: Trie): Gen[Trie] = {
      type F = EndoFn[Trie]
      type G = Gen[F]
//      val vecOfGens = src.cataV(Vector.empty[G])((q, p, d) =>
//        d.active.fold(q)(a => a.target match {
//          case _: GenericReqId => q
//          case _: ReqCodeGroup => q :+ gt.map[F](t => _.put(p, d.copy(active = Some(a.copy(target = ReqCodeGroup(t))))))
//        }
//      ))
      val vecOfGens = src.cataV(Vector.empty[G])((q, p, d) =>
        activeGroup.getOption(d).fold(q)(_ =>
          q :+ gt.map[F](t => _.put(p, activeGroup.set(ReqCodeGroup(t))(d)))))
      val genVec = Gen.sequence(vecOfGens)
      genVec.map(_.foldLeft(src)((q, f) => f(q)))
    }
  }

  def reqCodes(g: Gen[ReqCode.Trie]) =
    g map ReqCodes.apply

  // -------------------------------------------------------------------------------------------------------------------
  // Project

  lazy val hashRefFixer =
    grammarFixerIgnoreCase(Grammar.hashRefKey)(_.firstChar, _.allChars)
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

  lazy val projectConfig: Gen[ProjectConfig] =
    for {
      (issues, tags) ← Gen.tuple2(customIssueTypes, tagTree) map distinctHashRefKeys.run
      reqtypes       ← customReqTypes
      reqTypeIds     = StaticReqType.values ++ reqtypes.keys
      reqTypeIdSet   = reqTypeIds.whole.toSet
      fields         ← fieldSet(reqTypeIdSet, tags.keySet, reqtypes.keySet)
    } yield ProjectConfig(issues, reqtypes, fields, tags)

  def genProject(cfg            : ProjectConfig,
                 reqsWithoutText: Requirements,
                 reqCodes1      : ReqCodes,
                 reqTags        : ReqData.Tags,
                 reqImps        : Implications): Gen[Project] = {
    val cissueIds      = cfg.customIssueTypes.keySet
    val cissueIdG      = Gen oneofO cissueIds.toSeq
    val reqIds         = reqsWithoutText.reqs.keys
    val reqIdG         = Gen oneofO reqIds.toSeq
    val reqIdSet       = reqIds.toSet
    val activeCodeIds  = reqCodes1.cataA(Vector.empty[ReqCodeId])((q, _, a) => q :+ a.id)
    val activeCodeIdG  = Gen oneofO activeCodeIds
    val atagIds        = cfg.tags.vstream(_.tag).filterT[ApplicableTag].map(_.id).toSet
    val atagIdG        = Gen.oneofO(atagIds.toSeq)
    val textColIds     = cfg.fields.customFields.values.filterT[CustomField.Text].map(_.id).toSet
    for {
      reqText        ← reqFieldDataText2(reqIdSet, textColIds, activeCodeIdG, cissueIdG, atagIdG)
      updReqText     = updateRequirementText(TextGen.genericReqTitleAtom(reqIdG, activeCodeIdG, cissueIdG, atagIdG).text) _
      reqs           ← genmodL(Requirements.genericReqs)(updReqText)(reqsWithoutText)
      reqCodes2      ← reqCode.updateGroupText(TextGen.reqCodeGroupTitleAtom(reqIdG, activeCodeIdG, cissueIdG).text)(reqCodes1.trie)
    } yield IdCeilings.supply(Project(cfg, reqs, ReqCodes(reqCodes2), reqText, reqTags, reqImps, _))
  }

  lazy val project: Gen[Project] =
    for {
      cfg             ← projectConfig
      atagIds         = cfg.tags.vstream(_.tag).filterT[ApplicableTag].map(_.id).toSet
      reqCount        ← GenS.choosesize.sup
      reqsWithoutText ← reqsWithoutText(reqCount, cfg)
      reqIds          = reqsWithoutText.reqs.keys
      reqIdG          = Gen oneofO reqIds.toSeq
      reqIdSet        = reqIds.toSet
      liveReqIds      = reqsWithoutText.reqs.values.toStream.filter(_.live(cfg.customReqTypes) :: Live).map(_.id)
      liveReqIdG      = Gen oneofO liveReqIds
      reqCodeDataG    = reqCode.data(liveReqIdG, reqIdG, reqCode.gEmptyReqCodeGroup).lim(3 `JVM|JS` 2)
      reqCodes        ← reqCodes(reqCode.trie(2 `JVM|JS` 2, reqCodeDataG))
      reqTags         ← reqFieldDataTags(reqIdSet, atagIds)
      reqImps         ← reqFieldDataImplications(reqIdSet)
      p               ← genProject(cfg, reqsWithoutText, reqCodes, reqTags, reqImps)
    } yield p

  // ===================================================================================================================
  // Protocol
  object protocol {
    import shipreq.webapp.base.protocol._
    import Gen.Covariance._

    lazy val reqTypeId: Gen[ReqTypeId] =
      Gen.oneofG(customReqTypeId, staticReqType)

    lazy val fieldId: Gen[FieldId] =
      Gen.oneofG(customFieldId, staticField)

    lazy val applicableReqTypes: Gen[ApplicableReqTypes] =
      isubset(reqTypeId.nes)

    lazy val fieldPosition: Gen[FieldCrud.Position] =
      fieldId.option

    lazy val textFieldValues =
      Gen.apply4(FieldCrud.TextFieldValues.apply)(shortText1, fieldRefKey, mandatory, applicableReqTypes)

    lazy val fieldValues: Gen[FieldCrud.Values] =
      Gen.oneofG(textFieldValues)

    object fieldCfgAction {
      import FieldCrud.CfgAction, CfgAction._
      lazy val create      : Gen[Create]       = fieldValues map Create
      lazy val updateValues: Gen[UpdateValues] = Gen.apply2(UpdateValues)(customFieldId, fieldValues)
      lazy val updateOrder : Gen[UpdateOrder]  = Gen.apply2(UpdateOrder)(fieldId, fieldPosition)
      lazy val delete      : Gen[Delete]       = Gen.apply2(Delete)(fieldId, deletionAction)
      lazy val any         : Gen[CfgAction]    = Gen.oneofG(create, updateValues, updateOrder, delete)
    }

    def tagProtocolValues: Tag => TagCrud.Values = {
      case TagGroup(_, n, d, mc, _)     => TagCrud.TagGroupValues(n, mc, d)
      case ApplicableTag(_, n, d, k, _) => TagCrud.ApplicableTagValues(n, k, d)
    }

    lazy val tagCrudInput =
      tagAndRels.flatMap(t => {
        val a = Gen insert tagProtocolValues(t._1)
        val b = Gen insert t._2
        a \&/ b
      })
  }

  // ===================================================================================================================
  object routines {
    import shipreq.webapp.base.protocol._
    import RemoteFn._
    import RandomData.protocol._

    lazy val remoteFnKey =
      Gen.alphanumericstring1

    def remoteFn(f: RemoteFn) =
      remoteFnKey.map(RemoteFn.Instance(_, f))

    lazy val projectSPA =
      Gen.apply9(ProjectSPA)(
        remoteFn(ProjectInit),
        remoteFn(CustomIssueTypeCrud),
        remoteFn(CustomReqTypeCrud),
        remoteFn(ReqTypeImplicationMod),
        remoteFn(FieldMandatorinessMod),
        remoteFn(FieldCrud.Fn),
        remoteFn(TagCrud.Fn),
        remoteFn(CreateContentFn),
        remoteFn(UpdateContentFn))

    class CrudActionGens[I, V](c: CrudFn.Aux[I, V])(idG: Gen[I], vG: Gen[V]) {
      import Gen.Covariance._
      lazy val create = vG.map(CrudAction.Create[I, V])
      lazy val update = Gen.apply2(CrudAction.Update[I, V])(idG, vG)
      lazy val delete = Gen.apply2(CrudAction.Delete[I, V])(idG, deletionAction)
      lazy val any    = Gen.oneofG[CrudAction[I, V]](create, update, delete)
    }

    lazy val customIssueTypeCrud = new CrudActionGens(CustomIssueTypeCrud)(
      RandomData.customIssueTypeId,
      Gen.tuple2(hashRefKey, optionalLargeText))

    lazy val customReqTypeCrud = new CrudActionGens(CustomReqTypeCrud)(
      RandomData.customReqTypeId,
      Gen.tuple3(reqTypeMnemonic, customReqTypeName, implicationRequired))

    lazy val tagCrud =
      new CrudActionGens(TagCrud.Fn)(RandomData.tagId, tagCrudInput)
  }

  // ===================================================================================================================
  object filter {
    import shipreq.webapp.base.filter._

    val quotedText =
      for {
        q <- Gen.oneof('\'', '"', '`')
        s <- unicodeString1
      } yield FilterSpec.QuotedText(s.replace(q, '_'), q)

    private val illegalSimpleTextStart = "/-#(){}'`\"".toCharArray.toSet
    private def fixSimpleText(s: String): String =
      if (s.headOption exists illegalSimpleTextStart.contains)
        "!" + s
      else if (Validators.reqType.mnemonicU isValidU s)
        s + "?"
      else
        s

    /** An odd number of backslashes cannot precede a slash */
    private val fixSlashEscaping = """(^|[^\\])(?:\\(?:\\\\)*)/""".r

    private def fixRegex(s: String): String =
      if (s endsWith "\\")
        s + "d"
      else
        fixSlashEscaping.replaceAllIn(s, "$1/")

    val simpleText =
      charPred(FilterParser.simpleTextChar).string1
        .map(s => FilterSpec.SimpleText(fixSimpleText(s)))

    val regex =
      unicodeString1.map(s => FilterSpec.Regex(fixRegex(s)))

    // -----------------------------------------------------------------------------------------------------------------
    object spec {
      import FilterSpec._

      val wholeType =
        reqTypeMnemonic map WholeType

      val someOfType =
        Gen.apply2(SomeOfType)(reqTypeMnemonic, Gen.chooseint(1,10000).nes.lim(20 `JVM|JS` 6))

      val reqsSpec: Gen[ReqsSpec] = {
        import Gen.Covariance._
        Gen.oneofG(wholeType, someOfType)
      }

      val reqs: Gen[Reqs] =
        reqsSpec.nev.lim(8)

      val attr: Gen[String] =
        charPred(FilterParser.attrChar).string1

      val reqType    = reqTypeMnemonic map ReqType
      val hashRef    = hashRefKey map HashRef
      val implies    = reqs map Implies
      val impliedBy  = reqs map ImpliedBy
      val presence   = attr map Presence
      val lack       = attr map Lack

      val flat: Gen[FilterSpec] = {
        import Gen.Covariance._
        Gen.oneofG(quotedText, simpleText, regex, reqType, hashRef, implies, impliedBy, presence, lack)
      }

      val fixRoot: EndoFn[FilterSpec] = {
        case AllOf(n) if n.tail.isEmpty => n.head
        case s => s
      }

      private def expr(depth: Int): Gen[FilterSpec] =
        if (depth <= 1)
          flat
        else {
          val next   = expr(depth - 1)
          val clause = next.nev.lim(8 `JVM|JS` 3)

          val allOf: Gen[FilterSpec] =
            clause.map(c => if (c.tail.isEmpty) c.head else AllOf(c))

          val anyOf: Gen[FilterSpec] =
            clause map AnyOf

          val not: Gen[FilterSpec] =
            next map {
              case n: Not => n
              case e      => Not(e)
            }

          Gen.oneofG(flat, allOf, anyOf, not)
        }

      val filterSpec  = expr(4 `JVM|JS` 3)
      val filterSpecO = filterSpec.option
    }

    // -----------------------------------------------------------------------------------------------------------------
    object ast {
      import FilterAst.{Text => FText, _}

      def reqs(id: Gen[ReqId]): Gen[Reqs] =
        id.set

      val attr     = Gen.oneof[Attr](Attr.AnyIssue, Attr.AnyTag)
      val presence = attr map Presence
      val lack     = attr map Lack

      val text: Gen[FText] =
        Gen.oneofG(
          simpleText.map(t => FText(t.text)),
          quotedText.map(t => FText(t.text)))

      val textPattern: Gen[Option[TextPattern]] =
        regex.map(r => FilterAst.textPattern(r.text).toOption)

      val textPatternish: Gen[FilterAst] =
        textPattern.flatMap(_.fold(text.subst[FilterAst])(Gen.insert))

      def reqType    (id: Gen[ReqTypeId])        : Gen[ReqType]        = id map ReqType
      def tag        (id: Gen[ApplicableTagId])  : Gen[Tag]            = id map Tag
      def customIssue(id: Gen[CustomIssueTypeId]): Gen[CustomIssue]    = id map CustomIssue
      def implies    (gr: Gen[Reqs])             : Gen[ImpliesAnyOf]   = gr map ImpliesAnyOf
      def impliedBy  (gr: Gen[Reqs])             : Gen[ImpliedByAnyOf] = gr map ImpliedByAnyOf

      def flat(gr: Option[Gen[ReqId]],
               gy: Option[Gen[ReqTypeId]],
               gt: Option[Gen[ApplicableTagId]],
               gi: Option[Gen[CustomIssueTypeId]]): Gen[FilterAst] = {
        import Gen.Covariance._
        val ogr = gr.map(reqs)
        val gens = (
          NonEmptyList[Gen[FilterAst]](text, textPatternish, presence, lack)
            <+ gy.map(reqType)
            <+ gt.map(tag)
            <+ gi.map(customIssue)
            <+ ogr.map(implies)
            <+ ogr.map(impliedBy))
        Gen.oneofGL(gens)
      }

      private def expr(gen: Gen[FilterAst], depth: Int): Gen[FilterAst] =
        if (depth <= 1)
          gen
        else {
          val next   = expr(gen, depth - 1)
          val clause = next.nes.lim(8 `JVM|JS` 3)

          val allOf: Gen[FilterAst] =
            clause.map(Min2Set.maybe1(_)(identity)(AllOf))

          val anyOf: Gen[FilterAst] =
            clause.map(Min2Set.maybe1(_)(identity)(AnyOf))

          val not: Gen[FilterAst] =
            next map {
              case n: Not => n
              case e      => Not(e)
            }

          Gen.oneofG(gen, allOf, anyOf, not)
        }

      def filterAst(genFlat: Gen[FilterAst]) =
        expr(genFlat, 4 `JVM|JS` 3)

      def forProject(p: Project): Gen[FilterAst] = {
        val gr: Option[Gen[ReqId]]             = Gen oneofO p.reqs.reqs.keys.toSeq
        val gy: Option[Gen[ReqTypeId]]         = Gen oneofO p.config.reqTypes.map(_.reqTypeId)
        val gt: Option[Gen[ApplicableTagId]]   = Gen oneofO p.config.atags.map(_.id)
        val gi: Option[Gen[CustomIssueTypeId]] = Gen oneofO p.config.customIssueTypes.keys.toSeq
        filterAst(flat(gr, gy, gt, gi))
      }
    }
  }

  // ===================================================================================================================
  object events {
    import shipreq.webapp.base.event._
    import shipreq.webapp.base.hash._

    val tagChildren: Gen[TagInTree.Children] =
      tagId.vector

    val tagParents: Gen[TagInTree.Parents] =
      tagId.option mapBy tagId

    val anyApplicableReqTypes =
      customReqTypeId.set flatMap applicableReqTypes

    val reqCodeIdAndValue =
      Gen.apply2(ReqCode.IdAndValue)(reqCode.id, reqCode.value)

    val fieldId: Gen[FieldId] = {
      import Gen.Covariance._
      Gen.oneofG(staticField, customFieldId)
    }

    val customTextField =
      TextGen.customTextFieldAtom(Some(reqId), Some(reqCode.id), Some(customIssueTypeId), Some(applicableTagId)).text

    val reqCodeGroupTitle =
      TextGen.reqCodeGroupTitleAtom(Some(reqId), Some(reqCode.id), Some(customIssueTypeId)).text

    val genericReqTitleAtom =
      TextGen.genericReqTitleAtom(Some(reqId), Some(reqCode.id), Some(customIssueTypeId), Some(applicableTagId))

    val genericReqTitle =
      genericReqTitleAtom.text

    val genericReqTitle1 =
      genericReqTitleAtom.text1(Text.GenericReqTitle)

    def nesd[A: UnivEq](g: Gen[A]): Gen[NonEmpty[SetDiff[A]]] = {
      val set = g.set
      val attempt =
        for {
          a <- set
          b <- set
        } yield SetDiff(a, b -- a)
      attempt.flatMap(d =>
        NonEmpty(d) match {
          case Some(ne) => Gen insert ne
          case None     => g.map(a => NonEmpty.force(SetDiff(Set.empty[A], Set(a))))
        }
      )
    }

    abstract class GenericDataGen[GD <: GenericData](final val gd: GD) {
      import gd.equalityAttr

      def valueFor(a: gd.Attr): Gen[gd.Value]

      val attr = oneofS(gd.attrs)

      lazy val values: Gen[gd.Values] =
        attr.set
          .flatMap(as => Gen sequence as.toVector.map(valueFor))
          .map(_.foldLeft(gd.emptyValues)(_ + _))

      val nonEmptyValues: Gen[gd.NonEmptyValues] =
        attr.nes
          .flatMap(as => Gen sequence as.toNEV.map(valueFor))
          .map(vs => gd.nev(vs.head, vs.tail: _*))
    }

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
        case Name        => unicodeString1.sup  map Name       .apply
        case Imp         => implicationRequired map Imp        .apply
        case gd.Mnemonic => reqTypeMnemonic     map gd.Mnemonic.apply
      }
    }

    object customTextFieldGD extends GenericDataGen(CustomTextFieldGD) {
      import gd._
      override def valueFor(a: Attr): Gen[Value] = a match {
        case Name      => unicodeString1.sup    map Name    .apply
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

    object createGenericReqGD extends GenericDataGen(CreateGenericReqGD) {
      import gd._
      override def valueFor(a: Attr): Gen[Value] = a match {
        case Title    => genericReqTitle1      map Title   .apply
        case ReqCodes => reqCodeIdAndValue.nes map ReqCodes.apply
        case Tags     => applicableTagId.nes   map Tags    .apply
        case ImpSrcs  => reqId.nes             map ImpSrcs .apply
        case ImpTgts  => reqId.nes             map ImpTgts .apply
      }
    }

    object reqCodeGroupGD extends GenericDataGen(ReqCodeGroupGD) {
      import gd._
      override def valueFor(a: Attr): Gen[Value] = a match {
        case Code  => reqCode.value     map Code .apply
        case Title => reqCodeGroupTitle map Title.apply
      }
    }

    object applicableTagGD extends GenericDataGen(ApplicableTagGD) {
      import gd._
      override def valueFor(a: Attr): Gen[Value] = a match {
        case Name     => unicodeString1.sup    map Name    .apply
        case Desc     => unicodeString1.option map Desc    .apply
        case Key      => hashRefKey            map Key     .apply
        case Children => tagChildren           map Children.apply
        case Parents  => tagParents            map Parents .apply
      }
    }

    object tagGroupGD extends GenericDataGen(TagGroupGD) {
      import gd._
      override def valueFor(a: Attr): Gen[Value] = a match {
        case Name          => unicodeString1.sup    map Name         .apply
        case Desc          => unicodeString1.option map Desc         .apply
        case MutexChildren => mutexChildren         map MutexChildren.apply
        case Children      => tagChildren           map Children     .apply
        case Parents       => tagParents            map Parents      .apply
      }
    }

    val addStaticField: Gen[AddStaticField] =
      staticField map AddStaticField

    val projectTemplate: Gen[ProjectTemplate] =
      oneofV(ProjectTemplate.values)

    val applyTemplate: Gen[ApplyTemplate] =
      projectTemplate map ApplyTemplate

    val createApplicableTag: Gen[CreateApplicableTag] =
      Gen.apply2(CreateApplicableTag)(applicableTagId, applicableTagGD.nonEmptyValues)

    val createCustomImpField: Gen[CreateCustomImpField] =
      Gen.apply2(CreateCustomImpField)(customFieldImplicationId, customImpFieldGD.nonEmptyValues)

    val createCustomIssueType: Gen[CreateCustomIssueType] =
      Gen.apply2(CreateCustomIssueType)(customIssueTypeId, customIssueTypeGD.nonEmptyValues)

    val createCustomReqType: Gen[CreateCustomReqType] =
      Gen.apply2(CreateCustomReqType)(customReqTypeId, customReqTypeGD.nonEmptyValues)

    val createCustomTagField: Gen[CreateCustomTagField] =
      Gen.apply2(CreateCustomTagField)(customFieldTagId, customTagFieldGD.nonEmptyValues)

    val createCustomTextField: Gen[CreateCustomTextField] =
      Gen.apply2(CreateCustomTextField)(customFieldTextId, customTextFieldGD.nonEmptyValues)

    val createGenericReq: Gen[CreateGenericReq] =
      Gen.apply3(CreateGenericReq)(genericReqId, customReqTypeId, createGenericReqGD.values)

    val createReqCodeGroup: Gen[CreateReqCodeGroup] =
      Gen.apply2(CreateReqCodeGroup)(reqCode.id, reqCodeGroupGD.nonEmptyValues)

    val createTagGroup: Gen[CreateTagGroup] =
      Gen.apply2(CreateTagGroup)(tagGroupId, tagGroupGD.nonEmptyValues)

    val deleteCustomField: Gen[DeleteCustomField] =
      Gen.apply2(DeleteCustomField)(customFieldId, deletionAction)

    val deleteCustomIssueType: Gen[DeleteCustomIssueType] =
      Gen.apply2(DeleteCustomIssueType)(customIssueTypeId, deletionAction)

    val deleteCustomReqType: Gen[DeleteCustomReqType] =
      Gen.apply2(DeleteCustomReqType)(customReqTypeId, deletionAction)

    val deleteReqCodeGroup: Gen[DeleteReqCodeGroup] =
      reqCode.id map DeleteReqCodeGroup

    val deleteReq: Gen[DeleteReq] =
      Gen.apply2(DeleteReq)(reqId, deletionAction)

    val deleteStaticField: Gen[DeleteStaticField] =
      staticField map DeleteStaticField

    val deleteTag: Gen[DeleteTag] =
      Gen.apply2(DeleteTag)(tagId, deletionAction)

    val patchImplicationSrc: Gen[PatchImplicationSrc] =
      Gen.apply2(PatchImplicationSrc)(reqId, nesd(reqId))

    val patchImplicationTgt: Gen[PatchImplicationTgt] =
      Gen.apply2(PatchImplicationTgt)(reqId, nesd(reqId))

    val patchReqCodes: Gen[PatchReqCodes] = {
      val codes = reqCode.id.set
      for {
        id      ← reqId
        add     ← codes.mapBy(reqCode.value).map(Multimap(_)) // TODO Could have same ID with different codes
        addIds  = add.allValues.toSet
        remove  ← codes.map(_ -- addIds)
        restore ← codes.map(_ -- addIds -- remove)
      } yield PatchReqCodes(id, remove, restore, add)
    }

    val patchReqTags: Gen[PatchReqTags] =
      Gen.apply2(PatchReqTags)(reqId, nesd(applicableTagId))

    val repositionField: Gen[RepositionField] =
      Gen.apply2(RepositionField)(fieldId, fieldId.option)

    val setCustomTextField: Gen[SetCustomTextField] =
      Gen.apply3(SetCustomTextField)(reqId, customFieldTextId, customTextField)

    val setGenericReqTitle: Gen[SetGenericReqTitle] =
      Gen.apply2(SetGenericReqTitle)(genericReqId, genericReqTitle)

    val setGenericReqType: Gen[SetGenericReqType] =
      Gen.apply2(SetGenericReqType)(genericReqId, customReqTypeId)

    val updateApplicableTag: Gen[UpdateApplicableTag] =
      Gen.apply2(UpdateApplicableTag)(applicableTagId, applicableTagGD.nonEmptyValues)

    val updateCustomImpField: Gen[UpdateCustomImpField] =
      Gen.apply2(UpdateCustomImpField)(customFieldImplicationId, customImpFieldGD.nonEmptyValues)

    val updateCustomIssueType: Gen[UpdateCustomIssueType] =
      Gen.apply2(UpdateCustomIssueType)(customIssueTypeId, customIssueTypeGD.nonEmptyValues)

    val updateCustomReqType: Gen[UpdateCustomReqType] =
      Gen.apply2(UpdateCustomReqType)(customReqTypeId, customReqTypeGD.nonEmptyValues)

    val updateCustomTagField: Gen[UpdateCustomTagField] =
      Gen.apply2(UpdateCustomTagField)(customFieldTagId, customTagFieldGD.nonEmptyValues)

    val updateCustomTextField: Gen[UpdateCustomTextField] =
      Gen.apply2(UpdateCustomTextField)(customFieldTextId, customTextFieldGD.nonEmptyValues)

    val updateReqCodeGroup: Gen[UpdateReqCodeGroup] =
      Gen.apply2(UpdateReqCodeGroup)(reqCode.id, reqCodeGroupGD.nonEmptyValues)

    val updateTagGroup: Gen[UpdateTagGroup] =
      Gen.apply2(UpdateTagGroup)(tagGroupId, tagGroupGD.nonEmptyValues)

    val activeEventGens: NonEmptyVector[Gen[ActiveEvent]] =
      valuesForAdt[ActiveEvent, Gen[ActiveEvent]] {
        case _: AddStaticField        => addStaticField        .subst
        case _: ApplyTemplate         => applyTemplate         .subst
        case _: CreateApplicableTag   => createApplicableTag   .subst
        case _: CreateCustomImpField  => createCustomImpField  .subst
        case _: CreateCustomIssueType => createCustomIssueType .subst
        case _: CreateCustomReqType   => createCustomReqType   .subst
        case _: CreateCustomTagField  => createCustomTagField  .subst
        case _: CreateCustomTextField => createCustomTextField .subst
        case _: CreateGenericReq      => createGenericReq      .subst
        case _: CreateReqCodeGroup    => createReqCodeGroup    .subst
        case _: CreateTagGroup        => createTagGroup        .subst
        case _: DeleteCustomField     => deleteCustomField     .subst
        case _: DeleteCustomIssueType => deleteCustomIssueType .subst
        case _: DeleteCustomReqType   => deleteCustomReqType   .subst
        case _: DeleteReqCodeGroup    => deleteReqCodeGroup    .subst
        case _: DeleteReq             => deleteReq             .subst
        case _: DeleteStaticField     => deleteStaticField     .subst
        case _: DeleteTag             => deleteTag             .subst
        case _: PatchImplicationSrc   => patchImplicationSrc   .subst
        case _: PatchImplicationTgt   => patchImplicationTgt   .subst
        case _: PatchReqCodes         => patchReqCodes         .subst
        case _: PatchReqTags          => patchReqTags          .subst
        case _: RepositionField       => repositionField       .subst
        case _: SetCustomTextField    => setCustomTextField    .subst
        case _: SetGenericReqTitle    => setGenericReqTitle    .subst
        case _: SetGenericReqType     => setGenericReqType     .subst
        case _: UpdateApplicableTag   => updateApplicableTag   .subst
        case _: UpdateCustomImpField  => updateCustomImpField  .subst
        case _: UpdateCustomIssueType => updateCustomIssueType .subst
        case _: UpdateCustomReqType   => updateCustomReqType   .subst
        case _: UpdateCustomTagField  => updateCustomTagField  .subst
        case _: UpdateCustomTextField => updateCustomTextField .subst
        case _: UpdateReqCodeGroup    => updateReqCodeGroup    .subst
        case _: UpdateTagGroup        => updateTagGroup        .subst
      }

    val activeEvent: Gen[ActiveEvent] =
      oneofVG(activeEventGens)

    val event: Gen[Event] = {
      val gens = valuesForAdt[Event, NonEmptyVector[Gen[Event]]] {
        case _: ActiveEvent => activeEventGens.map(_.subst)
      }
      oneofVG(gens flatMap identity)
    }

    val hashScheme: Gen[HashScheme] =
      oneofV(HashScheme.all)

    val hash: Gen[Int] = Gen.int

    val projectHash: Gen[ProjectHash] =
      Gen.apply2(ProjectHash.apply)(hashScheme, hash)

    val verifiedEvent: Gen[VerifiedEvent] =
      Gen.apply3(VerifiedEvent.apply)(hashScheme, hash, event)
  }
}
