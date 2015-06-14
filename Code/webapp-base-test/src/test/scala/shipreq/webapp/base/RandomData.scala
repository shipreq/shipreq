package shipreq.webapp.base

import japgolly.nyaya.util._
import japgolly.nyaya.test.{Distinct, Gen, GenS}
import monocle.Lens
import monocle.function.{first, second, third}
import monocle.std.{some => atSome}
import monocle.std.tuple2._
import monocle.std.tuple3._
import scala.annotation.tailrec
import scala.collection.GenTraversable
import scalaz.{NonEmptyList, OneAnd, StateT, Name, Need}
import scalaz.std.list._
import scalaz.std.option.{none => _, _}
import scalaz.std.set._
import scalaz.std.stream._
import scalaz.std.vector._

import shipreq.base.util._, MTrie.Ops
import shipreq.base.util.ScalaExt._
import shipreq.base.util.TaggedTypes.TaggedLong
import shipreq.base.util.Debug._
import shipreq.webapp.base.data._, ReqType.Mnemonic, Field.ApplicableReqTypes
import shipreq.webapp.base.delta._
import shipreq.webapp.base.test._
import shipreq.webapp.base.text.{Text, Grammar}
import DataImplicits._
import ReqFieldData.{Implications, ImplicationsU}

// TODO RandomData is inaccurate in that CorrectionParts aren't applied.

object RandomData {

  type StateG[S, A] = StateT[Gen, S, A]
  implicit def gliftS[S, A](g: Gen[A]): StateG[S, A] = StateT(s => g.map(a => (s,a)))

  def stateGen[S, A](g: S => Gen[A]): StateG[S, A] = StateT(s => g(s).map(a => (s,a)))

  implicit class CustomGenExt[A](val g: Gen[A]) extends AnyVal {
    def nev: GenS[NonEmptyVector[A]] = for {t <- g.vector; h <- g} yield NonEmptyVector(h, t)
    def nes(implicit ev: UnivEq[A]): GenS[NonEmptySet[A]] = for {t <- g.set; h <- g} yield NonEmptySet(h, t)
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
  def asciiString : Gen[String] = asciiChar.string
  def asciiString1: Gen[String] = asciiChar.string1

  def unicodeChar   : Gen[Char]   = Gen.chooseint(0, 0xd7ff) map mkChar
  def unicodeString : Gen[String] = unicodeChar.string
  def unicodeString1: Gen[String] = unicodeChar.string1

//  private val _charPredAllChars = ('\u0001' to '\ud7ff').seq
  private val _charPredAllChars = ('\u0001' to '\u0100').seq
//  private val _charPredAllChars = ('\u0020' to '\u0100').seq
//  private val _charPredAllChars = ('\u0020' to '\u0080').seq
  def charPred(p: org.parboiled2.CharPredicate): Gen[Char] =
    Gen.oneofO(_charPredAllChars filter p.apply).get

  def oneofV[A](as: NonEmptyVector[A]): Gen[A] =
    Gen.oneof(as.head, as.tail: _*)

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
    Gen.positivelong

  def shortText1 =
    Gen.string1.lim(AppConsts.shortTextMaxLength)

  def shortText =
    Gen.string.lim(AppConsts.shortTextMaxLength)

  lazy val optionalLargeText =
    shortText1.lim(AppConsts.largeTextMaxLength).option

  lazy val rev =
    Gen.positivelong.map(Rev)

  def revAnd[D](d: D): Gen[RevAnd[D]] =
    rev.map(RevAnd(_, d))

  def revAndG[D](r: Gen[D]): Gen[RevAnd[D]] =
    Gen.apply2(RevAnd[D])(rev, r)

  lazy val revPair =
    for {
      r1 <- rev
      r2 <- rev
    } yield if (r1.value <= r2.value) (r1, r2) else (r2, r1)

  def revAndIMap[D, I <: TaggedLong](r: Gen[D])(mod: List[D] => List[D])
                                    (implicit i: DataIdAux[D, I], j: TestDataIdAux[D, I]): Gen[RevAnd[IMap[I, D]]] = {
    val d = distinctId[D, I].lift[List]
    val f = mod compose d.run
    val g = f andThen (i.emptyIMap ++ _)
    revAndG(r.list map g)
  }

  def distinctId[D, I <: TaggedLong](implicit i: DataIdAux[D, I], j: TestDataIdAux[D, I]) =
    Distinct.flong.xmap(j.mkId)(_.value).distinct.contramap[D](i.id, j.setId)

  def isubset[A: UnivEq](g: Gen[NonEmptySet[A]]): Gen[ISubset[A]] = {
    Gen.oneofG(
      Gen insert ISubset.All(),
      g map ISubset.Only.apply,
      g map ISubset.Not.apply)
  }

  def imapToMapLens[K, V] = Lens((_: IMap[K, V]).underlyingMap)(v => _ replaceUnderlying v)

  lazy val live =
    Gen.oneof[Live](Live, Dead)

  lazy val implicationRequired =
    Gen.oneof[ImplicationRequired](ImplicationRequired, ImplicationRequired.Not)

  lazy val mandatory =
    Gen.oneof[Mandatory](Mandatory, Mandatory.Not)

  lazy val hashRefKey: Gen[HashRefKey] =
    grammarStr1(Grammar.hashRefKey)(_.firstChar, _.allChars, _.length) map HashRefKey

  // -------------------------------------------------------------------------------------------------------------------
  // Custom issue types

  lazy val customIssueTypeId =
    id map CustomIssueTypeId

  lazy val customIssueType =
    Gen.apply4(CustomIssueType.apply)(customIssueTypeId, hashRefKey, optionalLargeText, live)

  /** HashRefKey uniqueness enforced in Project, not here */
  lazy val customIssueTypes =
    revAndIMap(customIssueType)(identity)

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

  lazy val customReqTypes = {
    def dname = Distinct.str.at(CustomReqType.name)
    def dmnemonic = {
      val distm = reqTypeMnemonicFixer.distinct
      val cur = distm.at(CustomReqType.mnemonic)
      val old = distm.lift[Set].at(CustomReqType.oldMnemonics)
      cur + old
    }
    val d = (dname * dmnemonic).lift[List]
    revAndIMap(customReqType)(d.run)
  }

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

  lazy val tag =
    Gen.oneofG[Tag](tagGroup.subst, applicableTag.subst)

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

  lazy val revAndTagTree: Gen[RevAnd[TagTree]] =
    Gen.apply2(RevAnd.apply[TagTree])(rev, tagTree)

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

  // -------------------------------------------------------------------------------------------------------------------
  // Text

  object TextGen {
    import shipreq.webapp.base.text._
    import Atom._
    import Text.{ReqTitle => _, _}
    import Gen.Covariance._

    lazy val webAddressR = charPred(Parsers.webAddressChar).string1
    lazy val emailL = charPred(Parsers.emailCharL).string1
    lazy val emailR = charPred(Parsers.emailCharR).string1

    // private[this] implicit def autoSomeG[A](g: Gen[A]) = g.some
    private[this] implicit class ELExt[A <: AnyAtom](val _l: List[Gen[A]]) extends AnyVal {
      def <+(o: Option[Gen[A]]): List[Gen[A]] =
        o.fold(_l)(_ :: _l)
    }
    private[this] implicit class NELExt[A <: AnyAtom](val _nel: NonEmptyList[Gen[A]]) extends AnyVal {
      def <+(o: Option[Gen[A]]): NonEmptyList[Gen[A]] =
        o.fold(_nel)(_ <:: _nel)
      def <++(l: List[Gen[A]]): NonEmptyList[Gen[A]] =
        l <::: _nel
    }

    val strchr  = Gen.oneofG(Gen.chooseint(32, 127), Gen.chooseint(128, 65534)).map(_.toChar)
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

    val removeFromLiteralsR = """[#@]+|[a-z]://|\*( )|<math>|\[\s*[a-zA-Z]+\s*(?:-\s*)?\d+\s*\]""".r
    def removeFromLiterals[L <: Literal#Literal](l: L): L =
      l.map(removeFromLiteralsR.replaceAllIn(_, "x$1"))

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

  def sGenericReq(pubidS: ReqIdC => StateG[PubidRegister, PubidC], rtLive: ReqTypeId => Live): StateG[PubidRegister, GenericReq] =
    for {
      id     ← genericReqId |> gliftS[PubidRegister, GenericReqId]
      pubid  ← pubidS(id)
      desc   = Vector.empty
      live0  ← live
    } yield {
      val live = if (rtLive(pubid.reqTypeId) :: Dead) Dead else live0
      GenericReq(id, pubid, desc, live)
    }

  def pubidRegisterAnd[A, B](inita: A, genb: StateG[PubidRegister, B])(f: (A, B) => A): GenS[(PubidRegister, A)] = {
    val init = StateT.stateT[Gen, PubidRegister, A](inita)
    GenS.choosesize flatMap { sz =>
      val prog = Stream.fill(sz)(genb).foldLeft(init)((sn, ga) =>
        for {
          b <- sn
          a <- ga
        } yield f(b, a)
      )
      prog(PubidRegister.empty)
    }
  }

  def pubidRegisterAndIds(customReqTypeIds: NonEmptyVector[CustomReqTypeId]): GenS[(PubidRegister, Set[ReqIdC])] =
    pubidRegisterAnd(Set.empty[ReqIdC], sGenericReqId(sAllocPubidC(customReqTypeIds)))(_ + _)

  def requirements(customReqTypeIds: Vector[CustomReqTypeId], rtLive: ReqTypeId => Live): GenS[Requirements] =
    NonEmptyVector.maybe(customReqTypeIds,
      GenS(_ => Gen insert Requirements.empty))( // ← This will change when UseCases are added
      customReqTypeIdNev =>
        pubidRegisterAnd(Requirements.emptyData, sGenericReq(sAllocPubidC(customReqTypeIdNev), rtLive))(_ + _)
          .map { case (pr, reqs) => Requirements(reqs, pr) }
      )

  def updateRequirementText(gt: Gen[Text.GenericReqTitle.OptionalText])(data: Requirements.Data): Gen[Requirements.Data] = {
    val streamOfGens = data.vstream {
        case v: GenericReq => gt.map(t => v.copy(title = t))
      }
    val genStream = Gen.sequence(streamOfGens)
    genStream.map(Requirements.emptyData ++ _)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Req Data

  def reqFieldDataText(cols: Set[CustomField.Text.Id], reqs: Set[ReqId], txt: Gen[Text.CustomTextField.NonEmptyText]): Gen[ReqFieldData.Text] =
    txt mapByKeySubset reqs mapByKeySubset cols

  def reqFieldDataTags(reqs: TraversableOnce[ReqId], tags: Set[ApplicableTagId]): Gen[ReqFieldData.Tags] = {
    val rndTags = Gen.subset(tags).map(_.toSet)
    (rndTags mapByKeySubset reqs).map(Multimap(_))
  }

  type ImplicationsUM = Map[ReqId, Set[ReqId]]
  @tailrec def preventImplicationCycles(m: ImplicationsUM): ImplicationsUM =
    ReqFieldData.implicationCycleDetector.findCycle(m) match {
      case None         => m
      case Some((a, b)) => preventImplicationCycles(m - b)
    }

  val emptyImplicationsU: ImplicationsU = Multimap.empty

  val MaxImplicationPairs = 100 `JVM|JS` 40
  // val MaxImplicationsPerSrc = 2  `JVM|JS` 4
  // val MaxImplicationKeys    = 10 `JVM|JS` 4

  def reqFieldDataImplications(reqIds: Set[ReqId]): Gen[Implications] = {
    def fix(m: ImplicationsUM): Implications = {
      val m2 = preventImplicationCycles(m)
      // println(m2); println()
      Implications(Multimap(m2))
    }

    def method1(g: Gen[ReqId]) =
      g.pair
        .list.lim(MaxImplicationPairs)
        .map(kvs => emptyImplicationsU.addPairs(kvs: _*).m |> fix)

//    def method2(g: Gen[ReqId]) =
//      Gen.tuple2(g, g.set1 lim MaxImplicationsPerSrc)
//        .list.lim(MaxImplicationKeys)
//        .map(_.toMap |> fix)

    Gen.oneofO(reqIds.toSeq) match {
      case Some(g) => method1(g)
      case None    => Gen insert Implications(emptyImplicationsU)
    }
  }

  // def customTextFieldAtom(gr: Gen[ReqId], gi: Gen[CustomIssueTypeId], gt: Gen[ApplicableTagId]): Gen[CustomTextField.Atom] = {
  def reqFieldData(reqs    : Set[ReqId],
                   txtCols : Set[CustomField.Text.Id],
                   reqCodeG: Option[Gen[ReqCodeId]],
                   cissueG : Option[Gen[CustomIssueTypeId]],
                   tagG    : Option[Gen[ApplicableTagId]],
                   tags    : Set[ApplicableTagId]): Gen[ReqFieldData] = {

    val gr = Gen.oneofO(reqs.toSeq)

    Gen.apply3(ReqFieldData.apply)(
      reqFieldDataText(txtCols, reqs, TextGen.customTextFieldAtom(gr, reqCodeG, cissueG, tagG).ptext1(Text.CustomTextField)),
      reqFieldDataTags(reqs, tags),
      reqFieldDataImplications(reqs))
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Req Codes

  object reqCode {
    import ReqCode._

    lazy val node: Gen[Node] =
      grammarStr1(Grammar.reqCode)(_.firstChar, _.allChars, _.nodeLength) map Node.applyFn

    lazy val value: GenS[Value] =
      node.nev

    lazy val id =
      RandomData.id map ReqCodeId

    type FlatInstance = (Value, Data)

    val distinctIds =
      Distinct.flong.xmap(ReqCodeId)(_.value).distinct

    val distinctReqCodes = {
      def fix(ss: Set[ReqCode.Value]): ReqCode.Value = {
        var c = ss.head
        while (ss contains c) {
          val n1 = c.head
          val n2 = ReqCode.Node(n1.value + "x")
          c = NonEmptyVector(n2, c.tail)
        }
        c
      }
      Distinct.Fixer.lift(fix).distinct
    }

    val distinctFlatInstances = {
      val flatData = second[FlatInstance, Data]
      val dataActive = flatData ^|-? (Data.active ^<-? atSome)

      val ids1 = distinctIds at ActiveData.id at dataActive
      val ids2 = distinctIds.lift[Set] at Data.refsToGroup at flatData
      val ids3 = distinctIds.lift[Set]
        .liftMapValues.contramap[Multimap[ReqId, Set, ReqCodeId]](_.m, (_, m: Map[ReqId, Set[ReqCodeId]]) => Multimap(m))
        // TODO ↑ Use liftMultimapValues when Nyaya gets it
        .at(flatData ^|-> Data.refsToReqs)
      val id = ids1 + ids2 + ids3

      val reqCode = distinctReqCodes at first[FlatInstance, Value]

      (id * reqCode).lift[Vector]
    }

    val smallIdSet = id.set.lim(3)

    val gEmptyRefsToReqs: Gen[Multimap[ReqId, Set, ReqCodeId]] =
      Gen.insert(Multimap.empty)

    def data(ogLiveReqId: Option[Gen[ReqId]], ogReqId: Option[Gen[ReqId]], gGroup: Gen[ReqCodeGroup]): Gen[Data] = {
      import Gen.Covariance._

      val gTarget: Gen[Target] =
        ogLiveReqId match {
          case Some(g) => Gen.oneofG(g, g, g, g, gGroup)
          case None    => gGroup
        }

      val gRefsToReqs: Gen[Multimap[ReqId, Set, ReqCodeId]] =
        ogReqId match {
          case Some(g) => g.mapTo(smallIdSet).map(Multimap(_))
          case None    => gEmptyRefsToReqs
        }

      for {
        i           <- id
        target      <- gTarget
        refsToGroup <- smallIdSet
        refsToReqs  <- gRefsToReqs
        x           <- Gen.chooseint(0, 9)
      } yield
        if (x == 0)
          target match {
            case t: ReqId        => Data(None, refsToGroup, refsToReqs.add(t, i))
            case _: ReqCodeGroup => Data(None, refsToGroup + i, refsToReqs)
          }
        else
          Data(Some(ActiveData(i, target)), refsToGroup, refsToReqs)
    }

    def flatInstance(gData: Gen[Data]): Gen[FlatInstance] =
      Gen.tuple2(value, gData)


    def trie(ogLiveReqId: Option[Gen[ReqId]], ogReqId: Option[Gen[ReqId]], gGroup: Gen[ReqCodeGroup]): GenS[Trie] =
      flatInstance(data(ogLiveReqId, ogReqId, gGroup)).vector
        .map(distinctFlatInstances.run)
        .map(_.foldLeft(emptyTrie) { case (q, (c, d)) => q.put(c, d) })

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
    type A = RevAnd[CustomIssueTypeIMap]
    type B = RevAnd[TagTree]
    type T = (A, B)
    val keyDist = hashRefFixer.distinct
    val issues = keyDist
      .at(CustomIssueType.key).liftMapValues[CustomIssueTypeId]
      .at(first[T, A] ^|-> RevAnd.data[CustomIssueTypeIMap] ^|-> imapToMapLens)
    val tags = keyDist
      .lift[Option].contramap[Tag](_.keyO, setTagKey)
      .at(TagInTree.tag).liftMapValues[TagId]
      .at(second[T, B] ^|-> RevAnd.data[TagTree] ^|-> imapToMapLens)
    issues + tags
  }

  lazy val project: Gen[Project] =
    for {
      (issues, tags) ← Gen.tuple2(customIssueTypes, revAndTagTree) map distinctHashRefKeys.run
      cissueIds      = issues.data.keySet
      cissueIdG      = Gen oneofO cissueIds.toSeq
      reqtypes       ← customReqTypes
      deadReqtypeIds = reqtypes.data.values.toStream.filter(_.live :: Dead).map(_.id)
      reqTypeIdsC    = reqtypes.data.keys.toVector
      reqTypeIds     = StaticReqType.values ++ reqTypeIdsC
      reqTypeIdSet   = reqTypeIds.whole.toSet
      fields         ← revAndG(fieldSet(reqTypeIdSet, tags.data.keySet, reqtypes.data.keySet))
      reqs1          ← requirements(reqTypeIdsC, id => Dead <~ (deadReqtypeIds contains id))
      reqIds         = reqs1.reqs.keys
      liveReqIds     = reqs1.reqs.values.toStream.filter(_.live :: Live).map(_.id)
      reqIdSet       = reqIds.toSet
      reqIdG         = Gen oneofO reqIds.toSeq
      liveReqIdG     = Gen oneofO liveReqIds
      reqCodes1      ← reqCodes(reqCode.trie(liveReqIdG, reqIdG, reqCode.gEmptyReqCodeGroup).lim(18 `JVM|JS` 6))
      activeCodeIds  = reqCodes1.cataA(Vector.empty[ReqCodeId])((q, _, a) => q :+ a.id)
      activeCodeIdG  = Gen oneofO activeCodeIds
      atagIds        = tags.data.vstream(_.tag).filterT[ApplicableTag].map(_.id).toSet
      atagIdG        = Gen.oneofO(atagIds.toSeq)
      textColIds     = fields.data.customFields.values.filterT[CustomField.Text].map(_.id).toSet
      reqFieldData   ← revAndG(reqFieldData(reqIdSet, textColIds, activeCodeIdG, cissueIdG, atagIdG, atagIds))
      reqs2          ← genmodL(Requirements.reqs)(updateRequirementText(TextGen.genericReqTitleAtom(reqIdG, activeCodeIdG, cissueIdG, atagIdG).text))(reqs1)
      reqCodes2      ← reqCode.updateGroupText(TextGen.reqCodeGroupTitleAtom(reqIdG, activeCodeIdG, cissueIdG).text)(reqCodes1.trie)
      reqs           ← revAnd(reqs2)
      reqCodes       ← revAnd(ReqCodes(reqCodes2))
    } yield Project(issues, reqtypes, fields, tags, reqs, reqCodes, reqFieldData)

  // ===================================================================================================================
  // Protocol
  object protocol {
    import shipreq.webapp.base.protocol.{FieldProtocol => FP, _}
    import Gen.Covariance._

    lazy val deletionAction =
      oneofV(DeletionAction.values)

    lazy val reqTypeId: Gen[ReqTypeId] =
      Gen.oneofG(customReqTypeId, staticReqType)

    lazy val fieldId: Gen[FieldId] =
      Gen.oneofG(customFieldId, staticField)

    lazy val applicableReqTypes: Gen[ApplicableReqTypes] =
      isubset(reqTypeId.nes)

    lazy val fieldPosition: Gen[FP.Position] =
      fieldId.option

    lazy val textFieldValues =
      Gen.apply4(FP.TextFieldValues.apply)(shortText1, fieldRefKey, mandatory, applicableReqTypes)

    lazy val fieldValues: Gen[FP.Values] =
      Gen.oneofG(textFieldValues)

    lazy val fieldDelta: Gen[FP.Delta] =
      Gen.apply2(FP.Delta.apply)(staticField \/ customField(applicableReqTypes, true, true), fieldPosition)

    object fieldCfgAction {
      import FP.CfgAction, CfgAction._
      lazy val create      : Gen[Create]       = fieldValues map Create
      lazy val updateValues: Gen[UpdateValues] = Gen.apply2(UpdateValues)(customFieldId, fieldValues)
      lazy val updateOrder : Gen[UpdateOrder]  = Gen.apply2(UpdateOrder)(fieldId, fieldPosition)
      lazy val delete      : Gen[Delete]       = Gen.apply2(Delete)(fieldId, deletionAction)
      lazy val any         : Gen[CfgAction]    = Gen.oneofG(create, updateValues, updateOrder, delete)
    }

    def tagProtocolValues: Tag => TagProtocol.Values = {
      case TagGroup(_, n, d, mc, _)     => TagProtocol.TagGroupValues(n, mc, d)
      case ApplicableTag(_, n, d, k, _) => TagProtocol.ApplicableTagValues(n, k, d)
    }

    lazy val tagCrudInput =
      remoteDeltaG.povTag.flatMap(t => {
        val a = Gen insert tagProtocolValues(t.tag)
        val b = Gen insert t.rels
        a \&/ b
      })
  }

  // ===================================================================================================================
  object remoteDeltaG {
    import shipreq.webapp.base.protocol._
    import RandomData.protocol._

    def forPart: Partition => Gen[RemoteDeltaG] = {
      case Partition.CustomIssueTypes => customIssueTypesDG
      case Partition.CustomReqTypes   => customReqTypesDG
      case Partition.Fields           => fieldsDG
      case Partition.Tags             => tagsDG
    }

    def generic(p: Partition)(ir: Gen[p.Id], dr: Gen[p.Data]): Gen[RemoteDeltaG] = {
      import p.di
      for {
        d        ← dr.list
        i0       ← ir.set
        i        = d.foldLeft(i0)(_ - _.id)
        (r1, r2) ← revPair
      } yield RemoteDeltaG(p, r1, r2)(i, d)
    }

    lazy val customIssueTypesDG =
      generic(Partition.CustomIssueTypes)(customIssueTypeId, customIssueType)

    lazy val customReqTypesDG =
      generic(Partition.CustomReqTypes)(customReqTypeId, customReqType)

    lazy val fieldsDG =
      generic(Partition.Fields)(fieldId, fieldDelta)

    lazy val tagsDG =
      generic(Partition.Tags)(tagId, povTag)

    lazy val povTag =
      for {
        t      ← tag
        (p, c) ← tagId.set.pair
      } yield {
        val children = (c - t.id -- p).toVector
        val parents  = (p - t.id -- c).toStream.map(_ -> none[TagId]).toMap
        TagProtocol.PovTag(t, TagProtocol.PovRelations(parents, children))
      }
  }

  object remoteDelta {
    def forPart: Partition => Gen[RemoteDelta] =
      remoteDeltaG.forPart(_).map(List(_))
  }

  // ===================================================================================================================
  object routines {
    import shipreq.webapp.base.protocol._
    import Routine._, Routines._
    import RandomData.protocol._

    lazy val remoteName =
      Gen.alphanumericstring1

    def remote[D <: Desc](d: D) =
      remoteName.map(Remote(_, d))

    lazy val projectSPA =
      Gen.apply7(ProjectSPA)(
        remote(ProjectInit),
        remote(CustomIssueTypeCrud),
        remote(CustomReqTypeCrud),
        remote(ReqTypeImplicationMod),
        remote(FieldMandatorinessMod),
        remote(FieldCrud),
        remote(TagCrud))

    class CrudActionGens[I, V](c: Crudable.Aux[I, V])(idG: Gen[I], vG: Gen[V]) {
      import Gen.Covariance._
      lazy val create = vG.map(CrudAction.Create[V])
      lazy val update = Gen.apply2(CrudAction.Update[I, V])(idG, vG)
      lazy val delete = Gen.apply2(CrudAction.Delete[I])(idG, deletionAction)
      lazy val any    = Gen.oneofG[CrudAction[I, V]](create, update, delete)
    }

    lazy val customIssueTypeCrud = new CrudActionGens(CustomIssueTypeCrud)(
      RandomData.customIssueTypeId,
      Gen.tuple2(hashRefKey, optionalLargeText))

    lazy val customReqTypeCrud = new CrudActionGens(CustomReqTypeCrud)(
      RandomData.customReqTypeId,
      Gen.tuple3(reqTypeMnemonic, customReqTypeName, implicationRequired))

    lazy val tagCrud =
      new CrudActionGens(TagCrud)(RandomData.tagId, tagCrudInput)
  }

  // ===================================================================================================================
  object filter {
    import shipreq.webapp.base.filter._

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

      val quotedText =
        for {
          q <- Gen.oneof('\'', '"', '`')
          s <- unicodeString1
        } yield QuotedText(s.replace(q, '_'), q)

      private val illegalSimpleTextStart = "/-#(){}'`\"".toCharArray.toSet
      def fixSimpleText(s: String): String =
        if (s.headOption exists illegalSimpleTextStart.contains)
          "!" + s
        else if (Validators.reqType.mnemonicU isValidU s)
          s + "?"
        else
          s


      /** An odd number of backslashes cannot precede a slash */
      private val fixSlashEscaping = """(^|[^\\])(?:\\(?:\\\\)*)/""".r

      def fixRegex(s: String): String =
        if (s endsWith "\\")
          s + "d"
        else
          fixSlashEscaping.replaceAllIn(s, "$1/")

      val simpleText = charPred(FilterParser.simpleTextChar).string1.map(s => SimpleText(fixSimpleText(s)))
      val regex      = unicodeString1.map(s => Regex(fixRegex(s)))
      val reqType    = reqTypeMnemonic map ReqType
      val hashRef    = hashRefKey.map(h => HashRef(h.value))
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
  }
}
