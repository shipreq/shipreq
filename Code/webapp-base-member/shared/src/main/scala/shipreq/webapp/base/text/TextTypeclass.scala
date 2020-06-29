package shipreq.webapp.base.text

import scala.annotation.nowarn
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import shipreq.base.util.NonEmptyArraySeq

/**
 * Provides a specific typeclass (`TC[_]`) for any text type.
 */
trait AtomTC[TC[_]] {

  def lazily[A](a: => TC[A]): TC[A]

  def xmap[A, B](fa: TC[A])(f: A => B)(g: B => A): TC[B]

  def arr[A](implicit a: TC[A], ct: ClassTag[A]): TC[ArraySeq[A]]
  def nea[A](as: TC[ArraySeq[A]])(implicit a: TC[A]): TC[NonEmptyArraySeq[A]]

  def sum[T <: Atom.Base](t: T)(get: Atom.Type => TC[t.Atom], all: List[TC[t.Atom]]): TC[t.Atom]

  def str: TC[String]

  def blankLine     [T <: Atom.NewLine   ](t: T): TC[t.BlankLine     ]
  def codeBlock     [T <: Atom.CodeBlock ](t: T): TC[t.CodeBlock     ]
  def codeRef       [T <: Atom.ContentRef](t: T): TC[t.CodeRef       ]
  def reqRef        [T <: Atom.ContentRef](t: T): TC[t.ReqRef        ]
  def tagRef        [T <: Atom.TagRef    ](t: T): TC[t.TagRef        ]
  def useCaseStepRef[T <: Atom.ContentRef](t: T): TC[t.UseCaseStepRef]

  def issue        [T <: Atom.Issue     ](t: T)(implicit x: TC[Text.InlineIssueDesc.OptionalText]): TC[t.Issue]
  def unorderedList[T <: Atom.ListMarkup](t: T)(implicit x: TC[NonEmptyArraySeq[t.ListItem]])     : TC[t.UnorderedList]

  final val instances = TextTC[TC](this)
}

object TextTC {
  def apply[TC[_]](a: AtomTC[TC]): TextTC[TC] =
    new TextTC(a)
}

@nowarn("cat=unused")
class TextTC[TC[_]](a: AtomTC[TC]) {
  import TextMacros._
  import a._

  final def emailAddress  [T <: Atom.PlainTextMarkup](t: T): TC[t.EmailAddress] = xmap(str)(t.EmailAddress(_))(_.value)
  final def literal       [T <: Atom.Literal        ](t: T): TC[t.Literal     ] = xmap(str)(t.Literal     (_))(_.value)
  final def monospace     [T <: Atom.PlainTextMarkup](t: T): TC[t.Monospace   ] = xmap(str)(t.Monospace   (_))(_.value)
  final def teX           [T <: Atom.PlainTextMarkup](t: T): TC[t.TeX         ] = xmap(str)(t.TeX         (_))(_.value)
  final def webAddress    [T <: Atom.PlainTextMarkup](t: T): TC[t.WebAddress  ] = xmap(str)(t.WebAddress  (_))(_.value)

  private final def bold         [T <: Atom.PlainTextMarkup](t: T)(h: TC[t.styled.NonEmptyText]): TC[t.Bold         ] = xmap(h)(t.Bold         (_))(_.inner)
  private final def italic       [T <: Atom.PlainTextMarkup](t: T)(h: TC[t.styled.NonEmptyText]): TC[t.Italic       ] = xmap(h)(t.Italic       (_))(_.inner)
  private final def strikethrough[T <: Atom.PlainTextMarkup](t: T)(h: TC[t.styled.NonEmptyText]): TC[t.Strikethrough] = xmap(h)(t.Strikethrough(_))(_.inner)
  private final def underline    [T <: Atom.PlainTextMarkup](t: T)(h: TC[t.styled.NonEmptyText]): TC[t.Underline    ] = xmap(h)(t.Underline    (_))(_.inner)

  private final def heading1[T <: Atom.Headings](t: T)(h: TC[t.headingTitle.NonEmptyText]): TC[t.Heading1] = xmap(h)(t.Heading1(_))(_.title)
  private final def heading2[T <: Atom.Headings](t: T)(h: TC[t.headingTitle.NonEmptyText]): TC[t.Heading2] = xmap(h)(t.Heading2(_))(_.title)
  private final def heading3[T <: Atom.Headings](t: T)(h: TC[t.headingTitle.NonEmptyText]): TC[t.Heading3] = xmap(h)(t.Heading3(_))(_.title)
  private final def heading4[T <: Atom.Headings](t: T)(h: TC[t.headingTitle.NonEmptyText]): TC[t.Heading4] = xmap(h)(t.Heading4(_))(_.title)
  private final def heading5[T <: Atom.Headings](t: T)(h: TC[t.headingTitle.NonEmptyText]): TC[t.Heading5] = xmap(h)(t.Heading5(_))(_.title)
  private final def heading6[T <: Atom.Headings](t: T)(h: TC[t.headingTitle.NonEmptyText]): TC[t.Heading6] = xmap(h)(t.Heading6(_))(_.title)

  private[this] lazy val codeGroupTitle3        = generateTypeclasses(Text.CodeGroupTitle)
  private[this] lazy val customTextField3       = generateTypeclasses(Text.CustomTextField)
  private[this] lazy val deletionReason3        = generateTypeclasses(Text.DeletionReason)
  private[this] lazy val genericReqTitle3       = generateTypeclasses(Text.GenericReqTitle)
  private[this] lazy val headingTitleFull3      = generateTypeclasses(Text.HeadingTitleFull)
  private[this] lazy val headingTitleNoIssues3  = generateTypeclasses(Text.HeadingTitleNoIssues)
  private[this] lazy val issue3                 = generateTypeclasses(Text.InlineIssueDesc)
  private[this] lazy val manualIssue3           = generateTypeclasses(Text.ManualIssue)
  private[this] lazy val styledInnerContentRef3 = generateTypeclasses(Text.StyledInnerContentRef)
  private[this] lazy val styledInnerFull3       = generateTypeclasses(Text.StyledInnerFull)
  private[this] lazy val styledInnerNoIssues3   = generateTypeclasses(Text.StyledInnerNoIssues)
  private[this] lazy val styledInnerNoTags3     = generateTypeclasses(Text.StyledInnerNoTags)
  private[this] lazy val useCaseStep3           = generateTypeclasses(Text.UseCaseStep)
  private[this] lazy val useCaseTitle3          = generateTypeclasses(Text.UseCaseTitle)

  implicit def codeGroupTitleA        = codeGroupTitle3       ._1
  implicit def customTextFieldA       = customTextField3      ._1
  implicit def deletionReasonA        = deletionReason3       ._1
  implicit def genericReqTitleA       = genericReqTitle3      ._1
  implicit def headingTitleFullA      = headingTitleFull3     ._1
  implicit def headingTitleNoIssuesA  = headingTitleNoIssues3 ._1
  implicit def issueA                 = issue3                ._1
  implicit def manualIssueA           = manualIssue3          ._1
  implicit def styledInnerContentRefA = styledInnerContentRef3._1
  implicit def styledInnerFullA       = styledInnerFull3      ._1
  implicit def styledInnerNoIssuesA   = styledInnerNoIssues3  ._1
  implicit def styledInnerNoTagsA     = styledInnerNoTags3    ._1
  implicit def useCaseStepA           = useCaseStep3          ._1
  implicit def useCaseTitleA          = useCaseTitle3         ._1

  implicit def codeGroupTitleO        = codeGroupTitle3       ._2
  implicit def customTextFieldO       = customTextField3      ._2
  implicit def deletionReasonO        = deletionReason3       ._2
  implicit def genericReqTitleO       = genericReqTitle3      ._2
  implicit def headingTitleFullO      = headingTitleFull3     ._2
  implicit def headingTitleNoIssuesO  = headingTitleNoIssues3 ._2
  implicit def issueO                 = issue3                ._2
  implicit def manualIssueO           = manualIssue3          ._2
  implicit def styledInnerContentRefO = styledInnerContentRef3._2
  implicit def styledInnerFullO       = styledInnerFull3      ._2
  implicit def styledInnerNoIssuesO   = styledInnerNoIssues3  ._2
  implicit def styledInnerNoTagsO     = styledInnerNoTags3    ._2
  implicit def useCaseStepO           = useCaseStep3          ._2
  implicit def useCaseTitleO          = useCaseTitle3         ._2

  implicit def codeGroupTitleN        = codeGroupTitle3       ._3
  implicit def customTextFieldN       = customTextField3      ._3
  implicit def deletionReasonN        = deletionReason3       ._3
  implicit def genericReqTitleN       = genericReqTitle3      ._3
  implicit def headingTitleFullN      = headingTitleFull3     ._3
  implicit def headingTitleNoIssuesN  = headingTitleNoIssues3 ._3
  implicit def issueN                 = issue3                ._3
  implicit def manualIssueN           = manualIssue3          ._3
  implicit def styledInnerContentRefN = styledInnerContentRef3._3
  implicit def styledInnerFullN       = styledInnerFull3      ._3
  implicit def styledInnerNoIssuesN   = styledInnerNoIssues3  ._3
  implicit def styledInnerNoTagsN     = styledInnerNoTags3    ._3
  implicit def useCaseStepN           = useCaseStep3          ._3
  implicit def useCaseTitleN          = useCaseTitle3         ._3
}
