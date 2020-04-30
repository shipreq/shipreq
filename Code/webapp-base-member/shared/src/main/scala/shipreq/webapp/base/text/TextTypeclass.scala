package shipreq.webapp.base.text

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import shipreq.base.util.NonEmptyArraySeq

/**
 * Provides a specific typeclass (`TC[_]`) for any text type.
 */
trait AtomTC[TC[_]] {

  def lazily[A](a: => TC[A]): TC[A]

  def arr[A](implicit a: TC[A], ct: ClassTag[A]): TC[ArraySeq[A]]
  def nea[A](as: TC[ArraySeq[A]])(implicit a: TC[A]): TC[NonEmptyArraySeq[A]]

  def sum[T <: Atom.Base](t: T)(get: Atom.Type => TC[t.Atom], all: List[TC[t.Atom]]): TC[t.Atom]

  def blankLine     [T <: Atom.NewLine        ](t: T): TC[t.BlankLine     ]
  def codeBlock     [T <: Atom.CodeBlock      ](t: T): TC[t.CodeBlock     ]
  def codeRef       [T <: Atom.ContentRef     ](t: T): TC[t.CodeRef       ]
  def emailAddress  [T <: Atom.PlainTextMarkup](t: T): TC[t.EmailAddress  ]
  def literal       [T <: Atom.Literal        ](t: T): TC[t.Literal       ]
  def monospace     [T <: Atom.PlainTextMarkup](t: T): TC[t.Monospace     ]
  def reqRef        [T <: Atom.ContentRef     ](t: T): TC[t.ReqRef        ]
  def tagRef        [T <: Atom.TagRef         ](t: T): TC[t.TagRef        ]
  def teX           [T <: Atom.PlainTextMarkup](t: T): TC[t.TeX           ]
  def useCaseStepRef[T <: Atom.ContentRef     ](t: T): TC[t.UseCaseStepRef]
  def webAddress    [T <: Atom.PlainTextMarkup](t: T): TC[t.WebAddress    ]

  def issue        [T <: Atom.Issue     ](t: T)(implicit x: TC[Text.InlineIssueDesc.OptionalText]): TC[t.Issue]
  def unorderedList[T <: Atom.ListMarkup](t: T)(implicit x: TC[NonEmptyArraySeq[t.ListItem]])     : TC[t.UnorderedList]

  final val instances = TextTC[TC](this)
}

object TextTC {
  def apply[TC[_]](a: AtomTC[TC]): TextTC[TC] =
    new TextTC(a)
}

class TextTC[TC[_]](a: AtomTC[TC]) {
  import TextMacros.generateTypeclasses

  private[this] lazy val issue3      = generateTypeclasses(Text.InlineIssueDesc)
  private[this] val genericReqTitle3 = generateTypeclasses(Text.GenericReqTitle)
  private[this] val codeGroupTitle3  = generateTypeclasses(Text.CodeGroupTitle)
  private[this] val customTextField3 = generateTypeclasses(Text.CustomTextField)
  private[this] val deletionReason3  = generateTypeclasses(Text.DeletionReason)
  private[this] val manualIssue3     = generateTypeclasses(Text.ManualIssue)
  private[this] val useCaseStep3     = generateTypeclasses(Text.UseCaseStep)
  private[this] val useCaseTitle3    = generateTypeclasses(Text.UseCaseTitle)

  implicit val (inlineIssueDescA, inlineIssueDescO, inlineIssueDescN) = issue3
  implicit val (genericReqTitleA, genericReqTitleO, genericReqTitleN) = genericReqTitle3
  implicit val ( codeGroupTitleA,  codeGroupTitleO,  codeGroupTitleN) = codeGroupTitle3
  implicit val (customTextFieldA, customTextFieldO, customTextFieldN) = customTextField3
  implicit val ( deletionReasonA,  deletionReasonO,  deletionReasonN) = deletionReason3
  implicit val (    manualIssueA,     manualIssueO,     manualIssueN) = manualIssue3
  implicit val (    useCaseStepA,     useCaseStepO,     useCaseStepN) = useCaseStep3
  implicit val (   useCaseTitleA,    useCaseTitleO,    useCaseTitleN) = useCaseTitle3
}
