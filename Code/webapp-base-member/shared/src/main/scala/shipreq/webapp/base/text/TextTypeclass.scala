package shipreq.webapp.base.text

import japgolly.microlibs.nonempty.NonEmptyVector

/**
 * Provides a specific typeclass (`TC[_]`) for any text type.
 */
trait AtomTC[TC[_]] {

  def lazily[A](a: => TC[A]): TC[A]

  def vec[A](implicit a: TC[A]): TC[Vector[A]]
  def nev[A](as: TC[Vector[A]])(implicit a: TC[A]): TC[NonEmptyVector[A]]

  def sum[T <: Atom.Base](t: T)(f: t.Atom => TC[t.Atom], index: t.Atom => Int, all: Vector[TC[t.Atom]]): TC[t.Atom]

  def blankLine     [T <: Atom.NewLine        ](t: T): TC[t.BlankLine     ]
  def literal       [T <: Atom.Literal        ](t: T): TC[t.Literal       ]
  def webAddress    [T <: Atom.PlainTextMarkup](t: T): TC[t.WebAddress    ]
  def emailAddress  [T <: Atom.PlainTextMarkup](t: T): TC[t.EmailAddress  ]
  def mathTeX       [T <: Atom.PlainTextMarkup](t: T): TC[t.MathTeX       ]
  def reqRef        [T <: Atom.ReqRef         ](t: T): TC[t.ReqRef        ]
  def codeRef       [T <: Atom.ReqRef         ](t: T): TC[t.CodeRef       ]
  def useCaseStepRef[T <: Atom.ReqRef         ](t: T): TC[t.UseCaseStepRef]
  def tagRef        [T <: Atom.TagRef         ](t: T): TC[t.TagRef        ]

  def issue        [T <: Atom.Issue     ](t: T)(implicit x: TC[Text.InlineIssueDesc.OptionalText]): TC[t.Issue]
  def unorderedList[T <: Atom.ListMarkup](t: T)(implicit x: TC[NonEmptyVector[t.ListItem]])       : TC[t.UnorderedList]

  final val instances = TextTC[TC](this)
}

object TextTC {
  def apply[TC[_]](a: AtomTC[TC]): TextTC[TC] =
    new TextTC(a)
}

class TextTC[TC[_]](a: AtomTC[TC]) {
  import TextMacros.generateTypeclasses

  private lazy val issue3 = generateTypeclasses(Text.InlineIssueDesc)

  implicit val (inlineIssueDescA, inlineIssueDescO, inlineIssueDescN) = issue3
  implicit val (genericReqTitleA, genericReqTitleO, genericReqTitleN) = generateTypeclasses(Text.GenericReqTitle)
  implicit val ( codeGroupTitleA,  codeGroupTitleO,  codeGroupTitleN) = generateTypeclasses(Text.CodeGroupTitle)
  implicit val (customTextFieldA, customTextFieldO, customTextFieldN) = generateTypeclasses(Text.CustomTextField)
  implicit val ( deletionReasonA,  deletionReasonO,  deletionReasonN) = generateTypeclasses(Text.DeletionReason)
  implicit val (    useCaseStepA,     useCaseStepO,     useCaseStepN) = generateTypeclasses(Text.UseCaseStep)
  implicit val (   useCaseTitleA,    useCaseTitleO,    useCaseTitleN) = generateTypeclasses(Text.UseCaseTitle)
}
