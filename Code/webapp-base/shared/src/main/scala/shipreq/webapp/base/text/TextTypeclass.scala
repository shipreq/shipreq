package shipreq.webapp.base.text

import shipreq.base.util.NonEmptyVector

trait AtomTC[TC[_]] {

  def lazily[A](a: => TC[A]): TC[A]

  def vec[A](implicit a: TC[A]): TC[Vector[A]]
  def nev[A](as: TC[Vector[A]])(implicit a: TC[A]): TC[NonEmptyVector[A]]

  def sum[T <: Atom.Base](t: T)(f: t.Atom => TC[t.Atom], all: Vector[TC[t.Atom]]): TC[t.Atom]

  def blankLine    [T <: Atom.NewLine        ](t: T): TC[t.BlankLine   ]
  def literal      [T <: Atom.Literal        ](t: T): TC[t.Literal     ]
  def webAddress   [T <: Atom.PlainTextMarkup](t: T): TC[t.WebAddress  ]
  def emailAddress [T <: Atom.PlainTextMarkup](t: T): TC[t.EmailAddress]
  def mathTeX      [T <: Atom.PlainTextMarkup](t: T): TC[t.MathTeX     ]
  def reqRef       [T <: Atom.ReqRef         ](t: T): TC[t.ReqRef      ]
  def codeRef      [T <: Atom.ReqRef         ](t: T): TC[t.CodeRef     ]
  def tagRef       [T <: Atom.TagRef         ](t: T): TC[t.TagRef      ]

  def issue        [T <: Atom.Issue     ](t: T)(implicit x: TC[Text.InlineIssueDesc.OptionalText]): TC[t.Issue]
  def unorderedList[T <: Atom.ListMarkup](t: T)(implicit x: TC[NonEmptyVector[t.ListItem]])       : TC[t.UnorderedList]

  final val instances = TextTC[TC](this)
}

object TextTC {
  def apply[TC[_]](a: AtomTC[TC]): TextTC[TC] =
    new TextTC(a)
}

class TextTC[TC[_]](a: AtomTC[TC]) {

  private def forIssue: (TC[Text.InlineIssueDesc.Atom], TC[Text.InlineIssueDesc.OptionalText], TC[Text.InlineIssueDesc.NonEmptyText]) = {
    val t = Text.InlineIssueDesc

//    val blankLine     = a.blankLine    (t)
    val literal       = a.literal      (t)    .asInstanceOf[TC[t.Atom]]
    val webAddress    = a.webAddress   (t)    .asInstanceOf[TC[t.Atom]]
    val emailAddress  = a.emailAddress (t)    .asInstanceOf[TC[t.Atom]]
    val mathTeX       = a.mathTeX      (t)    .asInstanceOf[TC[t.Atom]]
    val reqRef        = a.reqRef       (t)    .asInstanceOf[TC[t.Atom]]
    val codeRef       = a.codeRef      (t)    .asInstanceOf[TC[t.Atom]]
//    val tagRef        = a.tagRef       (t)

    //lazy val issue         = a.issue        (t)// [T <: Atom.Issue     ](t: T)(implicit x: TC[Text.InlineIssueDesc.OptionalText]): TC[t.Issue]
    //lazy val unorderedList = a.unorderedList(t)// [T <: Atom.ListMarkup](t: T)(implicit x: TC[NonEmptyVector[t.ListItem]])       : TC[t.UnorderedList]

    val all = Vector[TC[t.Atom]](
      literal     ,
      webAddress  ,
      emailAddress,
      mathTeX     ,
      reqRef      ,
      codeRef     )

    val choose: t.Atom => TC[t.Atom] = {
      case _: t.Literal      => literal
      case _: t.WebAddress   => webAddress
      case _: t.EmailAddress => emailAddress
      case _: t.MathTeX      => mathTeX
      case _: t.ReqRef       => reqRef
      case _: t.CodeRef      => codeRef
    }

    val atom = a.sum(t)(choose, all)
    val vec  = a.vec(atom)
    val nev  = a.nev(vec)(atom)

    (atom, vec, nev)
  }

  private def forCustomTextField : (TC[Text.CustomTextField.Atom], TC[Text.CustomTextField.OptionalText], TC[Text.CustomTextField.NonEmptyText]) = {
    val t = Text.CustomTextField

    val blankLine     = a.blankLine    (t)    .asInstanceOf[TC[t.Atom]]
    val literal       = a.literal      (t)    .asInstanceOf[TC[t.Atom]]
    val webAddress    = a.webAddress   (t)    .asInstanceOf[TC[t.Atom]]
    val emailAddress  = a.emailAddress (t)    .asInstanceOf[TC[t.Atom]]
    val mathTeX       = a.mathTeX      (t)    .asInstanceOf[TC[t.Atom]]
    val reqRef        = a.reqRef       (t)    .asInstanceOf[TC[t.Atom]]
    val codeRef       = a.codeRef      (t)    .asInstanceOf[TC[t.Atom]]
    val tagRef        = a.tagRef       (t)    .asInstanceOf[TC[t.Atom]]

    lazy val issue         = a.issue        (t)(issue3._2) .asInstanceOf[TC[t.Atom]]
    lazy val unorderedList: TC[t.Atom] = a.unorderedList(t)(li)        .asInstanceOf[TC[t.Atom]]

    lazy val all = Vector[TC[t.Atom]](
      blankLine   ,
      literal     ,
      webAddress  ,
      emailAddress,
      mathTeX     ,
      reqRef      ,
      codeRef     ,
      tagRef      ,
      issue       ,
      unorderedList)

    lazy val choose: t.Atom => TC[t.Atom] = {
      case _: t.BlankLine     => blankLine
      case _: t.Literal       => literal
      case _: t.WebAddress    => webAddress
      case _: t.EmailAddress  => emailAddress
      case _: t.MathTeX       => mathTeX
      case _: t.ReqRef        => reqRef
      case _: t.CodeRef       => codeRef
      case _: t.TagRef        => tagRef
      case _: t.Issue         => issue
      case _: t.UnorderedList => unorderedList
    }

    lazy val atom = a.sum(t)(choose, all)
    lazy val vec  = a.vec(atom)
    lazy val li: TC[NonEmptyVector[t.ListItem]] = a.lazily(a.nev(a vec vec)(vec))
    val nev  = a.nev(vec)(atom)

    (atom, vec, nev)
  }

  private def forGenericReqTitle: (TC[Text.GenericReqTitle.Atom], TC[Text.GenericReqTitle.OptionalText], TC[Text.GenericReqTitle.NonEmptyText]) = {
    val t = Text.GenericReqTitle

//    val blankLine     = a.blankLine    (t)    .asInstanceOf[TC[t.Atom]]
    val literal       = a.literal      (t)    .asInstanceOf[TC[t.Atom]]
    val webAddress    = a.webAddress   (t)    .asInstanceOf[TC[t.Atom]]
    val emailAddress  = a.emailAddress (t)    .asInstanceOf[TC[t.Atom]]
    val mathTeX       = a.mathTeX      (t)    .asInstanceOf[TC[t.Atom]]
    val reqRef        = a.reqRef       (t)    .asInstanceOf[TC[t.Atom]]
    val codeRef       = a.codeRef      (t)    .asInstanceOf[TC[t.Atom]]
    val tagRef        = a.tagRef       (t)    .asInstanceOf[TC[t.Atom]]

    val issue         = a.issue        (t)(issue3._2) .asInstanceOf[TC[t.Atom]]
//    lazy val unorderedList = a.unorderedList(t)(li)        .asInstanceOf[TC[t.Atom]]

    val all = Vector[TC[t.Atom]](
//      blankLine   ,
      literal     ,
      webAddress  ,
      emailAddress,
      mathTeX     ,
      reqRef      ,
      codeRef     ,
      tagRef      ,
      issue       )
//      unorderedList)

    val choose: t.Atom => TC[t.Atom] = {
//      case _: t.BlankLine     => blankLine
      case _: t.Literal       => literal
      case _: t.WebAddress    => webAddress
      case _: t.EmailAddress  => emailAddress
      case _: t.MathTeX       => mathTeX
      case _: t.ReqRef        => reqRef
      case _: t.CodeRef       => codeRef
      case _: t.TagRef        => tagRef
      case _: t.Issue         => issue
//      case _: t.UnorderedList => unorderedList
    }

    val atom = a.sum(t)(choose, all)
    val vec  = a.vec(atom)
//    lazy val li   = a.nev(a vec vec)(vec)
    val nev  = a.nev(vec)(atom)

    (atom, vec, nev)
  }


  private def forReqCodeGroupTitle: (TC[Text.ReqCodeGroupTitle.Atom], TC[Text.ReqCodeGroupTitle.OptionalText], TC[Text.ReqCodeGroupTitle.NonEmptyText]) = {
    val t = Text.ReqCodeGroupTitle

    //    val blankLine     = a.blankLine    (t)    .asInstanceOf[TC[t.Atom]]
    val literal       = a.literal      (t)    .asInstanceOf[TC[t.Atom]]
    val webAddress    = a.webAddress   (t)    .asInstanceOf[TC[t.Atom]]
    val emailAddress  = a.emailAddress (t)    .asInstanceOf[TC[t.Atom]]
    val mathTeX       = a.mathTeX      (t)    .asInstanceOf[TC[t.Atom]]
    val reqRef        = a.reqRef       (t)    .asInstanceOf[TC[t.Atom]]
    val codeRef       = a.codeRef      (t)    .asInstanceOf[TC[t.Atom]]
//    val tagRef        = a.tagRef       (t)    .asInstanceOf[TC[t.Atom]]

    val issue         = a.issue        (t)(issue3._2) .asInstanceOf[TC[t.Atom]]
    //    lazy val unorderedList = a.unorderedList(t)(li)        .asInstanceOf[TC[t.Atom]]

    val all = Vector[TC[t.Atom]](
      //      blankLine   ,
      literal     ,
      webAddress  ,
      emailAddress,
      mathTeX     ,
      reqRef      ,
      codeRef     ,
//      tagRef      ,
      issue       )
    //      unorderedList)

    val choose: t.Atom => TC[t.Atom] = {
      //      case _: t.BlankLine     => blankLine
      case _: t.Literal       => literal
      case _: t.WebAddress    => webAddress
      case _: t.EmailAddress  => emailAddress
      case _: t.MathTeX       => mathTeX
      case _: t.ReqRef        => reqRef
      case _: t.CodeRef       => codeRef
//      case _: t.TagRef        => tagRef
      case _: t.Issue         => issue
      //      case _: t.UnorderedList => unorderedList
    }

    val atom = a.sum(t)(choose, all)
    val vec  = a.vec(atom)
    //    lazy val li   = a.nev(a vec vec)(vec)
    val nev  = a.nev(vec)(atom)

    (atom, vec, nev)
  }

  private lazy val issue3 = forIssue

  implicit val (inlineIssueDescA,   inlineIssueDescO,   inlineIssueDescN)   = issue3
  implicit val (genericReqTitleA,   genericReqTitleO,   genericReqTitleN)   = forGenericReqTitle
  implicit val (customTextFieldA,   customTextFieldO,   customTextFieldN)   = forCustomTextField
  implicit val (reqCodeGroupTitleA, reqCodeGroupTitleO, reqCodeGroupTitleN) = forReqCodeGroupTitle
}
