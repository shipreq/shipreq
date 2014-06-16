package shipreq.webapp.lib

import net.liftweb.http.SHtml
import net.liftweb.http.js.{JsExp, JsCmd, JsCmds}
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.feature.validation.{Validators, ValidationResult, Validator, CorrectionPart}
import shipreq.webapp.lib.Types.InputCorrected
import shipreq.webapp.security.PasswordAndSalt
import shipreq.webapp.util.JsExt.{JqSetValue, JqId}
import shipreq.webapp.util.HtmlTransformExt
import JsCmds.jsExpToJsCmd
import FormVar.CssSelF

/**
 * Representation of a datum or data in a HTML form.
 *
 * @tparam I (In) The type of the variable used to hold the data in-memory before it is analysed or processed.
 * @tparam O (Out) The type of the data after correction and successful validation.
 */
final case class FormVar[I, O](csssel: CssSelF[I], validate: I => ValidationResult[O]) {
  type Var = I
}

object FormVar {

  type CssSelF[I] = (=> I, (I => Unit)) => CssSel

  implicit class CssSelFExt[I](val f: CssSelF[I]) extends AnyVal {
    def scopeBy(sel: String): CssSelF[I] = (g, s) => sel #> f(g, s)
  }

  private object CS {
    private val checkedS = Some("checked")
    @inline private def checkedO(b: Boolean) = if (b) checkedS else None

    val strOnSubmit: CssSelF[String] = (get, set) => {
      val i = get
      ("*" #> SHtml.onSubmit(set)
        & HtmlTransformExt.renderOpString("input [value]", i)
        & "textarea *" #> i
      )
    }

    val booleanOnSubmit: CssSelF[Boolean] = (get, set) =>
      "*" #> SHtml.onSubmitBoolean(set) & "* [checked]" #> checkedO(get)

    def ajaxStr(cp: CorrectionPart[String, String], jsId: JsExp): CssSelF[String] = {
      def callback(set: String => Unit): String => JsCmd = i => {
        val c = cp.correct(i)
        set(c.value)
        updateJs(c)
      }
      def updateJs(v: InputCorrected[String]): JsCmd = jsId ~> JqSetValue(v.value)
      (get, set) => "*" #> SHtml.ajaxText(get, callback(set))
    }
  }

  // ===================================================================================================================

  def nop[T] = unvalidated[T]((_, _) => HtmlTransformExt.PassThru)

  private def unvalidated[A](csssel: CssSelF[A]) =
    FormVar[A, A](csssel, ValidationResult(_))

  private def validated[I, O](v: Validator[I, _, O])(csssel: CssSelF[I]) =
    FormVar[I, O](csssel, v.correctAndValidate)

  def ajaxStr[O](v: Validator[String, String, O], id: JqId): FormVar[String, O] =
    ajaxStr(v, "#" + id.id, id)

  def ajaxStr[O](v: Validator[String, String, O], sel: String, jsId: JsExp): FormVar[String, O] =
    validated(v)(CS.ajaxStr(v.cp, jsId) scopeBy sel)

  def strOnSubmit[O](v: Validator[String, _, O], sel: String): FormVar[String, O] =
    validated(v)(CS.strOnSubmit scopeBy sel)

  def boolOnSubmit[O](v: Validator[Boolean, _, O], sel: String): FormVar[Boolean, O] =
    validated(v)(CS.booleanOnSubmit scopeBy sel)

  def boolOnSubmit(sel: String): FormVar[Boolean, Boolean] =
    unvalidated(CS.booleanOnSubmit scopeBy sel)

  type PasswordPair = FormVar[(String, String), String]
  def passwordPair(selPw1: String, selPw2: String): PasswordPair =
    validated(Validators.passwords)((get, set) =>
      CS.strOnSubmit.scopeBy(selPw1)(get._1, x => set(get put1 x)) &
      CS.strOnSubmit.scopeBy(selPw2)(get._2, x => set(get put2 x)))

  val emptyPasswordPair = ("", "")
  val emptyPasswordChange = ("", emptyPasswordPair)

  def passwordChange(ps: PasswordAndSalt, selCur: String, pp: PasswordPair): FormVar[Validators.PasswordChange, String] =
    validated(Validators.passwordChange(ps))((get, set) =>
      CS.strOnSubmit.scopeBy(selCur)(get._1, x => set(get put1 x)) &
      pp.csssel                     (get._2, x => set(get put2 x))
    )

  def passwordChange(cur: Option[PasswordAndSalt], selCur: String, removeCurFieldCsssel: CssSel, pp: PasswordPair): FormVar[Validators.PasswordChange, String] =
    cur match {
      case Some(ps) => passwordChange(ps, selCur, pp)
      case None =>
        validated(Validators.passwordSet)((get, set) => removeCurFieldCsssel & pp.csssel(get._2, x => set(get put2 x)))
    }

  // ===================================================================================================================

  // Generated with bin/gen-formvar

  sealed trait Merge2[II, A, B] { def apply[X](f: (A, B) => X): FormVar[II, X] }
  final def merge[A,AO, B,BO](a: FormVar[A,AO], b: FormVar[B,BO]): Merge2[(A, B), AO, BO] = new Merge2[(A, B), AO, BO] {
    override def apply[X](fx: (AO, BO) => X): FormVar[(A, B), X] = FormVar[(A, B), X](
      (G,S) => a.csssel(G._1,x=>S(G put1 x)) & b.csssel(G._2,x=>S(G put2 x))
      ,v => Validator.Ap.apply2(a validate v._1, b validate v._2)(fx)
    )}

  sealed trait Merge3[II, A, B, C] { def apply[X](f: (A, B, C) => X): FormVar[II, X] }
  final def merge[A,AO, B,BO, C,CO](a: FormVar[A,AO], b: FormVar[B,BO], c: FormVar[C,CO]): Merge3[(A, B, C), AO, BO, CO] = new Merge3[(A, B, C), AO, BO, CO] {
    override def apply[X](fx: (AO, BO, CO) => X): FormVar[(A, B, C), X] = FormVar[(A, B, C), X](
      (G,S) => a.csssel(G._1,x=>S(G put1 x)) & b.csssel(G._2,x=>S(G put2 x)) & c.csssel(G._3,x=>S(G put3 x))
      ,v => Validator.Ap.apply3(a validate v._1, b validate v._2, c validate v._3)(fx)
    )}

  sealed trait Merge4[II, A, B, C, D] { def apply[X](f: (A, B, C, D) => X): FormVar[II, X] }
  final def merge[A,AO, B,BO, C,CO, D,DO](a: FormVar[A,AO], b: FormVar[B,BO], c: FormVar[C,CO], d: FormVar[D,DO]): Merge4[(A, B, C, D), AO, BO, CO, DO] = new Merge4[(A, B, C, D), AO, BO, CO, DO] {
    override def apply[X](fx: (AO, BO, CO, DO) => X): FormVar[(A, B, C, D), X] = FormVar[(A, B, C, D), X](
      (G,S) => a.csssel(G._1,x=>S(G put1 x)) & b.csssel(G._2,x=>S(G put2 x)) & c.csssel(G._3,x=>S(G put3 x)) & d.csssel(G._4,x=>S(G put4 x))
      ,v => Validator.Ap.apply4(a validate v._1, b validate v._2, c validate v._3, d validate v._4)(fx)
    )}

  sealed trait Merge5[II, A, B, C, D, E] { def apply[X](f: (A, B, C, D, E) => X): FormVar[II, X] }
  final def merge[A,AO, B,BO, C,CO, D,DO, E,EO](a: FormVar[A,AO], b: FormVar[B,BO], c: FormVar[C,CO], d: FormVar[D,DO], e: FormVar[E,EO]): Merge5[(A, B, C, D, E), AO, BO, CO, DO, EO] = new Merge5[(A, B, C, D, E), AO, BO, CO, DO, EO] {
    override def apply[X](fx: (AO, BO, CO, DO, EO) => X): FormVar[(A, B, C, D, E), X] = FormVar[(A, B, C, D, E), X](
      (G,S) => a.csssel(G._1,x=>S(G put1 x)) & b.csssel(G._2,x=>S(G put2 x)) & c.csssel(G._3,x=>S(G put3 x)) & d.csssel(G._4,x=>S(G put4 x)) & e.csssel(G._5,x=>S(G put5 x))
      ,v => Validator.Ap.apply5(a validate v._1, b validate v._2, c validate v._3, d validate v._4, e validate v._5)(fx)
    )}

  sealed trait Merge6[II, A, B, C, D, E, F] { def apply[X](f: (A, B, C, D, E, F) => X): FormVar[II, X] }
  final def merge[A,AO, B,BO, C,CO, D,DO, E,EO, F,FO](a: FormVar[A,AO], b: FormVar[B,BO], c: FormVar[C,CO], d: FormVar[D,DO], e: FormVar[E,EO], f: FormVar[F,FO]): Merge6[(A, B, C, D, E, F), AO, BO, CO, DO, EO, FO] = new Merge6[(A, B, C, D, E, F), AO, BO, CO, DO, EO, FO] {
    override def apply[X](fx: (AO, BO, CO, DO, EO, FO) => X): FormVar[(A, B, C, D, E, F), X] = FormVar[(A, B, C, D, E, F), X](
      (G,S) => a.csssel(G._1,x=>S(G put1 x)) & b.csssel(G._2,x=>S(G put2 x)) & c.csssel(G._3,x=>S(G put3 x)) & d.csssel(G._4,x=>S(G put4 x)) & e.csssel(G._5,x=>S(G put5 x)) & f.csssel(G._6,x=>S(G put6 x))
      ,v => Validator.Ap.apply6(a validate v._1, b validate v._2, c validate v._3, d validate v._4, e validate v._5, f validate v._6)(fx)
    )}

  sealed trait Merge7[II, A, B, C, D, E, F, G] { def apply[X](f: (A, B, C, D, E, F, G) => X): FormVar[II, X] }
  final def merge[A,AO, B,BO, C,CO, D,DO, E,EO, F,FO, G,GO](a: FormVar[A,AO], b: FormVar[B,BO], c: FormVar[C,CO], d: FormVar[D,DO], e: FormVar[E,EO], f: FormVar[F,FO], g: FormVar[G,GO]): Merge7[(A, B, C, D, E, F, G), AO, BO, CO, DO, EO, FO, GO] = new Merge7[(A, B, C, D, E, F, G), AO, BO, CO, DO, EO, FO, GO] {
    override def apply[X](fx: (AO, BO, CO, DO, EO, FO, GO) => X): FormVar[(A, B, C, D, E, F, G), X] = FormVar[(A, B, C, D, E, F, G), X](
      (G,S) => a.csssel(G._1,x=>S(G put1 x)) & b.csssel(G._2,x=>S(G put2 x)) & c.csssel(G._3,x=>S(G put3 x)) & d.csssel(G._4,x=>S(G put4 x)) & e.csssel(G._5,x=>S(G put5 x)) & f.csssel(G._6,x=>S(G put6 x)) & g.csssel(G._7,x=>S(G put7 x))
      ,v => Validator.Ap.apply7(a validate v._1, b validate v._2, c validate v._3, d validate v._4, e validate v._5, f validate v._6, g validate v._7)(fx)
    )}

  sealed trait Merge8[II, A, B, C, D, E, F, G, H] { def apply[X](f: (A, B, C, D, E, F, G, H) => X): FormVar[II, X] }
  final def merge[A,AO, B,BO, C,CO, D,DO, E,EO, F,FO, G,GO, H,HO](a: FormVar[A,AO], b: FormVar[B,BO], c: FormVar[C,CO], d: FormVar[D,DO], e: FormVar[E,EO], f: FormVar[F,FO], g: FormVar[G,GO], h: FormVar[H,HO]): Merge8[(A, B, C, D, E, F, G, H), AO, BO, CO, DO, EO, FO, GO, HO] = new Merge8[(A, B, C, D, E, F, G, H), AO, BO, CO, DO, EO, FO, GO, HO] {
    override def apply[X](fx: (AO, BO, CO, DO, EO, FO, GO, HO) => X): FormVar[(A, B, C, D, E, F, G, H), X] = FormVar[(A, B, C, D, E, F, G, H), X](
      (G,S) => a.csssel(G._1,x=>S(G put1 x)) & b.csssel(G._2,x=>S(G put2 x)) & c.csssel(G._3,x=>S(G put3 x)) & d.csssel(G._4,x=>S(G put4 x)) & e.csssel(G._5,x=>S(G put5 x)) & f.csssel(G._6,x=>S(G put6 x)) & g.csssel(G._7,x=>S(G put7 x)) & h.csssel(G._8,x=>S(G put8 x))
      ,v => Validator.Ap.apply8(a validate v._1, b validate v._2, c validate v._3, d validate v._4, e validate v._5, f validate v._6, g validate v._7, h validate v._8)(fx)
    )}

  sealed trait Merge9[II, A, B, C, D, E, F, G, H, I] { def apply[X](f: (A, B, C, D, E, F, G, H, I) => X): FormVar[II, X] }
  final def merge[A,AO, B,BO, C,CO, D,DO, E,EO, F,FO, G,GO, H,HO, I,IO](a: FormVar[A,AO], b: FormVar[B,BO], c: FormVar[C,CO], d: FormVar[D,DO], e: FormVar[E,EO], f: FormVar[F,FO], g: FormVar[G,GO], h: FormVar[H,HO], i: FormVar[I,IO]): Merge9[(A, B, C, D, E, F, G, H, I), AO, BO, CO, DO, EO, FO, GO, HO, IO] = new Merge9[(A, B, C, D, E, F, G, H, I), AO, BO, CO, DO, EO, FO, GO, HO, IO] {
    override def apply[X](fx: (AO, BO, CO, DO, EO, FO, GO, HO, IO) => X): FormVar[(A, B, C, D, E, F, G, H, I), X] = FormVar[(A, B, C, D, E, F, G, H, I), X](
      (G,S) => a.csssel(G._1,x=>S(G put1 x)) & b.csssel(G._2,x=>S(G put2 x)) & c.csssel(G._3,x=>S(G put3 x)) & d.csssel(G._4,x=>S(G put4 x)) & e.csssel(G._5,x=>S(G put5 x)) & f.csssel(G._6,x=>S(G put6 x)) & g.csssel(G._7,x=>S(G put7 x)) & h.csssel(G._8,x=>S(G put8 x)) & i.csssel(G._9,x=>S(G put9 x))
      ,v => Validator.Ap.apply9(a validate v._1, b validate v._2, c validate v._3, d validate v._4, e validate v._5, f validate v._6, g validate v._7, h validate v._8, i validate v._9)(fx)
    )}

}