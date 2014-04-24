package shipreq.webapp.lib

import net.liftweb.http.SHtml
import net.liftweb.http.js.{JsExp, JsCmd, JsCmds}
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._
import scalaz.Success
import shipreq.webapp.feature.validation.{Validators, ValidationResult, Validator, CorrectionPart}
import shipreq.webapp.lib.Types.{InputCorrected, @@}
import shipreq.webapp.util.JsExt.{JqSetValue, JqId}
import shipreq.webapp.util.HtmlTransformExt
import JsCmds.jsExpToJsCmd

trait FormVar[I] {
  def value: I
  def set(value: I): Unit
  def csssel: CssSel
}

trait FormVarV[I, O] extends FormVar[I] {
  def validate: ValidationResult[O]
}

case class FormVar2[A, B](a: FormVar[A], b: FormVar[B]) extends FormVar[(A, B)] {
  override def value = (a.value, b.value)
  override def csssel = a.csssel & b.csssel
  override def set(value: (A,B)): Unit = {
    a.set(value._1)
    b.set(value._2)
  }
  def set2(value: A)(implicit ev: A =:= B): Unit = {
    a.set(value)
    b.set(value)
  }
}

// =====================================================================================================================

object FormVar {

  trait VarListener[T] {
    def apply(initialValue: T, updateVar: T => Unit): CssSel
  }

  object VarListener {

    object StrOnSubmit extends VarListener[String] {
      override def apply(initialValue: String, updateVar: String => Unit): CssSel = (
        "*" #> SHtml.onSubmit(updateVar)
        & HtmlTransformExt.renderOpString("input [value]", initialValue)
        & "textarea *" #> initialValue
      )
    }

    object BooleanOnSubmit extends VarListener[Boolean] {
      val checked = Some("checked")
      override def apply(initialValue: Boolean, updateVar: Boolean => Unit): CssSel =
        "*" #> SHtml.onSubmitBoolean(updateVar) & "* [checked]" #> (if (initialValue) checked else None)
    }

    final class AjaxStr(v: CorrectionPart[String, String], jsId: JsExp) extends VarListener[String] {
      override def apply(initialValue: String, updateVar: String => Unit): CssSel =
        "*" #> SHtml.ajaxText(initialValue, callback(updateVar))

      private def callback(updateVar: String @@ InputCorrected => Unit): String => JsCmd =
        i => {
          val c = v.correct(i)
          updateVar(c)
          updateJs(c)
        }

      private def updateJs(v: String @@ InputCorrected): JsCmd =
        jsId ~> JqSetValue(v)
    }
  }

  final class Basic[I](initialValue: I, sel: String, listener: VarListener[I]) extends FormVar[I] {
    private[this] var v = initialValue
    override final def value = v
    override final def set(value: I): Unit = v = value
    override def csssel = sel #> listener(value, set)
  }

  class Delegate[I](fv: FormVar[I]) extends FormVar[I] {
    override def set(value: I): Unit = fv set value
    override def value = fv.value
    override def csssel = fv.csssel
  }

  final class Validated[I, O, FV <: FormVar[I]](val validator: Validator[I, _, O], val fv: FV) extends Delegate(fv) with FormVarV[I, O] {
    def validate: ValidationResult[O] = validator.correctAndValidate(fv.value)
  }

  final class Unvalidated[T](val fv: FormVar[T]) extends Delegate(fv) with FormVarV[T, T] {
    def validate: ValidationResult[T] = Success(fv.value)
  }

  // ===================================================================================================================

  import VarListener._

  def ajaxStr[O](v: Validator[String, String, O], id: JqId)(initialValue: String): FormVarV[String, O] =
    ajaxStr(v, "#" + id.id, id)(initialValue)

  def ajaxStr[O](v: Validator[String, String, O], sel: String, jsId: JsExp)(initialValue: String): FormVarV[String, O] = {
    val l = new AjaxStr(v, jsId)
    val fv = new Basic(initialValue, sel, l)
    new Validated(v, fv)
  }

  def strOnSubmit[O](v: Validator[String, _, O], sel: String)(initialValue: String): FormVarV[String, O] =
    new Validated(v, new Basic(initialValue, sel, StrOnSubmit))

  def boolOnSubmit[O](v: Validator[Boolean, _, O], sel: String)(initialValue: Boolean): FormVarV[Boolean, O] =
    new Validated(v, new Basic(initialValue, sel, BooleanOnSubmit))

  def boolOnSubmit(sel: String)(initialValue: Boolean): FormVarV[Boolean, Boolean] =
    new Unvalidated(new Basic(initialValue, sel, BooleanOnSubmit))

  def passwordPair(selPw1: String, selPw2: String) = {
    val a = new Basic("", selPw1, StrOnSubmit)
    val b = new Basic("", selPw2, StrOnSubmit)
    new Validated(Validators.passwords, FormVar2(a, b))
  }

  // ===================================================================================================================
  
  implicit def FormVarV[O](fv: FormVarV[_, O]): ValidationResult[O] = fv.validate

  case class AP2[A,B](a: FormVarV[_,A], b: FormVarV[_,B]) {
    def csssel = a.csssel & b.csssel
    def validate[X](f: (A,B) => X): ValidationResult[X] = Validator.Ap.apply2(a,b)(f)
  }
  
  case class AP3[A,B,C](a: FormVarV[_,A], b: FormVarV[_,B], c: FormVarV[_,C]) {
    def csssel = a.csssel & b.csssel & c.csssel
    def validate[X](f: (A,B,C) => X): ValidationResult[X] = Validator.Ap.apply3(a,b,c)(f)
  }

  case class AP4[A,B,C,D](a: FormVarV[_,A], b: FormVarV[_,B], c: FormVarV[_,C], d: FormVarV[_,D]) {
    def csssel = a.csssel & b.csssel & c.csssel & d.csssel
    def validate[X](f: (A,B,C,D) => X): ValidationResult[X] = Validator.Ap.apply4(a,b,c,d)(f)
  }

  case class AP5[A,B,C,D,E](a: FormVarV[_,A], b: FormVarV[_,B], c: FormVarV[_,C], d: FormVarV[_,D], e: FormVarV[_,E]) {
    def csssel = a.csssel & b.csssel & c.csssel & d.csssel & e.csssel
    def validate[X](f: (A,B,C,D,E) => X): ValidationResult[X] = Validator.Ap.apply5(a,b,c,d,e)(f)
  }

  case class AP6[A,B,C,D,E,F](a: FormVarV[_,A], b: FormVarV[_,B], c: FormVarV[_,C], d: FormVarV[_,D], e: FormVarV[_,E], f: FormVarV[_,F]) {
    def csssel = a.csssel & b.csssel & c.csssel & d.csssel & e.csssel & f.csssel
    def validate[X](x: (A,B,C,D,E,F) => X): ValidationResult[X] = Validator.Ap.apply6(a,b,c,d,e,f)(x)
  }
}
