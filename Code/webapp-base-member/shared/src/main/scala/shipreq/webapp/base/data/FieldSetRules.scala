package shipreq.webapp.base.data

import japgolly.univeq.UnivEq
import FieldReqTypeRules.Resolution

final class FieldSetRules(val imp : CustomField.Implication.Id => Resolution.ForImpField,
                          val tag : CustomField.Tag        .Id => Resolution.ForTagField,
                          val text: CustomField.Text       .Id => Resolution.ForTextField,
                         ) {

  def apply(id: CustomFieldId): Resolution[Any] =
    id match {
      case i: CustomField.Implication.Id => imp(i)
      case i: CustomField.Tag.Id         => tag(i)
      case i: CustomField.Text.Id        => text(i)
    }

  override def hashCode =
    (imp.hashCode + tag.hashCode * 31) * 31 + text.hashCode

  override def equals(obj: Any): Boolean =
    obj match {
      case f: FieldSetRules => (this eq f) || ((imp eq f.imp) && (tag eq f.tag) && (text eq f.text))
      case _                => false
    }
}

object FieldSetRules {

  def apply(imp : CustomField.Implication.Id => Resolution.ForImpField,
            tag : CustomField.Tag        .Id => Resolution.ForTagField,
            text: CustomField.Text       .Id => Resolution.ForTextField): FieldSetRules =
    new FieldSetRules(imp, tag, text)

  def const(r: Resolution[Nothing]): FieldSetRules = {
    val f = (_: Any) => r
    apply(f, f, f)
  }

  val optional: FieldSetRules =
    const(Resolution.Optional)

  implicit def univEq: UnivEq[FieldSetRules] = UnivEq.force
}