package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.univeq.UnivEq

final case class RuleRow(reqTypes     : String,
                         rule         : String,
                         default      : Option[String] = None,
                         deadReqTypes : Option[String] = None,
                         reqTypesError: Option[String] = None) {
  override def toString: String =
    s"RuleRow(${reqTypes.toString.replace("\n", "\\n")}, $rule, $default, $deadReqTypes, $reqTypesError)"
}

object RuleRow {

  def other(reqTypes     : String,
            rule         : String,
            default      : Option[String] = None,
            reqTypesError: Option[String] = None) =
    apply(
      reqTypes      = s"Other($reqTypes, ${ReqTypeRulesEditor.Internals.otherNew})",
      rule          = rule,
      default       = default,
      reqTypesError = reqTypesError,
    )

  implicit def univEq: UnivEq[RuleRow] = UnivEq.derive
}