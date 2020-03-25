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
            reqTypesError: Option[String] = None) = {
    import ReqTypeRulesEditor.Internals.otherNew
    val typesDesc = if (reqTypes.isEmpty) otherNew else s"$reqTypes, $otherNew"
    apply(
      reqTypes      = s"Other($typesDesc)",
      rule          = rule,
      default       = default,
      reqTypesError = reqTypesError,
    )
  }

  def all(rule         : String,
          default      : Option[String] = None,
          reqTypesError: Option[String] = None) =
    apply(
      reqTypes      = "All",
      rule          = rule,
      default       = default,
      reqTypesError = reqTypesError,
    )

  val New = RuleRow("", "Optional", reqTypesError = Some("Cannot be blank."))

  implicit def univEq: UnivEq[RuleRow] = UnivEq.derive
}