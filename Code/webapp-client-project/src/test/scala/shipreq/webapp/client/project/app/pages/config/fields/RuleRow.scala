package shipreq.webapp.client.project.app.pages.config.fields

import shipreq.webapp.client.project.widgets.ReqTypeRulesEditor

final case class RuleRow(reqTypes     : String,
                         rule         : String,
                         default      : Option[String] = None,
                         defaultError : Boolean = false,
                         dead         : Boolean = false,
                         reqTypesError: Option[String] = None) {
  override def toString: String =
    s"RuleRow(${reqTypes.toString.replace("\n", "\\n")}, $rule, $default, $defaultError, $dead, $reqTypesError)"
}

object RuleRow {

  def other(reqTypes     : String,
            rule         : String,
            default      : Option[String] = None,
            defaultError : Boolean = false,
            reqTypesError: Option[String] = None) = {
    import ReqTypeRulesEditor.Internals.otherNew
    val typesDesc = if (reqTypes.isEmpty) otherNew else s"$reqTypes, $otherNew"
    apply(
      reqTypes      = s"Other($typesDesc)",
      rule          = rule,
      default       = default,
      defaultError  = defaultError,
      reqTypesError = reqTypesError,
    )
  }

  def all(rule         : String,
          default      : Option[String] = None,
          defaultError : Boolean = false,
          reqTypesError: Option[String] = None) =
    apply(
      reqTypes      = "All",
      rule          = rule,
      default       = default,
      defaultError  = defaultError,
      reqTypesError = reqTypesError,
    )

  val New = RuleRow("", "Optional", reqTypesError = Some("Cannot be blank."))

  val AllEmptyDefault = all("Default to…", default = Some(""), defaultError = true)

  implicit def univEq: UnivEq[RuleRow] = UnivEq.derive
}