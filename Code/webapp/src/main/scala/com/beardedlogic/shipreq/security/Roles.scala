package shipreq.webapp.security

sealed case class Role(name: String) {
  if (!Roles.RoleNamePattern.pattern.matcher(name).matches)
    throw new IllegalStateException("Invalid role name: " + name)
}

object Roles {
  val RoleNamePattern = "^[a-z]+$".r

  val Admin = Role("admin")
}
