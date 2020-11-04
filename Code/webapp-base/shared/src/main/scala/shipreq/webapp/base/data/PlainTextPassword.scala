package shipreq.webapp.base.data

final case class PlainTextPassword(value: String) {
  def hashStr: String = "%08X".format(value.##)
}

object PlainTextPassword {
  implicit def univEq: UnivEq[PlainTextPassword] = UnivEq.derive
}
