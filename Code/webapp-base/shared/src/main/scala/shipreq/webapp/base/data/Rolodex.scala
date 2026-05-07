package shipreq.webapp.base.data

final case class Rolodex(asMap: Map[UserId.Public, Username]) {

  @inline def get(id: UserId.Public): Option[Username] =
    asMap.get(id)

  def need(id: UserId.Public): Username =
    asMap.get(id).getOrElse {
      val contents = asMap.map { case (id, username) => s"$id -> $username" }.mkString("\n")
      throw new IllegalStateException(s"Rolodex does not contain user ID: $id\nContents:\n$contents")
    }

  def add(id: UserId.Public, username: Username): Rolodex =
    Rolodex(asMap.updated(id, username))

  def ++(r: Rolodex): Rolodex =
    Rolodex(asMap ++ r.asMap)
}

object Rolodex {
  implicit def univEq: UnivEq[Rolodex] = UnivEq.derive

  def empty: Rolodex =
    apply(Map.empty)

  def init(id: UserId.Public, username: Username): Rolodex =
    apply(Map.empty[UserId.Public, Username].updated(id, username))
}
