package shipreq.webapp.base.protocol2

import boopickle.Pickler

/**
  * An instance of a client-side procedure, available for invocation if the right client-JS is loaded.
  *
  * Input => Unit
  */
final case class ClientSideProc[Input](objectName: String)(implicit pi: Pickler[Input]) {
  implicit val pickler: Pickler[Input] = pi

  val objectAndMethod: String =
    s"$objectName.${ClientSideProc.MainMethodName}"
}

object ClientSideProc {
  final val MainMethodName = "m"
}