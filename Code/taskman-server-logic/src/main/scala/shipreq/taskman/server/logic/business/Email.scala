package shipreq.taskman.server.logic.business

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.{Instant, ZoneId, ZoneOffset}
import shipreq.base.util.ScalaExt.StringBuilderExt
import shipreq.base.util.Util
import shipreq.taskman.api.{EmailAddr, UserId}
import shipreq.taskman.server.logic.{TaskDetail, TaskHeader}

object Email {

  final case class Addr(addr: EmailAddr, preParsed: Option[AnyRef] = None) {

    override def toString =
      preParsed match {
        case None    => s"Unparsed(${addr.value})"
        case Some(p) => s"${p.getClass.getSimpleName}($p)"
      }

    def tryParse[E, P](reuse: PartialFunction[AnyRef, P], parse: EmailAddr => E \/ P): E \/ P =
      preParsed match {
        case Some(p) if reuse.isDefinedAt(p) => \/-(reuse(p))
        case _                               => parse(addr)
      }
  }

  final case class EnvelopeProps(publicFrom: Addr, archiveAddrs: List[Addr])

  final case class TokenValues(shipreqName: String, loginUrl: String)

  final case class EnvelopeFront(to: NonEmptyVector[Addr], cc: List[Addr] = Nil, bcc: List[Addr] = Nil) {
    override def toString =
      Util.quickToString(getClass)(
        _.kv("to", to),
        _.kv("cc", cc, cc.nonEmpty),
        _.kv("bcc", bcc, bcc.nonEmpty))

    def from(from: Addr) = Envelope(from, to, cc, bcc)
  }

  final case class Envelope(from: Addr, to: NonEmptyVector[Addr], cc: List[Addr] = Nil, bcc: List[Addr] = Nil) {

    def showTo: String =
      to.iterator.map(_.addr.value).mkString(",")

    override def toString =
      Util.quickToString(getClass)(
        _.kv("from", from),
        _.kv("to", to),
        _.kv("cc", cc, cc.nonEmpty),
        _.kv("bcc", bcc, bcc.nonEmpty))
  }

  final case class Content(subject: String, body: String)

  private[business] object ContentUtil {

    val separator = "=" * 80

    final case class Info(data: Map[String, String]) extends AnyVal {

      def add[A](key: String, value: A)(implicit t: Info.ValueAdd[A]): Info =
        t.add(this, key, value)

      def addOption[A](key: String, value: Option[A])(implicit t: Info.ValueAdd[A]): Info =
        value match {
          case Some(v) => add(key, v)
          case None    => this
        }

      def addTask(v: TaskHeader): Info =
        add("task.id", v.id.value)

      def addTask(v: TaskDetail): Info =
        this
          .addTask(v.hdr)
          .add("task.failureCount", v.failureCount)
          .add("task.instance", v.task.toString)

      def addTask(v: Option[TaskDetail]): Info =
        v.fold(this)(addTask(_))

      def addUserId(id: UserId): Info =
        add("user.id", id.value.toString)

      def addUser(u: ShipReqUser): Info =
        addUserId(u.id)
          .add("user.email", u.email.value)
          .add("user.name", u.name)
          .add("user.username", u.username)

      def addUser(u: Option[ShipReqUser]): Info =
        u.fold(this)(addUser(_))

      def format: String = {
        val (itMulti, itSingle) = data.iterator.partition(_._2.contains('\n'))
        val single = MutableArray(itSingle.map { case (k, v) => s"$k = $v" }).sort.mkString("\n")
        val multi  = MutableArray(itMulti.map { case (k, v) => s"$k =\n${v.indentLines("  ")}" }).sort.mkString("\n\n")
        List(single, multi).filter(_.nonEmpty).mkString("\n\n")
      }
    }

    object Info {
      def empty: Info =
        apply(Map.empty)

      def create(kvs: (String, String)*): Info =
        apply(kvs.toMap)

      final case class ValueAdd[A](add: (Info, String, A) => Info) {
        def contramap[B](f: B => A): ValueAdd[B] =
          ValueAdd((i, k, v) => add(i, k, f(v)))
      }

      object ValueAdd {
        implicit val string : ValueAdd[String ] = apply((i, k, v) => Info(i.data.updated(k, v)))
        implicit val int    : ValueAdd[Int    ] = string.contramap(_.toString)
        implicit val long   : ValueAdd[Long   ] = string.contramap(_.toString)
        implicit val boolean: ValueAdd[Boolean] = string.contramap(_.toString)

        implicit val instant: ValueAdd[Instant] =
          apply((i, k, v) => i
            .add(k + ".utc", v.atOffset(ZoneOffset.UTC).toString)
            .add(k + ".local", v.atZone(ZoneId of "Australia/Melbourne").toString))
      }
    }

    def exceptionSubject(title: String, name: Option[String], message: Option[String]): String = {
      val f = (_: Option[String]).map(_.trim).filter(_.nonEmpty)
      val name2 = f(name)
      val message2 = f(message).filterNot(name2.contains)
      (name2, message2) match {
        case (None   , None   ) => title
        case (Some(n), None   ) => s"$title: $n"
        case (Some(n), Some(m)) => s"$title: $n: $m"
        case (None   , Some(m)) => s"$title: $m"
      }
    }
  }
}
