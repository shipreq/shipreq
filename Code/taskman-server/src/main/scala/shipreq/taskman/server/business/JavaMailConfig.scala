package shipreq.taskman.server.business

import cats.syntax.all._
import japgolly.clearconfig._
import java.util.Properties
import javax.mail.{Authenticator, PasswordAuthentication, Session}

object JavaMailConfig {

  private def get[A](n1: String)(implicit prefix: Prefix, p: ConfigValueParser[A]): ConfigDef[Properties => Unit] = {
    val n = prefix.value + n1
    ConfigDef.get(n)(p).map(oa => p => oa.foreach(a => p.setProperty(n, a.toString)))
  }

  private case class Prefix(value: String)

  // https://javamail.java.net/nonav/docs/api/com/sun/mail/smtp/package-summary.html
  private def main: ConfigDef[Properties => Unit] = {
    implicit val prefix = Prefix("mail.")
    ConfigDef.mergeConsumerFns(
      get[Boolean]("debug"),
      get[String]("from"),
      get[String]("host"),
      get[Boolean]("mime.address.strict"),
      get[String]("store.protocol"),
      get[String]("transport.protocol"),
      get[String]("user"))
  }

  // https://javamail.java.net/nonav/docs/api/com/sun/mail/smtp/package-summary.html
  private def smtp(implicit prefix: Prefix): ConfigDef[Properties => Unit] =
    ConfigDef.mergeConsumerFns(
      get[Boolean]("allow8bitmime"),
      get[Boolean]("auth"),
      get[Boolean]("auth.digest-md5.disable"),
      get[Boolean]("auth.login.disable"),
      get[String]("auth.mechanisms"),
      get[Boolean]("auth.ntlm.disable"),
      get[String]("auth.ntlm.domain"),
      get[Int]("auth.ntlm.flags"),
      get[Boolean]("auth.plain.disable"),
      get[Boolean]("auth.xoauth2.disable"),
      get[String]("class"),
      get[Int]("connectiontimeout"),
      get[String]("dsn.notify"),
      get[String]("dsn.ret"),
      get[Boolean]("ehlo"),
      get[String]("from"),
      get[String]("host"),
      get[String]("localaddress"),
      get[String]("localhost"),
      get[Int]("localport"),
      get[String]("mailextension"),
      get[Boolean]("noop.strict"),
      get[Int]("port"),
      get[Boolean]("quitwait"),
      get[Boolean]("reportsuccess"),
      get[String]("sasl.authorizationid"),
      get[Boolean]("sasl.enable"),
      get[String]("sasl.mechanisms"),
      get[String]("sasl.realm"),
      get[Boolean]("sasl.usecanonicalhostname"),
      get[Boolean]("sendpartial"),
      get[String]("socketFactory.class"),
      get[Boolean]("socketFactory.fallback"),
      get[Int]("socketFactory.port"),
      get[String]("socks.host"),
      get[String]("socks.port"),
      get[Boolean]("ssl.checkserveridentity"),
      get[String]("ssl.ciphersuites"),
      get[Boolean]("ssl.enable"),
      get[String]("ssl.protocols"),
      get[String]("ssl.socketFactory.class"),
      get[Int]("ssl.socketFactory.port"),
      get[String]("ssl.trust"),
      get[Boolean]("starttls.enable"),
      get[Boolean]("starttls.required"),
      get[String]("submitter"),
      get[Int]("timeout"),
      get[String]("user"),
      get[Boolean]("userset"),
      get[Int]("writetimeout")
    )

  def props: ConfigDef[Properties] =
    ConfigDef.mergeConsumerFns(
      smtp(Prefix("mail.smtp.")),
      // smtp(Prefix("mail.smtps.")),
      main
    ).map { fn =>
      val p = new Properties()
      fn(p)
      p
    }

  def passwordAuthentication: ConfigDef[Option[PasswordAuthentication]] =
    (ConfigDef.get[String]("mail.user"), ConfigDef.get[String]("mail.password"))
      .tupled
      .mapAttempt[Option[PasswordAuthentication]] {
        case (Some(u), Some(p)) => \/-(Some(new PasswordAuthentication(u, p)))
        case (None, Some(_)) => -\/("Username not specified.")
        case (Some(_), None) => -\/("Password not specified.")
        case (None, None) => \/-(None)
      }

  def authenticator: ConfigDef[Option[Authenticator]] =
    passwordAuthentication.map(_.map(a =>
      new Authenticator {
        override def getPasswordAuthentication = a
      }
    ))

  def sessionFn: ConfigDef[() => Session] =
    (props, authenticator).mapN ((p, oa) => () => Session.getInstance(p, oa.orNull))

}
