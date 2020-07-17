package shipreq.taskman.server.business

import io.circe.Json
import scala.runtime.AbstractFunction1
import scalaz.syntax.bind._
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.server.business.Http._
import shipreq.taskman.server.business.MailGun._
import shipreq.taskman.server.logic.business.BusinessOp
import shipreq.taskman.server.logic.business.BusinessOp.SendEmail

object MailGun {

  final case class Props(domain: String, apiKey: String, tags: Set[String]) {

    private[MailGun] val tagFields: List[(String, String)] =
      (tags + "taskman").iterator.map(("o:tag", _)).toList
  }

  final class Endpoints(props: Props) {

    val send: Http[SendEmail, Unit] =
      Post(s"https://api.mailgun.net/v3/${props.domain}/messages")
        .authWith(Credential.basic("api", props.apiKey))
        .requestAsForm[SendEmail](i =>
          "from" -> i.envelope.from.addr.value ::
          "subject" -> i.content.subject ::
          "text" -> i.content.body ::
          i.envelope.cc.map("cc" -> _.addr.value) :::
          i.envelope.bcc.map("bcc" -> _.addr.value) :::
          i.envelope.to.iterator.map("to" -> _.addr.value).toList :::
          props.tagFields
        )
      .responseAsJson[Json]
      .map(_ => \/-(()))
  }
}

final class MailGun(props: Props)(implicit httpClient: HttpClient)
    extends AbstractFunction1[BusinessOp.SendEmail, Fx[Unit]] with HasLogger {

  private implicit val httpLogger: HttpLogger =
    HttpLogger(logger)

  private val endpoints = new Endpoints(props)

  override def apply(op: SendEmail): Fx[Unit] =
    endpoints.send.run(op) >>
      Fx(logger.info(s"Email sent: ${op.envelope.showTo} [${op.content.subject}]"))
}
