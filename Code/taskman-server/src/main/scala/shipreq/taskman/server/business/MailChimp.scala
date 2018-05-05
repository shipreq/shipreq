package shipreq.taskman.server.business

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import okhttp3.Response
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import scalaz.syntax.std.option._
import scalaz.{-\/, \/, \/-, ~>}
import shipreq.base.util.ArticulateError
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.EmailAddr
import shipreq.taskman.server.logic.business.MailingList
import shipreq.taskman.server.logic.business.MailingList.API._
import shipreq.taskman.server.logic.business.MailingList._
import Http._
import MailChimp._

object MailChimp {

  /**
    * @param dc MailChimp data center
    */
  final case class Props(dc        : String,
                         key       : String,
                         masterList: String)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Protocol

  val i0 = JInt(0)
  val i1 = JInt(1)
  @inline def boolAsInt(b: Boolean) = if (b) i1 else i0

  def subscribeOptions(sendConfEmail: Boolean, updExisting: Boolean) =
    ("double_optin" -> sendConfEmail) ~
    ("update_existing" -> updExisting) ~
    ("send_welcome" -> false)

  val batchSubscribeStatic = subscribeOptions(false, true)

  def buildReqSubscription(s: Subscription) =
    ("email" ->
      ("email" -> s.addr.value)) ~
    ("merge_vars" ->
      ("NAME" -> s.name) ~
      ("NEWSLETTER" -> boolAsInt(s.newsletter)) ~
      ("ACCT" -> s.status.remoteValue))

  object Endpoints {
    def apply(props: Props): Endpoints =
      new Endpoints(s"https://${props.dc}.api.mailchimp.com/2.0", props.key)
  }

  final class Endpoints(urlPrefix: String, apiKey: String) {
    private val apiKeyJson = render("apikey" -> apiKey)

    private def endpoint(path: String): Http[JObject, JValue \/ JValue] =
      Post(s"$urlPrefix/$path.json")
        .jsonRequest
        .contramap[JObject](apiKeyJson merge _)
        .jsonResponse
        .and(_.flatMap {
          case x@ \/-(j) => Fx.lift(ApiFailure.Partial.extract(j) <\/ x)
          case x         => Fx pure x
        })

    object lists {

      val list: Http[GetListId, Option[ListId]] =
        endpoint("lists/list")
          .contramap[GetListId](i =>
            "filters" ->
              ("list_name" -> i.name) ~
              ("exact" -> true))
          .parseJsonResponse(
            ok = parseResponseForGetListId,
            ko = ApiFailure.Total.handle)

      val batchSubscribe: Http[BatchSubscribe, Unit] =
        endpoint("lists/batch-subscribe")
          .contramap[BatchSubscribe](i =>
            ("id" -> i.listId.value) ~
            ("batch" -> i.subs.list.map(buildReqSubscription)) ~
            batchSubscribeStatic)
          .parseJsonResponse(
            ok = _ => \/-(()),
            ko = ApiFailure.Total.handle)

      val subscribe: Http[Subscribe, SubscribeResult] =
        endpoint("lists/subscribe")
          .contramap[Subscribe](i =>
            ("id" -> i.listId.value) ~
            buildReqSubscription(i.sub) ~
            subscribeOptions(i.sendConfEmail, false))
          .parseJsonResponse(
            ok = _ => \/-(Ok),
            ko = parseErrorForSubscribe)

      val updateMember: Http[UpdateMember, UpdateMemberResult] =
        endpoint("lists/update-member")
          .contramap[UpdateMember](i =>
            ("id" -> i.listId.value) ~
            buildReqSubscription(i.sub))
          .parseJsonResponse(
            ok = _ => \/-(Ok),
            ko = ApiFailure.Total.recoverOrHandle(f =>
              Option.when(f.name ==* "Email_NotExists" || f.name ==* "List_NotSubscribed")(NotSubscribed)))

    }

  }

  def parseResponseForGetListId(j: JValue): ArticulateError \/ Option[ListId] =
    ArticulateError.attempt {
      val JInt(total) = j \ "total"
      total.toInt match {
        case 0 => None
        case 1 =>
          val JString(id) = (j \ "data") (0) \ "id"
          Some(ListId(id))
      }
    }

  def parseErrorForSubscribe: (Response, JValue) => Fx[SubscribeResult] =
    ApiFailure.Total.recoverOrHandle(f => Option.when(f.name ==* "List_AlreadySubscribed")(AlreadySubscribed))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Error handling

  object ApiFailure {

    final case class Total(code: Int, name: String, msg: String) {
      def toArticulateError: ArticulateError =
        ArticulateError("MailChimp API Failure")
          .hint(s"code = $code")
          .hint(s"name = $name")
          .hint(s"msg  = $msg")
    }

    object Total {
      def parse(j: JValue): ArticulateError \/ Total =
        ArticulateError.safe(
          (j \ "status") match {
            case JString("error") =>
              val JInt(code)    = j \ "code"
              val JString(name) = j \ "name"
              val JString(msg)  = j \ "error"
              \/-(Total(code.toInt, name, msg))
            case _ => -\/(ArticulateError("Not an error."))
          }
        )

      def recoverOrHandle[O](tryRecover: Total => Option[O])(resp: Response, j: JValue): Fx[O] =
        ApiFailure.Total.parse(j) match {
          case \/-(f) => tryRecover(f) match {
            case Some(o) => Fx pure o
            case None    => Fx fail f.toArticulateError
          }
          case -\/(_) => Http.genericResponseErrorHandler[O](resp, j)
        }

      def handle[O](resp: Response, j: JValue): Fx[O] =
        recoverOrHandle[O](_ => None)(resp, j)
    }

    final case class Partial(code: Int, msg: String, email: Option[EmailAddr]) {
      def fullMsg: String = {
        val emailPrefix = email.fold("")(e => s"$e: ")
        s"$emailPrefix[$code] $msg"
      }
    }

    object Partial {
      def toArticulateError(h: Partial, t: List[Partial]): ArticulateError =
        ArticulateError(s"${t.size + 1} partial MailChimp API failure(s) occurred.")
          .hint(h.fullMsg, t.map(_.fullMsg): _*)

      def parse(j: JValue): ArticulateError \/ List[Partial] =
        ArticulateError.attempt(
          for {
            JArray(errors) <- j \ "errors"
            e <- errors
          } yield {
            val JInt(code) = e \ "code"
            val JString(msg) = e \ "error"
            val opEmail = (e \ "email").toOption.map { i =>
              val JString(email) = i \ "email"
              EmailAddr(email)
            }
            Partial(code.toInt, msg, opEmail)
          }
        )

      /** Detects partial failures in a successful result and if present, returns a \/-(ArticulateError(…)) */
      def extract(j: JValue): Option[ArticulateError] =
        parse(j) match {
          case \/-(Nil)    => None
          case \/-(h :: t) => Some(toArticulateError(h, t))
          case -\/(e)      => Some(e)
        }
    }
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

final class MailChimp(props: Props)(implicit httpClient: HttpClient) extends (MailingList.API ~> Fx) with HasLogger {
  private implicit val httpLoggers: HttpLoggers =
    HttpLoggers(log, _.replace(props.key, "<KEY>"))

  private val endpoints: Endpoints =
    Endpoints(props)

  override def apply[A](api: API[A]): Fx[A] =
    api match {
      case a: GetListId      => endpoints.lists.list          .run(a)
      case a: BatchSubscribe => endpoints.lists.batchSubscribe.run(a)
      case a: Subscribe      => endpoints.lists.subscribe     .run(a)
      case a: UpdateMember   => endpoints.lists.updateMember  .run(a)
    }
}
