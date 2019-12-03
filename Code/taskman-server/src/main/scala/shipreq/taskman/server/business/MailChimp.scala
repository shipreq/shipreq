package shipreq.taskman.server.business

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import io.circe._
import io.circe.syntax._
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
                         key       : ApiKey,
                         masterList: String)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Protocol

  private[this] val i0 = 0.asJson
  private[this] val i1 = 1.asJson
  @inline private def boolAsInt(b: Boolean) = if (b) i1 else i0

  private def subscribeOptions(sendConfEmail: Boolean, updExisting: Boolean): List[(String, Json)] =
    ("double_optin" -> sendConfEmail.asJson) ::
    ("update_existing" -> updExisting.asJson) ::
    ("send_welcome" -> false.asJson) ::
    Nil

  private val batchSubscribeStatic = subscribeOptions(false, true)

  private def buildReqSubscription(s: Subscription): List[(String, Json)] = {
    val email = JsonObject(
      "email" -> s.addr.value.asJson,
    )

    val mergeVars = JsonObject(
      "NAME"       -> s.name.asJson,
      "NEWSLETTER" -> boolAsInt(s.newsletter),
      "ACCT"       -> s.status.remoteValue.asJson,
    )

    ("email" -> Json.fromJsonObject(email)) ::
    ("merge_vars" -> Json.fromJsonObject(mergeVars)) ::
      Nil
  }

  final case class ApiKey(value: String)

  object Endpoints {
    def apply(props: Props): Endpoints =
      new Endpoints(s"https://${props.dc}.api.mailchimp.com/2.0", props.key)
  }

  final class Endpoints(urlPrefix: String, apiKey: ApiKey) {

    private def endpoint[I, O](path: String)
                              (ko: Json => Fx[O])
                              (implicit encoderI: Encoder.AsObject[I],
                               decoderO: Decoder[O]): Http[I, O] = {
      val apiKeyJson = apiKey.value.asJson

      val encoder: Encoder[I] =
        Encoder.instance { i =>
          Json.fromJsonObject(encoderI.encodeObject(i).add("apikey", apiKeyJson))
        }

      val decoder: Decoder[ArticulateError \/ O] =
        Decoder.instance { c =>
          ApiFailure.Partial.decoderOption(c).flatMap {
            case None    => decoderO(c).map(\/-(_))
            case Some(f) => Right(-\/(f))
          }
        }

      Post(s"$urlPrefix/$path.json")
        .requestAsJson(encoder)
        .responseAsJsonOrJson(decoder)
        .and(_.flatMap {
          case \/-(\/-(o)) => Fx.pure(o)
          case \/-(-\/(e)) => Fx.fail(e)
          case -\/(j)      => ko(j)
        })
    }

    object lists {

      val list: Http[GetListId, Option[ListId]] = {
        implicit val enc = Encoder.AsObject.instance[GetListId](i =>
          JsonObject(
            "filters" -> Json.obj(
              ("list_name" -> i.name.asJson),
              ("exact" -> true.asJson))))

        implicit val dec = decoderGetListIdResponse

        endpoint("lists/list")(ApiFailure.Total.handle)
      }

      val batchSubscribe: Http[BatchSubscribe, Unit] = {
        implicit val enc = Encoder.AsObject.instance[BatchSubscribe](i =>
          JsonObject.fromIterable(
            ("id" -> i.listId.value.asJson) ::
              ("batch" -> Json.arr(i.subs.list.map(s => Json.obj(buildReqSubscription(s): _*)): _*)) ::
              batchSubscribeStatic))

        implicit val dec = decoderBatchSubscribe

        endpoint("lists/batch-subscribe")(ApiFailure.Total.handle)
      }

      val subscribe: Http[Subscribe, SubscribeResult] = {
        implicit val enc = Encoder.AsObject.instance[Subscribe](i =>
          JsonObject.fromIterable(
            ("id" -> i.listId.value.asJson) ::
              subscribeOptions(i.sendConfEmail, false) :::
              buildReqSubscription(i.sub)))

        implicit val dec = decoderSubscribe

        endpoint("lists/subscribe")(parseErrorForSubscribe)
      }

      val updateMember: Http[UpdateMember, UpdateMemberResult] = {
        implicit val enc = Encoder.AsObject.instance[UpdateMember](i =>
          JsonObject.fromIterable(
            ("id" -> i.listId.value.asJson) ::
              buildReqSubscription(i.sub)))

        implicit val dec = decoderUpdateMember

        endpoint("lists/update-member")(
          ApiFailure.Total.recoverOrHandle(f =>
            Option.when(f.name ==* "Email_NotExists" || f.name ==* "List_NotSubscribed")(NotSubscribed)))
      }
    }
  }

  private[business] def decoderBatchSubscribe: Decoder[Unit] =
    Decoder.const(())

  private[business] def decoderSubscribe: Decoder[SubscribeResult] =
    Decoder.const(Ok)

  private[business] def decoderUpdateMember: Decoder[UpdateMemberResult] =
    Decoder.const(Ok)

  private[business] def decoderGetListIdResponse: Decoder[Option[ListId]] =
    Decoder.instance { c =>
      val cTotal = c.downField("total")
      cTotal.as[Int].flatMap {
        case 0 => Right(None)
        case 1 => c.downField("data").downArray.downField("id").as[String].map(s => Some(ListId(s)))
        case _ => Left(DecodingFailure("Expected 0 or 1", cTotal.history))
      }
    }

  private[business] def parseErrorForSubscribe: Json => Fx[SubscribeResult] =
    ApiFailure.Total.recoverOrHandle(f => Option.when(f.name ==* "List_AlreadySubscribed")(AlreadySubscribed))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Error handling

  private[business] object ApiFailure {

    /** Sample:
      * {{{
      *   {
      *     "status": "error",
      *     "code": 553,
      *     "name": "Invalid_PagingLimit",
      *     "error": "Page Limit Number must be greater than or equal to 0"
      *   }
      * }}}
      */
    final case class Total(code: Int, name: String, msg: String) {
      def toArticulateError: ArticulateError =
        ArticulateError("MailChimp API Failure")
          .hint(s"code = $code")
          .hint(s"name = $name")
          .hint(s"msg  = $msg")
    }

    object Total {

      implicit def univEq: UnivEq[Total] = UnivEq.derive

      implicit val decoder: Decoder[Total] =
        Decoder.instance { c =>
          for {
            code  <- c.get[Int]("code")
            name  <- c.get[String]("name")
            error <- c.get[String]("error")
          } yield Total(code, name, error)
        }

      val decoderOption: Decoder[Option[Total]] =
        Decoder.instance { c =>
          if (c.downField("status").as[String].contains("error"))
            c.as[Total].map(Some(_))
          else
            Right(None)
        }

      def recoverOrHandle[O](tryRecover: Total => Option[O])(j: Json): Fx[O] =
        decoderOption.decodeJson(j) match {
          case Right(Some(t)) =>
            tryRecover(t) match {
              case Some(o) => Fx pure o
              case None    => Fx fail t.toArticulateError
            }
          case Right(None) => Fx.fail(ArticulateError("Not an error.").hint(s"Response = ${j.noSpaces}"))
          case Left(e)     => Fx.fail(ArticulateError(e).hint(s"Response = ${j.noSpaces}"))
        }

      def handle[O]: Json => Fx[O] =
        recoverOrHandle[O](_ => None)(_)
    }

    /** Sample:
      * {{{
      *   {
      *     "code": 250,
      *     "error": "ACCT must be provided - Value must be one of: Never, Active (not Activ)",
      *     "email": {
      *       "email": "great@yay.com"
      *     }
      *   }
      * }}}
      */
    final case class Partial(code: Int, msg: String, email: Option[EmailAddr]) {
      def fullMsg: String = {
        val emailPrefix = email.fold("")(e => s"${e.value}: ")
        s"$emailPrefix[$code] $msg"
      }
    }

    object Partial {

      implicit def univEq: UnivEq[Partial] = UnivEq.derive

      def toArticulateError(h: Partial, t: List[Partial]): ArticulateError =
        ArticulateError(s"${t.size + 1} partial MailChimp API failure(s) occurred.")
          .hint(h.fullMsg, t.map(_.fullMsg): _*)

      implicit val decoder: Decoder[Partial] = {
        implicit val decoderEmailAddr: Decoder[EmailAddr] = Decoder.forProduct1("email")(EmailAddr.apply)
        Decoder.instance { c =>
          for {
            code  <- c.get[Int]("code")
            error <- c.get[String]("error")
            email <- c.get[Option[EmailAddr]]("email")
          } yield Partial(code, error, email)
        }
      }

      val decoderErrors: Decoder[List[Partial]] =
        Decoder.instance { c =>
          val errors = c.downField("errors")
          if (errors.succeeded)
            errors.as[List[Partial]]
          else
            Right(Nil)
        }

      val decoderOption: Decoder[Option[ArticulateError]] =
        decoderErrors.map {
          case Nil    => None
          case h :: t => Some(toArticulateError(h, t))
        }
    }
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

final class MailChimp(props: Props)(implicit httpClient: HttpClient) extends (MailingList.API ~> Fx) with HasLogger {
  private implicit val httpLogger: HttpLogger =
    HttpLogger(logger, _.replace(props.key.value, "<KEY>"))

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
