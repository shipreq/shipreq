package shipreq.webapp.member.protocol.webworker

import japgolly.scalajs.react._
import scala.scalajs.js
import scala.scalajs.js.isUndefined
import scala.util.{Failure, Success, Try}
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.lib.LoggerJs

object ManagedWebWorker {

  trait Client[Req[_], R[_], Enc] {

    final def send[A](req: Req[A])(implicit readResult: R[A]): AsyncCallback[A] =
      sendEncoded(req, encode(req))

    def encode(req: Req[_]): Enc

    def sendEncoded[A](req: Req[A], enc: Enc)(implicit readResult: R[A]): AsyncCallback[A]

    def close: Callback
  }

  object Client {

    def apply[Req[_], Push](worker            : AbstractWebWorker.Client,
                            protocol          : WebWorkerProtocol,
                            onPush            : Push => Callback,
                            onError           : OnError,
                            logger            : LoggerJs)
                           (implicit reqWriter: protocol.Writer[Req[_]],
                            pushReader        : protocol.Reader[Push],
                           ): CallbackTo[Client[Req, protocol.Reader, protocol.Encoded]] = CallbackTo {

      import protocol.{Encoded, Reader}

      var closed        = false
      var lastPromiseId = 0
      var promises      = List.empty[Promise[Encoded]]
      val initBarrier   = AsyncCallback.barrier.runNow()

      def popPromise(id: Int): CallbackTo[Option[Promise[Encoded]]] =
        CallbackTo {
          var result = Option.empty[Promise[Encoded]]
          promises = promises.filter { p =>
            if (p.id ==* id) {
              result = Some(p)
              false // don't keep
            } else
              true // keep
          }
          result
        }

      def receive(data: js.Any): Callback =
        if (ServerIsReady == (data: Any))
          initBarrier.complete
        else if (!isUndefined(data.asInstanceOf[js.Dynamic].id)) {
          val msg = data.asInstanceOf[MessageWithId[Encoded]]
          popPromise(msg.id).flatMap {
            case Some(p) => p.complete(msg.body)
            case None    => onError.handle(ErrorMsg(s"Promise #${msg.id} not found"))
          }
        } else {
          val msg = data.asInstanceOf[PushMessage[Encoded]]
          CallbackTo(protocol.decode[Push](msg.body)).flatMap(onPush)
        }

      val ensureNotClosed: AsyncCallback[Unit] =
        AsyncCallback.delay {
          if (closed)
            throw new RuntimeException("Closed")
        }

      val preSend: AsyncCallback[Unit] =
        ensureNotClosed >> initBarrier.waitForCompletion >> ensureNotClosed

      worker.onError(onError).runNow()
      worker.listen(receive).runNow()

      new Client[Req, Reader, Encoded] {

        override def encode(req: Req[_]): Encoded =
          protocol.encode[Req[_]](req)

        override def sendEncoded[A](req: Req[A], enc: Encoded)(implicit readResult: Reader[A]): AsyncCallback[A] =
          preSend >> AsyncCallback.promise[A].map { case (result, complete) =>
            lastPromiseId += 1
            val id = lastPromiseId

            def listener(msg: Encoded): Callback = {
              val decoded = Try(protocol.decode[A](msg))
              logger(_.debug(s"Received WW response #$id: ${decoded.fold(_.toString, a => ("" + a).take(100).quoteInner)}"))
              complete(decoded)
            }

            val promise = Promise[Encoded](id, listener)
            promises ::= promise

            val msg = new MessageWithId(id, enc)
            logger(_.debug(s"Sending WW request #$id: ${("" + req).take(100).quoteInner}"))
            worker.send(msg, protocol.transferables(enc)).runNow()

            result
          }.asAsyncCallback.flatten.memo()

        override def close =
          Callback {
            if (!closed) {
              closed = true
              worker.send(ClientClosing, ()).runNow()
            }
          }
      }
    }

    private final case class Promise[A](id: Int, complete: A => Callback)

    implicit def reusability[Req[_], R[_], Enc]: Reusability[Client[Req, R, Enc]] =
      Reusability.byRef
  }

  // ===================================================================================================================

  trait Server[Client, Push] {
    def broadcast(msg: Push, exclude: Option[Client]): Callback
  }

  object Server {

    trait ServiceMaker[Req[_], Push] {
      def apply[Client](server: Server[Client, Push]): Service[Client, Req]
    }

    trait Service[Client, Req[_]] {
      def apply[A](client: Client, req: Req[A]): AsyncCallback[A]
    }

    trait ResponseEncoder[W[_], Req[_]] {
      def apply[A](req: Req[A]): W[A]
    }

    def start[Req[_], Push](worker              : AbstractWebWorker.Server,
                            protocol            : WebWorkerProtocol)
                           (serviceMaker        : ServiceMaker[Req, Push],
                            responseEncoder     : ResponseEncoder[protocol.Writer, Req],
                            onError             : OnError,
                            logger              : LoggerJs,
                           )(implicit pushWriter: protocol.Writer[Push],
                             readRequest        : protocol.Reader[Req[_]]): Callback =
      Callback {
        import worker.{Client => C}

        var clients = List.empty[C]

        def registerClient(client: C): Callback =
          Callback {
            clients ::= client
            logger(_.info("New client: ", client.asInstanceOf[js.Any]))
          } >> worker.send(client :: Nil, ServerIsReady, ())

        def deregisterClient(client: C): Callback =
          Callback {
            clients = clients.filter(_ != client)
            logger(_.info("De-registering client: ", client.asInstanceOf[js.Any]))
          }

        val server: Server[C, Push] = new Server[C, Push] {
          override def broadcast(push: Push, exclude: Option[C]): Callback =
            Callback.byName {
              var cs = clients.iterator
              for (c <- exclude) {
                cs = cs.filter(_ != c)
              }

              val enc = protocol.encode(push)
              val msg = new PushMessage(enc)

              worker.send(cs, msg, protocol.transferables(enc))
            }
        }

        val service = serviceMaker(server)

        def respond[A](client: C, id: Int, req: Req[A]): AsyncCallback[Unit] =
          service(client, req).attemptTry.flatMap {
            case Success(a) =>
              logger(_.debug(s"Responding to request #$id with result: ${(""+a).take(100).quoteInner}"))
              val enc = protocol.encode(a)(responseEncoder(req))
              val msg = new MessageWithId(id, enc)
              worker.send(client :: Nil, msg, protocol.transferables(enc)).asAsyncCallback
            case Failure(err) =>
              logger(_.error(s"Failed to service request #$id."))
              LoggerJs.exception(err)
              AsyncCallback.unit
          }

        def onMessage(client: C): js.Any => Callback =
          data =>
            (data: Any) match {
              case ClientClosing => deregisterClient(client)
              case _ =>
                Callback.byName {
                  val msg = data.asInstanceOf[MessageWithId[protocol.Encoded]]
                  val req = protocol.decode[Req[_]](msg.body)
                  respond(client, msg.id, req).toCallback
                }
            }

        worker.onError(onError).runNow()

        worker.listen { client =>
          for {
            _ <- registerClient(client)
          } yield onMessage(client)
        }.runNow()

      }
  }

  // ===================================================================================================================

  private final class MessageWithId[+A](val id: Int, val body: A) extends js.Object

  private final class PushMessage[+A](val body: A) extends js.Object

  private final val ServerIsReady = "."
  private final val ClientClosing = ";"

}
