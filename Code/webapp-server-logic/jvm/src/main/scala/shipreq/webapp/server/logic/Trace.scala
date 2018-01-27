package shipreq.webapp.server.logic

import doobie.imports.ConnectionIO
import java.nio.ByteBuffer
import java.time.Duration
import scalaz.syntax.monad._
import scalaz.{Monad, ~>}
import shipreq.base.util.Url

object Trace {

  trait Basic[F[_]] {

    /** Trace a generic task.
      *
      * Creates either a top-level trace, or a sub-trace if nested.
      */
    def generic[A](name: String)(f: => F[A]): F[A]

    /** Sub-trace.
      *
      * Creates a sub-task in a parent task. If not nested under a top-level trace, then no trace is created.
      */
    def sub[A](name: String)(f: => F[A]): F[A]
  }

  trait Algebra[F[_], Req, Res] extends Basic[F] {

    /** Trace a HTTP request.
      *
      * Creates a top-level trace.
      */
    def http(req: Req, path: Url.Relative)(f: => F[Res]): F[Res]

    /** Trace the invocation of a server-side procedure (by the user's browser).
      *
      * Creates a top-level trace.
      */
    def serverSideProc(name: String, input: ByteBuffer)(f: => Server.SspResponse[F]): Server.SspResponse[F]

    // Provided:

    def db(real: ConnectionIO ~> F): ConnectionIO ~> F =
      new (ConnectionIO ~> F) {
        override def apply[A](fa: ConnectionIO[A]): F[A] =
          generic("SQL")(real(fa))
      }

//    def dbSecurity(db: DB.ForSecurity[F]): DB.ForSecurity[F] =
//      new DB.ForSecurity[F] {
//        def t[A](name: String)(f: F[A]) = sub("SQL:" + name)(f)
//        override def getUserAndPasswordByEmail(email: EmailAddr)      = t("getUserAndPasswordByEmail")(db.getUserAndPasswordByEmail(email))
//        override def getUserAndPasswordByUsername(username: Username) = t("getUserAndPasswordByUsername")(db.getUserAndPasswordByUsername(username))
//        override def logLoginSuccess(id: UserId, ip: Option[IP])      = t("logLoginSuccess")(db.logLoginSuccess(id, ip))
//        override def getProjectOwner(id: ProjectId)                   = t("getProjectOwner")(db.getProjectOwner(id))
//      }

    def server(orig: Server.Algebra[F]): Server.Algebra[F] = {
      val self = this
      new Server.Algebra[F] {
        override def delay[A](f: F[A], d: Duration) = orig.delay(f, d)
        override def fork[A](f: F[A])               = orig.fork(f)
        override val clientIP                       = orig.clientIP
        override val sessionId                      = orig.sessionId
        override def now                            = orig.now
        // Customisation:
        override val registerServerSideProc = (name, f) =>
          self.sub("RegisterSSP:" + name)(
            orig.registerServerSideProc(name, i => self.serverSideProc("SSP." + name, i)(f(i))))
      }
    }

    def compose(inner: Algebra[F, Req, Res]): Algebra[F, Req, Res] = {
      val outer = this
      new Algebra[F, Req, Res] {
        override def generic[A](name: String)(f: => F[A]) =
          outer.generic(name)(inner.generic(name)(f))
        override def sub[A](name: String)(f: => F[A]) =
          outer.sub(name)(inner.sub(name)(f))
        override def http(req: Req, path: Url.Relative)(f: => F[Res]) =
          outer.http(req, path)(inner.http(req, path)(f))
        override def serverSideProc(name: String, input: ByteBuffer)(f: => Server.SspResponse[F]) =
          outer.serverSideProc(name, input)(inner.serverSideProc(name, input)(f))
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def off[F[_], Req, Res]: Algebra[F, Req, Res] =
    new Algebra[F, Req, Res] {
      override def generic[A](name: String)(f: => F[A]) = f
      override def sub[A](name: String)(f: => F[A]) = f
      override def http(req: Req, path: Url.Relative)(f: => F[Res]) = f
      override def serverSideProc(name: String, input: ByteBuffer)(f: => Server.SspResponse[F]) = f
      override def server(orig: Server.Algebra[F]) = orig
      override def compose(a: Algebra[F, Req, Res]) = a
    }

  def logToStdout[F[_], Req, Res](implicit F: Monad[F]): Algebra[F, Req, Res] =
    new Algebra[F, Req, Res] {
      override def http(req: Req, path: Url.Relative)(f: => F[Res]) = sub(path.relativeUrl)(f)
      override def serverSideProc(name: String, input: ByteBuffer)(f: => Server.SspResponse[F]) = sub(name)(f)
      override def sub[A](name: String)(f: => F[A]) = generic(name)(f)
      override def generic[A](name: String)(f: => F[A]) =
        for {
          start <- F point System.nanoTime()
          _     <- F point println(s"Starting $name …")
          a     <- f
          end   <- F point System.nanoTime()
          _     <- F point printf("Finished %s in %,3d ns\n", name, end - start)
        } yield a
    }

}