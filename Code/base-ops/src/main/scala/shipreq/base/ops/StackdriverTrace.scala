package shipreq.base.ops

import com.google.auth.oauth2.GoogleCredentials
import com.google.cloud.trace.{Trace, Tracer}
import com.google.cloud.trace.core.{ConstantTraceOptionsFactory, Labels, RateLimitingTraceOptionsFactory, ThrowableStackTraceHelper}
import com.google.cloud.trace.service.TraceGrpcApiService
import japgolly.microlibs.config.ConfigParser.Implicits.Defaults._
import japgolly.microlibs.config._
import java.io.FileInputStream
import scalaz.syntax.applicative._
import shipreq.base.util.Memo

object StackdriverTrace {

  final case class Cfg(

    /** GCP project the traces should be associated with. */
    projectId: String,

    /** Credentials file to be used for Stackdriver Trace API calls. */
    credentials: Option[String],

    /** The maximum number of seconds a Trace will be buffered locally before
      * being written to the Stackdriver Trace API. */
    scheduledDelaySec: Int,

    /** The maximum local buffer size (in bytes) to use before flushing to the
      * Stackdriver Trace API. */
    bufferSizeBytes: Int,

    /** Ensures that on average no more than n traces are issued during any given second, with sustained requests
      * being smoothly spread over each second. */
    limitTracesPerSec: Double) {

    private def traceService = {
      val b = TraceGrpcApiService.builder()

      b.setProjectId(projectId)

      for (filename <- credentials) {
        val fin = new FileInputStream(filename)
        val cred = GoogleCredentials.fromStream(fin)
        fin.close()
        b.setCredentials(cred)
      }

      b.setScheduledDelay(scheduledDelaySec)

      b.setBufferSize(bufferSizeBytes)

      val stackTraceEnabled = true
      b.setTraceOptionsFactory(
        limitTracesPerSec match {
        case a if a > 0 => new RateLimitingTraceOptionsFactory(a, stackTraceEnabled)
        case _          => new ConstantTraceOptionsFactory(true, stackTraceEnabled)
      })

      b.build()
    }

    val init: () => Unit =
      Memo.fn0(Trace.init(traceService))

    def getTracer(): Tracer = {
      init()
      Trace.getTracer()
    }

    def sqlTracer() =
      StackdriverTrace.sqlTracer(getTracer())
  }

  def config: Config[Option[Cfg]] =
    (     Config.get     [String]("projectId")
      |@| Config.get     [String]("credentials")
      |@| Config.getOrUse[Int]   ("scheduledDelaySec", 15).ensure(_ >= 0, "Must be ≥ 0")
      |@| Config.getOrUse[Int]   ("bufferSizeBytes", 32 * 1024).ensure(_ >= 0, "Must be ≥ 0")
      |@| Config.getOrUse[Double]("limitTracesPerSec", 100)
      ) ((projectId, credentials, scheduledDelaySec, bufferSizeBytes, limitTracesPerSec) =>
      projectId.map(id =>
        Cfg(
          projectId         = id,
          credentials       = credentials,
          scheduledDelaySec = scheduledDelaySec,
          bufferSizeBytes   = bufferSizeBytes,
          limitTracesPerSec = limitTracesPerSec)))

  object Label {
    // https://cloud.google.com/trace/docs/reference/v1/rpc/google.devtools.cloudtrace.v1
    final val Agent              = "/agent"
    final val Component          = "/component"
    final val ErrorMessage       = "/error/message"
    final val ErrorName          = "/error/name"
    final val HttpClientCity     = "/http/client_city"
    final val HttpClientCountry  = "/http/client_country"
    final val HttpClientProtocol = "/http/client_protocol"
    final val HttpClientRegion   = "/http/client_region"
    final val HttpHost           = "/http/host"
    final val HttpMethod         = "/http/method"
    final val HttpRedirectedUrl  = "/http/redirected_url"
    final val HttpRequestSize    = "/http/request/size"
    final val HttpResponseSize   = "/http/response/size"
    final val HttpStatusCode     = "/http/status_code"
    final val HttpUrl            = "/http/url"
    final val HttpUserAgent      = "/http/user_agent"
    final val Pid                = "/pid"
    final val Stacktrace         = "/stacktrace"
    final val Tid                = "/tid"
  }

  def sqlTracer(tracer: Tracer): SqlTracer =
    new SqlTracer {
      override def executePreparedStatement[@specialized(Boolean, Int, Long) A](method : String,
                                                                                sql    : String,
                                                                                batches: Int,
                                                                                run    : () => A): A = {
        val ctx = tracer.startSpan("JDBC")
        val labels = Labels.builder()
          .add("/jdbc/class", "PreparedStatement")
          .add("/jdbc/method", method)
          .add("/jdbc/sql", sql)
          .add("/jdbc/batches", batches.toString)

        try {
          val a = run()
          tracer.annotateSpan(ctx, labels.build())
          tracer.endSpan(ctx)
          a

        } catch {
          case t: Throwable =>
            labels.add(Label.ErrorMessage, t.getMessage)
            tracer.setStackTrace(ctx, ThrowableStackTraceHelper.createBuilder(t).build)
            tracer.annotateSpan(ctx, labels.build())
            tracer.endSpan(ctx)
            throw t
        }
      }
    }
}
