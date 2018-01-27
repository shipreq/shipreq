package shipreq.base.ops

import kamon.Kamon

object KamonBased {

  private[this] object Tag {
    object Jdbc {
      final val Class   = "jdbc.class"
      final val Method  = "jdbc.method"
      final val Sql     = "jdbc.sql"
      final val Batches = "jdbc.batches"
    }
  }

  val sqlTracer: SqlTracer =
    new SqlTracer {
      override def executePreparedStatement[@specialized(Boolean, Int, Long) A](method: String,
                                                                                sql: String,
                                                                                batches: Int,
                                                                                run: () => A): A = {
        val span = Kamon.buildSpan("JDBC").start()

        try {
          span.tag(Tag.Jdbc.Class, "PreparedStatement")
          span.tag(Tag.Jdbc.Method, method)
          span.tag(Tag.Jdbc.Sql, sql)
          span.tag(Tag.Jdbc.Batches, batches: Long)
          val a = run()
          span.finish()
          a

        } catch {
          case t: Throwable =>
            span.addError(t.getMessage, t)
            span.finish()
            throw t
        }
      }
    }

}
