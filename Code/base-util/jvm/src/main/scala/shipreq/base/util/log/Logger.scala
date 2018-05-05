package shipreq.base.util.log

import com.typesafe.scalalogging.{Logger => ScalaLogger}
import java.util.concurrent.ConcurrentHashMap

object Logger {
  private[this] val cache =
    new ConcurrentHashMap[Class[_], ScalaLogger]()

  private[this] val create: java.util.function.Function[Class[_], ScalaLogger] =
    c => {
      // object A { object B } = A$B$
      var n = c.getTypeName
      if (n.endsWith("$"))
        n = n.dropRight(1)
      n = n.replace('$', '.')
      ScalaLogger(n)
    }

  private[log] def forClass(c: Class[_]): ScalaLogger =
    cache.computeIfAbsent(c, create)
}

trait HasLogger {
  final protected val log = Logger.forClass(getClass)
}
