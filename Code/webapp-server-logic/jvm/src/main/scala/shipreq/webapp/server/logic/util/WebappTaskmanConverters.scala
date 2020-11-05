package shipreq.webapp.server.logic.util

import shipreq.taskman.{api => T}
import shipreq.webapp.base.{data => W}

object WebappTaskmanConverters {

  implicit class WTUserId(private val self: W.UserId) extends AnyVal {
    def toTaskman: T.UserId = T.UserId(self.value)
  }
  implicit class TWUserId(private val self: T.UserId) extends AnyVal {
    def toWebapp: W.UserId = W.UserId(self.value)
  }

  implicit class WTEmailAddr(private val self: W.EmailAddr) extends AnyVal {
    def toTaskman: T.EmailAddr = T.EmailAddr(self.value)
  }
  implicit class TWEmailAddr(private val self: T.EmailAddr) extends AnyVal {
    def toWebapp: W.EmailAddr = W.EmailAddr(self.value)
  }
}
