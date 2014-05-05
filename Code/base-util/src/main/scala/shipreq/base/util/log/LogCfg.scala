package shipreq.base.util.log

import java.net.URL
import shipreq.base.util.{Util, RunMode}
import org.slf4j.LoggerFactory

object LogCfg {

  object Logback {
    import ch.qos.logback.classic.LoggerContext
    import ch.qos.logback.classic.joran.JoranConfigurator

    def init(rm: RunMode): Unit = {
      val fs = RunMode.filenames(rm)(_.mkString("/", ".", ".logback.xml"))
      val uo = Util.existentLocalResources(fs).headOption
      uo.foreach(init)
    }

    def init(url: URL): Unit = {
      val lc = LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]
      val configurator = new JoranConfigurator()
      configurator.setContext(lc)
      lc.reset()
      configurator.doConfigure(url)
    }
  }
}