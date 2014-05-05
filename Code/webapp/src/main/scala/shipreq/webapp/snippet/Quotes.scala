package shipreq.webapp.snippet

import shipreq.webapp.app.AppConfig
import shipreq.webapp.util.{CacheFn, NonEmptyTemplate}
import net.liftweb.common.Logger
import net.liftweb.util.Helpers._
import scala.util.Random
import scala.xml.{NodeSeq, Elem, Node}

object Quotes extends Logger {

  val quotes = {
    val template = NonEmptyTemplate.load("templates-hidden/quotes").get
    val process = ".off" #> "" & "blockquote [class+]" #> "rndquote"
    def collect(t: NodeSeq) = t.head.child.toList.collect {case e: Elem => e}
    val countAll = collect(template).size
    val used = collect(process(template))
    info(s"Quotes available: ${used.size}/$countAll")
    used
  }

  private val rng = new Random
  private var nextQuotes: List[Node] = Nil

  def nextQuote(): Node = rng.synchronized {
    if (nextQuotes.isEmpty)
      nextQuotes = rng.shuffle(quotes)
    val h :: t = nextQuotes
    nextQuotes = t
    h
  }

  def renderFn = {
    val q = nextQuote
    "*" #> q
  }

  val rcache = CacheFn(renderFn)(AppConfig.QuoteCachePolicy)

  // def render = rcache.value
  val render = "*" #> "" // Quotes disabled
}
