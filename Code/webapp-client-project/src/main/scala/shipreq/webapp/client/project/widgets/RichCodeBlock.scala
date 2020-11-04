package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scala.collection.immutable.TreeSet
import scalacss.ScalaCssReact._
import shipreq.webapp.base.jsfacade.DomPurify
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.member.text.Atom.CodeBlockDetail
import shipreq.webapp.base.ui.CodeBlockWithSyntaxHighlighting
import shipreq.webapp.client.project.app.Style.{widgets => *}
import shipreq.webapp.client.project.app.WebWorkerClient

object RichCodeBlock {

  object Attribute {
    final val render      = "render"
    final val lineNumbers = "line-numbers"
  }

  final case class Props(detail   : Option[CodeBlockDetail],
                         code     : String,
                         webWorker: WebWorkerClient.Instance) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private val errCode = <.code(*.richCodeBlockErrorCode)

  private def render(p: Props): VdomNode =
    p.detail match {
      case None                              => renderCodeBlock(p.code, None, lineNumbers = false)
      case Some(CodeBlockDetail(lang, attr)) => renderSpecialised(p.code, lang, attr, p.webWorker)
    }

  private def renderCodeBlock(code: String, language: Option[String], lineNumbers: Boolean): VdomNode =
    CodeBlockWithSyntaxHighlighting.Props(
      code        = code,
      language    = language,
      lineNumbers = lineNumbers,
    ).render

  private def renderUnrecognisedAttr(language: String, suffix: TagMod, attributes: TreeSet[String]): VdomNode =
    <.div(
      *.richCodeBlockError,
      ^.key := 1,
      "Unsupported options for ",
      errCode(language),
      suffix,
      ":",
      <.ul(*.richCodeBlockErrorUL, attributes.toTagMod(a =>
        <.li(<.code(*.richCodeBlockErrorCode, a)))))

  private def renderSpecialised(code      : String,
                                language  : String,
                                attributes: TreeSet[String],
                                webWorker : WebWorkerClient.Instance): VdomNode = {

    var unrecognised = attributes

    def readAttr(a: String): Boolean = {
      unrecognised -= a
      attributes.contains(a)
    }

    var suffix: TagMod = null
    var result: VdomNode = null
    var lineNumbers = false

    def acceptRender(rendered: => VdomNode): Unit =
      if (readAttr(Attribute.render)) {
        suffix = TagMod(" with ", errCode(Attribute.render))
        result = rendered
      }

    language match {
      case "html" => acceptRender(renderHtml(code))
      case "svg"  => acceptRender(renderHtml(code))
      case "dot"  => acceptRender(renderDot(code, webWorker))
      case _      => ()
    }

    if (result eq null) {
      if (readAttr("line-numbers")) lineNumbers = true
    }

    if (unrecognised.nonEmpty)
      renderUnrecognisedAttr(language, Option(suffix).whenDefined, unrecognised)
    else if (result ne null)
      result
    else
      renderCodeBlock(code = code, language = Some(language), lineNumbers = lineNumbers)
  }

  private def renderHtml(code: String): VdomNode =
    <.div(^.key := "h", ^.dangerouslySetInnerHtml := DomPurify.sanitize(code))

  private def renderDot(code: String, webWorker: WebWorkerClient.Instance): VdomNode =
    UserDefinedGraph.Props(code, webWorker).render

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}