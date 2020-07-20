package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scala.collection.immutable.TreeSet
import scalacss.ScalaCssReact._
import shipreq.webapp.base.jsfacade.DomPurify
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.text.Atom.CodeBlockDetail
import shipreq.webapp.base.ui.CodeBlockWithSyntaxHighlighting
import shipreq.webapp.client.project.app.Style.{widgets => *}
import shipreq.webapp.client.project.app.WebWorkerClient

object RichCodeBlock {

  final case class Props(detail   : Option[CodeBlockDetail],
                         code     : String,
                         webWorker: WebWorkerClient.Instance) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private def render(p: Props): VdomNode =
    p.detail match {
      case None                              => renderCodeBlock(p.code, None)
      case Some(CodeBlockDetail(lang, attr)) => renderSpecialised(p.code, lang, attr, p.webWorker)
    }

  private def renderCodeBlock(code: String, language: Option[String]): VdomNode =
    CodeBlockWithSyntaxHighlighting.Props(
      code        = code,
      language    = language,
      lineNumbers = language.isDefined,
    ).render

  private def renderUnrecognisedAttr(language: String, attributes: TreeSet[String]): VdomNode =
    <.div(
      *.richCodeBlockError,
      ^.key := 1,
      "Unsupported options for ",
      <.code(*.richCodeBlockErrorCode, language),
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

    var result: VdomNode = null

    language match {
      case "html" => if (readAttr("render")) result = renderHtml(code)
      case "svg"  => if (readAttr("render")) result = renderHtml(code)
      case "dot"  => if (readAttr("render")) result = renderDot(code, webWorker)
      case _      => ()
    }

    if (unrecognised.nonEmpty)
      renderUnrecognisedAttr(language, unrecognised)
    else if (result ne null)
      result
    else
      renderCodeBlock(code = code, language = Some(language))
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