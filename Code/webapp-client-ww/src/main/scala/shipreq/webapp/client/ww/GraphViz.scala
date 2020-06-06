package shipreq.webapp.client.ww

import japgolly.scalajs.react.AsyncCallback
import japgolly.univeq._
import org.scalajs.dom.webworkers.DedicatedWorkerGlobalScope
import scala.scalajs.js
import scala.scalajs.js.JSON
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.{Backwards, Direction, ErrorMsg, Forwards}
import shipreq.webapp.base.data.savedview.ImpGraphConfig.GraphDir
import shipreq.webapp.base.{AssetManifest, UiText}
import shipreq.webapp.client.ww.api.Svg

object GraphViz {
  private type RawFn = js.Function1[String, js.Thenable[String]]

  def init(): Unit = {
    instance
    ()
  }

  private lazy val instance: RawFn = {
    DedicatedWorkerGlobalScope.self.asInstanceOf[js.Dynamic].vizWasmFile = AssetManifest.vizWasm
    DedicatedWorkerGlobalScope.self.importScripts(js.Array(AssetManifest.vizJs))
    js.Dynamic.global.viz.asInstanceOf[RawFn]
  }

  private val titlesAndComments = "(?:<title>[^<>]*?</title>|<!--[^\u0000]*?-->)".r

  def apply(dot: DOT): AsyncCallback[ErrorMsg \/ Svg] = {
    println(dot.content)
    val main: AsyncCallback[Svg] =
      for {
        svg <- AsyncCallback.fromJsPromise(instance(dot.content))
      } yield Svg(titlesAndComments.replaceAllIn(svg, ""))
    main.attempt.map {
      case Right(svg) => \/-(svg)
      case Left(t)    => -\/(ErrorMsg.fromThrowable(t))
    }
  }

  final case class DOT(content: String) extends AnyVal {
    @inline def toSvg: AsyncCallback[ErrorMsg \/ Svg] =
      GraphViz(this)
  }

  // ===================================================================================================================

  def digraph(f: Builder => Unit): DOT = {
    val b = new Builder
    b.group("digraph G"){
      b append "bgcolor=transparent;"
      f(b)
    }
    b.result()
  }

  final class Builder {
    private val sb = new StringBuilder

    def result(): DOT =
      DOT(sb.result())

    def append(c: Char): Unit = sb append c
    def append(i: Int): Unit = sb append i
    def append(s: String): Unit = sb append s

    def rankdir(graphDir: GraphDir): Unit = {
      sb append "rankdir="
      val dir: String =
        graphDir match {
          case GraphDir.BottomToTop => "BT"
          case GraphDir.LeftToRight => "LR"
          case GraphDir.RightToLeft => "RL"
          case GraphDir.TopToBottom => "TB"
        }
      sb append dir
      sb append ';'
    }

    def group(group: String)(inner: => Unit): Unit = {
      sb append group
      sb append '{'
      inner
      sb append '}'
    }

    def attrGroup(attr: String)(inner: => Unit): Unit = {
      sb append '{'
      sb append attr
      inner
      sb append '}'
    }

    def attrBlock(inner: => Unit): Unit = {
      sb append '['
      inner
      sb append ']'
    }

    @inline def escapeAttrValue(s: String): String =
      JSON.stringify(s)

    def setLabel(label: String): Unit = {
      sb append "label="
      sb append escapeAttrValue(label)
    }

    /** [label="x"] */
    def labelAttr(label: String): Unit =
      attrBlock(setLabel(label))

    /** hover text. TITLE in HTML */
    def setTooltip(tooltip: String): Unit = {
      sb append "tooltip="
      sb append escapeAttrValue(UiText.hoverText(tooltip))
    }

    /*
    Having flow like `1 -> 2,3,4` works fine in the latest versions of GraphViz, but (sometimes) causes problems with the
    version used in Viz.js. Instead they need to be broken into `1->2; 1->3; 1->4`.

    This is a graph that causes viz.js problems:
      digraph G{rankdir=TB;node[style=filled color="#333333"]edge[color="#333333"]node[fillcolor="#91D5BC"]1[label="BL-1"]node[fillcolor="#94DD59"]7[label="CO-1"]8[label="CO-2"]node[fillcolor="#D0A9D4"]5[label="MF-3"]6[label="MF-4"]3[label="MF-1"]4[label="MF-2"]node[fillcolor="#DFB863"]9[label="UC-1"]10[label="UC-2"]11[label="UC-3"]5->6;10->11;9->11,10;3->5,1;11->1;4->5;}
    */
    //  def intercalate[A](as: IterableOnce[A], between: => Unit)(f: A => Unit): Unit = {
    //    var first = true
    //    for (a <- as) {
    //      if (first)
    //        first = false
    //      else
    //        between
    //      f(a)
    //    }
    //  }

    def flowOneToMany[A](fromId: A, toIds: IterableOnce[A])(id: A => Unit, atEnd: => Unit): Unit =
      for (toId <- toIds.iterator) {
        id(fromId)
        arrow()
        id(toId)
        atEnd
        eol()
      }

    def flowS(from: String, dir: Direction, to: String): Unit =
      flowSB(sb append from, dir, sb append to)

    def flowSB(from: => Unit, dir: Direction, to: => Unit): Unit =
      dir match {
        case Forwards  => from; arrow(); to
        case Backwards => to  ; arrow(); from
      }

    def arrow(): Unit =
      sb append "->"

    def eol(): Unit =
      if (sb.last !=* ';') // Guess what? ;; causes Viz.JS to crash! (GraphViz itself is ok with it.)
      sb append ';'

    def eolAfterChange(body: => Unit): Unit = {
      val before = sb.length
      body
      if (before !=* sb.length)
        eol()
    }
  }
}
