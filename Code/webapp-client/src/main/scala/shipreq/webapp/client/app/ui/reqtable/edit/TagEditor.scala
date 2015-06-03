package shipreq.webapp.client.app.ui.reqtable
package edit

import japgolly.scalajs.react.extra.{ReusableVal, Px}
import scalaz.effect.IO
import scalaz.\/-
import shipreq.base.util.ScalaExt._
import shipreq.base.util.effect.IoUtils, IoUtils.IoExt
import shipreq.base.util.{Must, UnivEq}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.{TagColumnDistribution, UiText}
import shipreq.webapp.client.app.ui.TextSeqEditor, TextSeqEditor._
import shipreq.webapp.client.util.Plain

// TODO Hide dead tags & maintain across edits (unless show deleted is on)

object TagEditor {
  type A      = ApplicableTagId
  type Lookup = Map[String, ApplicableTag]

  val editor = textSetEditor[ApplicableTagId]("TagEditor", Grammar.hashRefKey.seqFormat.apply)

  def lookupForNoCol(p: Project): Must[Lookup] =
    lookupG(p, _.tags.notUsedInColumns)

  def lookupForCol(p: Project, f: CustomField.Tag.Id): Must[Lookup] =
    lookupG(p, _.tags inColumn f)

  def lookupG(p: Project, f: TagColumnDistribution.TagIds => Must[Set[ApplicableTag]]): Must[Lookup] =
    f(p.aliveTagColumnDistribution).map(
      _.toStream
      .map(_.mapStrengthL(_.key.value))
      .toMap
    )

  def apply(initial : Set[A],
            project : Project,
            lookupM : Px[Must[Lookup]],
            setState: Option[Cell.State] => IO[Unit]): Cell.State = {

    def init: String =
      initial.toVector.map { a =>
        val m = project.atag(a).map(_.key.value)
        UiText.mustA(m)
      }.sorted mkString " "

    val lookup = lookupM.map(mustResolve(_)(UnivEq.emptyMap))

    val autoComplete: Px[AutoComplete] =
      lookup.map(l => ReusableVal.byRef(
        AutoComplete.tag(l.values.toStream)(Plain)
      ))

    val parser: Parser[A] = () => {
      val l = lookup.value()
      s => l.get(s) match {
        case Some(t) => \/-(t.id)
        case None    => leftNone
      }
    }

    val abort: IO[Unit] =
      setState(None)

    val commit: Set[A] => IO[Unit] =
      // TODO If change occurred, send to server & lock cell. (If unchanged, clear state.)
      s => setState(None) >>> IO{ println("Sent to ze server: " + s) }

    Cell.selfManage(setState, init)(
      editor.Props(_, _, abort, parser, toSetWithoutValidation, commit, autoComplete.value()).apply)
  }
}
