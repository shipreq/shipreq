package shipreq.webapp.client.app.ui.reqtable
package edit

import japgolly.scalajs.react.extra.{ReusableVal, Px}
import scalaz.effect.IO
import scalaz.\/-
import shipreq.base.util.ScalaExt._
import shipreq.base.util.effect.IoUtils, IoUtils.IoExt
import shipreq.base.util.{SetDiff, Must, UnivEq}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.UiText
import shipreq.webapp.client.app.ui.TextSeqEditor, TextSeqEditor._
import shipreq.webapp.client.lib.HideDead
import shipreq.webapp.client.util.Plain

// TODO Hide dead tags & maintain across edits (unless show deleted is on)

object TagEditor {
  type Lookup = Map[String, ApplicableTag]

  val editor = textSetEditor[ApplicableTagId, SetDiff[ApplicableTagId]]("TagEditor", Grammar.hashRefKey.seqFormat.apply)

  def lookupForNoCol(p: Project): Must[Lookup] =
    lookupG(p, _.tags.notUsedInColumns)

  def lookupForCol(p: Project, f: CustomField.Tag.Id): Must[Lookup] =
    lookupG(p, _.tags inColumn f)

  def lookupG(p: Project, f: TagColumnDistribution.TagIds => Must[Set[ApplicableTag]]): Must[Lookup] =
    f(p.liveTagColumnDistribution).map(_
      .toStream
      .filter(_.live :: Live)
      .map(_.mapStrengthL(_.key.value))
      .toMap
    )

  def apply(initial : Set[ApplicableTagId],
            project : Project,
            lookupM : Px[Must[Lookup]],
            setState: Option[Cell.State] => IO[Unit]): Cell.State = {

    val lookup = lookupM.map(mustResolve(_)(UnivEq.emptyMap))

    val (initialValues, initialTextValue) = {
      val lm  = lookup.value()
      val ls  = lm.values.toStream.map(_.id).toSet
      val ids = initial & ls

      val text =
        ids.toVector.map { a =>
          val m = project.atag(a).map(_.key.value)
          UiText.mustA(m)
        }.sorted mkString " "

      (ids, text)
    }

    val autoComplete: Px[AutoComplete] =
      lookup.map(l => ReusableVal.byRef(
        AutoComplete.tag(l.values.toStream, HideDead)(Plain)
      ))

    val parser: Parser[ApplicableTagId] = () => {
      val l = lookup.value()
      s => l.get(s) match {
        case Some(t) => \/-(t.id)
        case None    => leftNone
      }
    }

    val abort: IO[Unit] =
      setState(None)

    val validate: Vector[ApplicableTagId] => ParseResult[SetDiff[ApplicableTagId]] =
      nvs => \/-(SetDiff.compare(initialValues, nvs.toSet))

    val commit: SetDiff[ApplicableTagId] => IO[Unit] =
      // TODO If change occurred, send to server & lock cell. (If unchanged, clear state.)
      s => setState(None) >>> IO{ if (s.nonEmpty) println("Sent to ze server: " + s) }

    Cell.selfManage(setState, initialTextValue)(
      editor.Props(_, _, abort, parser, validate, commit, autoComplete.value()).apply)
  }
}
