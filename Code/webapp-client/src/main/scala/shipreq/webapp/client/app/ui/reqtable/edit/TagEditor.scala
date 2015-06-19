package shipreq.webapp.client.app.ui.reqtable
package edit

import japgolly.scalajs.react.extra.{ReusableVal, Px}
import scalaz.\/-
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{SetDiff, Must, UnivEq}
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.ProjectChange
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.UiText
import shipreq.webapp.client.app.ui.TextSeqEditor, TextSeqEditor._
import shipreq.webapp.client.lib.{Plain, HideDead}
import ProjectChange.PatchReqTags

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

  def apply(initial  : Set[ApplicableTagId],
            subjectId: ReqId,
            project  : Project,
            lookupM  : Px[Must[Lookup]])
           (modCell  : Cell.ModCell,
            editIO   : EditIO[ProjectChange]): Cell.Cmd = {

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

    val (abort, commit) = editIO.setDiff[ApplicableTagId](PatchReqTags(subjectId, _)).abortCommit

    val validate: Vector[ApplicableTagId] => ParseResult[SetDiff[ApplicableTagId]] =
      nvs => \/-(SetDiff.compare(initialValues, nvs.toSet))

    Cell.selfManage(modCell, initialTextValue)((v, s, e) =>
      editor.Props(v, s, abort, parser, validate, commit(e), autoComplete.value()).apply)
  }
}
