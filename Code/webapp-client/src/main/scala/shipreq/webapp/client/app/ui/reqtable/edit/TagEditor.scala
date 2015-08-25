package shipreq.webapp.client.app.ui.reqtable
package edit

import japgolly.scalajs.react.extra.{ReusableVal, Px}
import scalaz.\/-
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{SetDiff, Must, UnivEq}
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.UiText
import shipreq.webapp.client.app.ui.{RemoteDataEditor, TextSeqEditor, VUCA}
import shipreq.webapp.client.lib.ui.TextEditor
import shipreq.webapp.client.lib.{Plain, HideDead}
import TextSeqEditor._
import UpdateContentCmd.PatchReqTags

object TagEditor {
  type Lookup = Map[String, ApplicableTag]

  type TagDiff = SetDiff[ApplicableTagId]

  val editor = new TextSeqEditor[ApplicableTagId, TagDiff](
    "TagEditor", Grammar.hashRefKey.seqFormat.apply, TextEditor.Input)

  def lookupForNoCol(p: Project): Must[Lookup] =
    lookupG(p, _.tags.notUsedInColumns)

  def lookupForCol(p: Project, f: CustomField.Tag.Id): Must[Lookup] =
    lookupG(p, _.tags inColumn f)

  def lookupG(p: Project, f: TagColumnDistribution.TagIds => Must[Set[ApplicableTag]]): Must[Lookup] =
    f(p.config.liveTagColumnDistribution).map(_
      .toStream
      .filter(_.live :: Live)
      .map(_.mapStrengthL(_.key.value))
      .toMap
    )

  def prepare(initial : Set[ApplicableTagId],
              project : Project,
              lookupM : Px[Must[Lookup]]): (VUCA[String, TagDiff] => editor.Props, String) = {

    val lookup = lookupM.map(mustResolve(_)(UnivEq.emptyMap))

    val (initialValues, initialTextValue) = {
      val lm  = lookup.value()
      val ls  = lm.values.toStream.map(_.id).toSet
      val ids = initial & ls

      val text =
        ids.toVector.map { a =>
          val m = project.config.atag(a).map(_.key.value)
          UiText.mustA(m)
        }.sorted mkString " "

      (ids, text)
    }

    val autoComplete: Px[AutoComplete] =
      lookup.map(l => ReusableVal.byRef(
        AutoComplete.tag(l.values.toStream, HideDead)(Plain)
      ))

    val parser: Parser[ApplicableTagId] = s => {
      val l = lookup.value()
      l.get(s) match {
        case Some(t) => \/-(t.id)
        case None    => leftNone
      }
    }

    val validate: Vector[ApplicableTagId] => ParseResult[TagDiff] =
      nvs => \/-(SetDiff.compare(initialValues, nvs.toSet))

    (editor.Props(_, parser, validate, autoComplete.value(), cellStyle, cellErrorMsgStyle), initialTextValue)
  }

  def selfManaged(initial : Set[ApplicableTagId],
                  project : Project,
                  lookupM : Px[Must[Lookup]],
                  commitFn: TagDiff => RemoteDataEditor.OnCommit): InitSelfManagedA[String] = {

    val (props, initialTextValue) = prepare(initial, project, lookupM)

    val onCommit = RemoteDataEditor.CommitFilter(commitFn).ignore(_.isEmpty)

    (initialTextValue, (s, u, a, commit) => props(VUCA(s, u, v => commit(onCommit(v)), a)).render)
  }

  def edit(subjectId: ReqId,
           initial  : Set[ApplicableTagId],
           project  : Project,
           lookupM  : Px[Must[Lookup]],
           commitFn : UpdateContentOnCommit): InitSelfManagedA[String] =
    selfManaged(initial, project, lookupM, commitFn.cmap[TagDiff](PatchReqTags(subjectId, _)))
}
