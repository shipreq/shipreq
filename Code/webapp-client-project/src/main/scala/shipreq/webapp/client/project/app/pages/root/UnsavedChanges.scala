package shipreq.webapp.client.project.app.pages.root

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react.{CallbackTo, Reusability}
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.member.text.{PlainText, TextSearch}
import shipreq.webapp.base.util._
import shipreq.webapp.client.project.app.pages
import shipreq.webapp.client.project.feature.EditorFeature
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ProjectWidgets

final case class UnsavedChanges(count    : Int,
                                locations: Set[UnsavedChanges.Location]) {
  import UnsavedChanges.Location

  assert(count >= 0)
  assert(locations.size <= count)

  def isEmpty = count == 0
  def nonEmpty = count > 0

  val descShort = UiText.unsavedChanges(count)

  def desc(p: Project): String = {
    val head = descShort
    if (count == 0)
      head
    else
      MutableArray(locations).map {
        case Location.Req(id)          => PlainText.pubidByReqId(id, p)
        case Location.ReqCodeGroup(id) => PlainText.reqCodeById(id, p)
        case Location.ProjectName      => "project name"
        case Location.ManualIssues     => "manual issue(s)"
        case Location.FieldConfig      => "field editor"
        case Location.IssueConfig      => "issue type editor"
        case Location.ReqTypeConfig    => "req type editor"
        case Location.TagConfig        => "tag editor"
      }
        .sort
        .map("  * " + _)
        .mkString(s"$head\n\nLocations:\n", "\n", "")
  }
}

object UnsavedChanges {

  val empty: UnsavedChanges =
    apply(0, Set.empty)

  implicit def univEq     : UnivEq     [UnsavedChanges] = UnivEq.derive
  implicit def reusability: Reusability[UnsavedChanges] = Reusability.byRefOrUnivEq

  final case class Input(state         : State,
                         editability   : EditorFeature.Editability.ForProject,
                         project       : Project,
                         textSearch    : TextSearch,
                         projectWidgets: ProjectWidgets.NoCtx,
                         useCases      : UseCases)

  object Input {
    implicit def reusability: Reusability[Input] = Reusability.byRef || Reusability.derive
  }

  def determine(input: Input): CallbackTo[UnsavedChanges] =
    CallbackTo {
      var count = 0
      var locs = Set.empty[Location]

      for (t <- Type.values) {
        val results = t.determine(input).runNow()
        if (results.nonEmpty) {
          count += results.length
          locs ++= results
        }
      }

      UnsavedChanges(count, locs)
    }

  // ===================================================================================================================

  sealed trait Location

  object Location {
    case object ProjectName                           extends Location
    case object ManualIssues                          extends Location
    case object FieldConfig                           extends Location
    case object IssueConfig                           extends Location
    case object ReqTypeConfig                         extends Location
    case object TagConfig                             extends Location
    final case class Req(id: ReqId)                   extends Location
    final case class ReqCodeGroup(id: ReqCodeGroupId) extends Location

    implicit def univEq     : UnivEq     [Location] = UnivEq.derive
    implicit def reusability: Reusability[Location] = Reusability.byRefOrUnivEq
  }

  // ===================================================================================================================

  sealed trait Type {
    def determine(input: Input): CallbackTo[Vector[Location]]
  }

  object Type {

    private[this] val emptyVector = Vector.empty

    case object ProjectName extends Type {
      override def determine(input: Input) =
        CallbackTo {
          input.state.projectName match {
            case Some(s) if s.corrected !=* input.project.name => emptyVector :+ Location.ProjectName
            case _                                             => emptyVector
          }
        }
    }

    case object CreationFields extends Type {
      import shipreq.webapp.client.project.feature.CreateFeature
      import CreateFeature.{EditorArgs, FieldKey, RowKey}

      override def determine(input: Input) =
        CallbackTo {
          var ls = Vector.empty[Location]

          // New manual issue
          if (input.state.issuesPage.newIssue.open is Open)
            for (e <- input.state.create(RowKey.ManualIssue)(FieldKey.ManualIssue)) {

              val uselessArgs =
                EditorArgs.ForTextEditor.empty(
                  project        = input.project,
                  textSearch     = input.textSearch,
                  projectWidgets = input.projectWidgets)

              // .isRight here means we have valid NonEmptyText
              // .isLeft is only true if the field is blank
              if (e.value(uselessArgs).isRight)
                ls :+= Location.ManualIssues
            }

          ls
        }
    }

    case object Editors extends Type {
      import shipreq.webapp.client.project.feature.EditorFeature
      import EditorFeature.{Editability, EditorArgs, Editor, FieldKey, RowKey}

      override def determine(input: Input) =
        CallbackTo {

          // TODO Fix in Scala 3
          def _uselessArgs(f: FieldKey): f.Args =
            EditorArgs.empty(f: f.type)(
              project        = input.project,
              textSearch     = input.textSearch,
              projectWidgets = input.projectWidgets)

          val uselessArgsMemo: FieldKey => Any =
            Memo[FieldKey, Any](_uselessArgs(_))

          def uselessArgs(f: FieldKey): f.Args =
            uselessArgsMemo(f).asInstanceOf[f.Args]

          def eligible(f: FieldKey)
                      (editor: Editor[f.Args, Any],
                       editability: => Permission): Boolean = {
            // Rarely, but sometimes, ids and their data are hard-deleted.
            // When that happens, the editor state isn't deleted (probably it should be) but the point is, calling
            // .change here can result in stuff like:
            //     java.util.NoSuchElementException: key not found: UseCaseStepId(201)
            @inline def changed =
              try
                editor.change(uselessArgs(f)).isChanged
              catch {
                case _: Throwable => false
              }

            // This is wrapped in try for the same reasons as above
            @inline def editable =
              try
                editability.is(Allow)
              catch {
                case _: Throwable => false
              }

            editable && changed
          }

          def countFields(state      : EditorFeature.State.ForFields,
                          editability: Editability.ForFields[FieldKey],
                          loc        : Location) = {

            var locs = List.empty[Location]

            for ((f, editor) <- state) {
              val e = editor.asInstanceOf[Editor[f.Args, f.Change]]
              if (eligible(f)(e, editability(f)))
                locs ::= loc
            }

            locs
          }

          input.state.edit.iterator.flatMap {
            case (row, fields) =>

              val editability = input.editability(row)

              row match {
                case RowKey.GenericReq(id) => countFields(fields, editability, Location.Req(id))
                case RowKey.UseCase(id)    => countFields(fields, editability, Location.Req(id))
                case RowKey.ManualIssues   => countFields(fields, editability, Location.ManualIssues)
                case RowKey.CodeGroup(id)  => countFields(fields, editability, Location.ReqCodeGroup(id))

                case RowKey.UseCaseSteps =>
                  fields.iterator.flatMap {
                    case (f, editor) =>
                      @inline def e = editor.asInstanceOf[Editor[f.Args, f.Change]]
                      f match {
                        case FieldKey.UseCaseStep(stepId) if eligible(f)(e, editability(f)) =>
                          input.useCases.stepIndex
                            .get(stepId)
                            .map(k => Location.Req(k.useCaseId))
                            .toList
                        case _ => Nil
                      }
                  }
              }
          }.toVector
        }
    }

    case object FieldConfig extends Type {
      import pages.config.fields.FieldConfig.EditorState
      override def determine(i: Input) =
        CallbackTo {
          i.state.fieldConfig.right.editorOption match {
            case Some(EditorState.ImpEditor (s)) if s.updateCmd(i.project.config).isChanged => emptyVector :+ Location.FieldConfig
            case Some(EditorState.TagEditor (s)) if s.updateCmd(i.project.config).isChanged => emptyVector :+ Location.FieldConfig
            case Some(EditorState.TextEditor(s)) if s.updateCmd(i.project.config).isChanged => emptyVector :+ Location.FieldConfig
            case _                                                                          => emptyVector
          }
        }
    }

    case object IssueConfig extends Type {
      override def determine(i: Input) =
        CallbackTo {
          i.state.customIssueTypeConfig.right.editorOption match {
            case Some(s) if s.updateCmd(i.project.config).isChanged => emptyVector :+ Location.IssueConfig
            case _                                                  => emptyVector
          }
        }
    }

    case object ReqTypeConfig extends Type {
      import pages.config.reqtypes.ReqTypeConfig.EditorState
      override def determine(i: Input) =
        CallbackTo {
          i.state.reqTypeConfig.right.editorOption match {
            case Some(EditorState.Custom(s)) if s.updateCmd(i.project.config).isChanged => emptyVector :+ Location.ReqTypeConfig
            case _                                                                      => emptyVector
          }
        }
    }

    case object TagConfig extends Type {
      override def determine(i: Input) =
        CallbackTo {
          i.state.tagConfig.right.editorOption match {
            case Some(\/-(s)) if s.updateCmd(i.project.config).isChanged => emptyVector :+ Location.TagConfig
            case Some(-\/(s)) if s.updateCmd(i.project.config).isChanged => emptyVector :+ Location.TagConfig
            case _                                                       => emptyVector
          }
        }
    }

    val values = AdtMacros.adtValues[Type]
  }

}
