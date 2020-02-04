package shipreq.webapp.client.project.app.root

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react.{CallbackTo, Reusability}
import japgolly.univeq._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.{Open, Project, ReqCodeGroupId, ReqId, UseCases}
import shipreq.webapp.base.lib.KeyboardTheme
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.project.feature.create.NewEditorArgs
import shipreq.webapp.client.project.lib.DataReusability._

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

  final case class Input(state      : State,
                         projectName: Project.Name,
                         useCases   : UseCases)

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
            case Some(s) if s.corrected !=* input.projectName => emptyVector :+ Location.ProjectName
            case _                                            => emptyVector
          }
        }
    }

    case object CreationFields extends Type {
      import shipreq.webapp.client.project.feature.CreateFeature
      import CreateFeature.{FieldKey, RowKey}

      private val uselessArgs = NewEditorArgs(None, false, None, "", KeyboardTheme.Shortcuts.empty)

      override def determine(input: Input) =
        CallbackTo {
          var ls = Vector.empty[Location]

          // New manual issue
          if (input.state.issuesPage.newIssue.open is Open)
            for (e <- input.state.create(RowKey.ManualIssue)(FieldKey.ManualIssue))
              // .isRight here means we have valid NonEmptyText
              // .isLeft is only true if the field is blank
              if (e.value(uselessArgs).isRight)
                ls :+= Location.ManualIssues

          ls
        }
    }

    case object Editors extends Type {
      import shipreq.webapp.client.project.feature.EditorFeature
      import EditorFeature.{Editor, FieldKey, RowKey}

      override def determine(input: Input) =
        CallbackTo {

          val isChanged: Editor[Nothing, Any] => Boolean =
            // Rarely, but sometimes, ids and their data are hard-deleted.
            // When that happens, the editor state isn't deleted (probably it should be) but the point is, calling
            // .change here can result in stuff like:
            //     java.util.NoSuchElementException: key not found: UseCaseStepId(201)
            _.change.attempt.runNow() match {
              case Right(pv) => pv.isChanged
              case Left(_)   => false
            }

          def countFields(f: EditorFeature.State.ForFields, l: Location) = {
            var ls = List.empty[Location]
            for (e <- f.values)
              if (isChanged(e))
                ls ::= l
            ls
          }

          input.state.edit.iterator.flatMap {
            case (row, fields) =>

              row match {
                case RowKey.GenericReq(id) => countFields(fields, Location.Req(id))
                case RowKey.UseCase(id)    => countFields(fields, Location.Req(id))
                case RowKey.ManualIssues   => countFields(fields, Location.ManualIssues)
                case RowKey.CodeGroup(id)  => countFields(fields, Location.ReqCodeGroup(id))

                case RowKey.UseCaseSteps =>
                  fields.iterator.flatMap {
                    case (field, editor) =>
                      field match {
                        case FieldKey.UseCaseStep(stepId) if isChanged(editor) =>
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

    val values = AdtMacros.adtValues[Type]
  }

}
