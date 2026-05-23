package shipreq.webapp.member.project.event

import shipreq.webapp.base.data.{ProjectRole, UserId}
import Event._

object EventPermission {

  /**
   * Note: the event is assumed to be valid. This is not the place to check the validity of events.
   */
  def requiredRole(author: UserId, event: Event): ProjectRole =
    event match {

      case _: ProjectNameSet
         | _: ProjectTemplateApply
         =>
          ProjectRole.Admin

      case a: AccessUpdate =>
        if (a.userId ==* author && a.newRole.isEmpty)
          // Users are allowed to remove themselves from projects
          ProjectRole.min
        else
          // Otherwise, only admin can add/remove users to/from a project
          ProjectRole.Admin

      case _: ApplicableTagCreateV1
         | _: ApplicableTagCreate
         | _: ApplicableTagUpdateV1
         | _: ApplicableTagUpdate
         | _: CodeGroupCreate
         | _: CodeGroupsDelete
         | _: CodeGroupUpdate
         | _: ContentRestore
         | _: CustomIssueTypeCreate
         | _: CustomIssueTypeDelete
         | _: CustomIssueTypeRestore
         | _: CustomIssueTypeUpdate
         | _: CustomReqTypeCreateV1
         | _: CustomReqTypeCreate
         | _: CustomReqTypeDelete
         | _: CustomReqTypeDeleteHard
         | _: CustomReqTypeDeleteSoft
         | _: CustomReqTypeRestore
         | _: CustomReqTypeUpdateV1
         | _: CustomReqTypeUpdate
         | _: FieldCustomDelete
         | _: FieldCustomImpCreateV1
         | _: FieldCustomImpCreate
         | _: FieldCustomImpUpdateV1
         | _: FieldCustomImpUpdate
         | _: FieldCustomRestore
         | _: FieldCustomTagCreateV1
         | _: FieldCustomTagCreate
         | _: FieldCustomTagUpdateV1
         | _: FieldCustomTagUpdate
         | _: FieldCustomTextCreateV1
         | _: FieldCustomTextCreate
         | _: FieldCustomTextUpdateV1
         | _: FieldCustomTextUpdate
         | _: FieldReposition
         | _: FieldStaticAdd
         | _: FieldStaticRemove
         | _: GenericReqCreate
         | _: GenericReqTitleSet
         | _: GenericReqTypeSet
         | _: ManualIssueCreate
         | _: ManualIssueDelete
         | _: ManualIssueUpdate
         | _: ReqCodesPatch
         | _: ReqFieldCustomTextSet
         | _: ReqImplicationsPatch
         | _: ReqsDelete
         | _: ReqTagsPatch
         | _: SavedViewCreateV1
         | _: SavedViewCreate
         | _: SavedViewDefaultSet
         | _: SavedViewDelete
         | _: SavedViewUpdateV1
         | _: SavedViewUpdate
         | _: TagDelete
         | _: TagGroupCreate
         | _: TagGroupUpdate
         | _: TagRestore
         | _: UseCaseCreate
         | _: UseCaseStepCreate
         | _: UseCaseStepDelete
         | _: UseCaseStepRestore
         | _: UseCaseStepShiftLeft
         | _: UseCaseStepShiftRight
         | _: UseCaseStepUpdate
         | _: UseCaseTitleSet
        =>
          ProjectRole.Collaborator
    }
}
