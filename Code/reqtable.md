# ReqTable Upgrade: Planning and Progress

### Planning

CEF.ForProject
- State
  - Map[ReqId, ForReq.State]
  - Map[ReqCodeGroupId, ForReqCodeGroup.State]
  - Map[UseCaseStepId, Editor]
- Props
  - Static
  - Editability.ForProject
  - State
  - forReq(reqId): CEF.ForReq

CEF.ForReq
- State = Map[EFKey.ForReq, Editor]
- Props
  - Reusable[whatever needed for next stage]
  - Editability.ForReq
  - State
  - apply(EFKey.ForReq): CEF.Cell

CEF.Cell
- State = Option[Editor]
- Props
  - Reusable[whatever needed for start edit impl]
  - Permission
  - State
  - startEdit(Callback): Option[Callback]



D0
- [RW] startEdit: Option[Callback]
- [R-] editor: Option[Editor]




ReqTable
↑ D2.State[ReqId | ReqCodeId, Column]
↑ Editability.ForProject

--> Row >: ReqRow <-- apply(ReqId)
↑ D1.State[Column]
↑ Editability.ForReq
↑ Static → AsyncFeature.D2 → Lens[S, D2.State]

--> Row >: RcgRow <-- apply(ReqId)
↑ D1.State[Column]
↑ Editability.ForRCG
↑ Static → AsyncFeature.D1 → Lens[S, D1.State]

----> Cell >: ReqCell <-- apply(EditFieldKey.ForReq)
↑ D0.State
↑ Option[Reusable[Callback]]
  Permission ↰
  Static → AsyncFeature.D0 → Lens[S, D0.State] ↰



WRT features:

State is state, it needn't be passed to components directly.
Props
- contains state or derivation of state
- may contain other stuff
- reusable
- does this not replace the need of Features? Props & State all that's needed?





Determine: are those initial editor values really needed and why?
Create Dn.Props which combines EditApplicability & State, meaning
- there's no need to pass a whole project to `startEdit()`
- the entire feature has row and cell reusability (woot!)
- maybe `EditorInstance#render` should use Props instead of `pxAllowEdit`
If Dn.Props exists, what's the purpose of Dn.Feature? Can it not just be logic embedded in the props?



EditApplicability

- apply(ReqId): EditApplicabilityForReq
  - apply(EditFieldKey.ForReq): Permission

- apply(UseCaseId): EditApplicabilityForUseCase
  - apply(EditFieldKey.ForUseCase): Permission

- apply(ReqCodeId): EditApplicabilityForReqCodeGroup
  - apply(EditFieldKey.ForReqCodeGroup): Permission

feature(ea: EditApplicabilityForReq         , e: Editor.ForReq)          = ea(e.key).option(...)
feature(ea: EditApplicabilityForUseCase     , e: Editor.ForUseCase)      = ea(e.key).option(...)
feature(ea: EditApplicabilityForReqCodeGroup, e: Editor.ForReqCodeGroup) = ea(e.key).option(...)

startEdit: Option[Callback]




CEF feature should contain async feature
CEF state+ should contain async state (why? (-) convenience of omitting one param. (+) always needed, mandatory...)

CEF.D0
- State
  - Option[Status]
- Feature
  - R/W access to some S
  - lens from S to CEF.D0.State
  - async feature 0
    - [init] W access to AAF.D0.State

--------------------------------------------------------------------------------

Needs:

ReqTable
- row = req | RCG
- column = ...
- Easy way to render (row,column): view + editor + async
- Reusability by row: view + editor + async
- Reusability by (row,column): view + editor + async

ReqDetail
- Easy way to render (req,cell): view + editor + async

Issues screen
- Easy way to render (req,cell): view + editor + async


Easy way to render means:
* `.render()` and done - no getting edit state, async state manually then having a cell-specific fallback for view.
* pass D{0,1,2} state and that's it - no separate edit/async states in props
* pass D{0,1,2} feature and that's it - no separate edit/async features in static props

Probably smart to have a ViewFeature (ew) and then have a feature that builds on top of View/Editor/Async.
Then share the "Cell" ADT between them all.

Question - what about non-req rows in ReqTable?
ReqTable's problem right? Have row-based renderers with Req-rows using new feature and RCG-rows using special thing.

Use in new ReqTable then update ReqDetail to use it?
Or create, update ReqDetail then when done, create new ReqTable?



### Progress

* Integrate AAF into CEF.
  It has a bunch of async shit in it anyway and always *needs* to be used in conjunction with AAF.
  Just integrate it and have nice simple usage.
  Specifically, one should only need to pass around CEF state/feature instead of CEF & AAF state/feature.

* It's going to be super common that I need to have screens where I present project data.
  Often I want it to be editable too. And I need reusability for ReqTable.
  There's enough usage and stability (and experience) now to look at what exists and what I need and
  find a general solution. CEF is halfway there but only for Write, not for Read.

* Older TODOs:
  * No content.
  * All dead.
  * All filtered out.
  * New button & form.
  * Sort form.
  * Filter form (∅,ok,ko) & help.
  * Summary math.
  * Column selection.
  * Delete/restore buttons.
  * Restore reusability on ReqTable and editors
  * Redo ReqTable rowlocking async
