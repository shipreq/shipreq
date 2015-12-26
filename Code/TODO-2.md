* Each editor should get focus when opened -
  either make the focus callback passed in though CellEditors.startEdit work,
  or use each component's didMount CB like ReqTypeSelector.

* Test all the editors and all the expected features.
  * Preview (including when multiple are open/dirty)

* Clean up the new stuff. (Tidy up, reduce duplication)

* Squash commits - use as first real commit on a topic branch

* Would by nice to redo the .validation package but can also live with it for now.
  * It doesn't need the middle I type. (?)
  * The [CV]Part{,U} relatioships are poor. Redesign and simplify.
  * looseMsg & per-field error reporting to shit. Ignore it and *IF* it's necessary
    somewhere to have multiple fields + looseMsg, make that an addon construct.
  * Failure needs to turn into VDOM nicely (not just .toGenericText with CRs in it).

* CreationInterface will need to be updated to use the new stuff.

* Remove:
  * ColumnEditors
  * RemoteDataEditor
  * Cell
  * PolyMap (?)
  * reqtable.edit
  * LocalEditorFeature (?)

* Reorganise all UI packages include the shit in .lib.ui

