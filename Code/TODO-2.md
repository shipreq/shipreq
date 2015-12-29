* Reorganise all UI packages include the shit in .lib.ui
  * Reorg Style too.

* Add React Collapse and use in previews.

* Ensure custom-text fields trimmed after parsing.

* Cancel text-complete popup on blur.

* Would by nice to redo the .validation package but can also live with it for now.
  * It doesn't need the middle I type. (?)
  * The [CV]Part{,U} relatioships are poor. Redesign and simplify.
  * looseMsg & per-field error reporting to shit. Ignore it and *IF* it's necessary
    somewhere to have multiple fields + looseMsg, make that an addon construct.
  * Failure needs to turn into VDOM nicely (not just .toGenericText with CRs in it).
