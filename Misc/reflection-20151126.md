It happened again, this time with RemoteDataEditor.
Again: wrote code that seemed abstract and reusable, used in two places, later tried to add an
unrelated feature to an editor that uses RemoteDataEditor and got stuck, can't do it.


Feature Conflation
==================

* RemoteDataEditor conflates remote-comms with editing. It's doing more than one thing.
  The remote-comms feature is about locked/failed-retry.

  This is bad because:
  * It prevents me from using the remote-comms feature without taking on the editor state & constraints.

  Better would be:
  * Having a separate feature for remote-comms, and one (or more) for editor logic.

  Old .lib.ui.Persistence has the same problem: it prioritises immediate convenience by trying to do
  everything (editor state, validation, remote-comms status, the remote call itself) all by itself
  in one point.


State Source/Shape Assumptions
==============================

* RemoteDataEditor decides where/how state will be stored. My conflicting feature also tried to
  declare how edit-state and saved-values are stored.

  This is bad because:
  * Each new UI component/screen may need to store edit/saved data differently.
  * I want to add a feature to get its behaviour, not to decide/manage my state.

  Better would be:
  * Each feature should be able to ask to read/modify state with the details on source/shape omitted.
    A Lens works well for this.
  * Identify where state access/shape is being assumed and queried to perform a calculation.
    Instead, just require that the result of the calculation be provided externally when needed.


Ask Much, Return Little
=======================

* RemoteDataEditor takes many arguments (everything it thinks is needed) and condenses it into
  something finished/complete/smaller (StateFor[A] which renders itself).

  This is bad because:
  * I need control over each part of what's being built (eg to add other features).
  * For something on the outside to change its own logic, RemoteDataEditor must be changed to accept
    and process new info.

  Better would be:
  * Everything required by the feature should be offered/exposed. The merging/application of those
    things should be possible externally.
  * It shouldn't close over (i.e. constrain to a type and require) the external thing that it is
    supposed to enrich.
  * In other words, don't build/give/return me a RichEditor; give me all the things I need to apply
    to an existing editor, to make it a RichEditor.


Solution-candidate problems
===========================

An experimental solution exists in FocusPreviewExperiment.
It introduces new problems:

* To implement a feature, the lowest-level component needs to wire-up all the feature's callbacks.
  This is bad because it's a little verbose and is easy to miss a wire, or miswire.
  This is good because it's clear what it does & how it works, and dev has full freedom & control
  over all aspects of their UI.

* We lose reuse over the wiring logic. For example, if we want ctrl-enter to be the keypress for
  commit, we might accidently make it shift-enter in just one piece of the one UI.
  This could be mitigated or solved later by adding a new contruct - the good news is that it would
  be an optional addition to the features, rather than being incompatible with them.

