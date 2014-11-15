package shipreq.webapp.client.util.ui.tablespec2

import scalaz.effect.IO

object design {

  /*

  *** edit data

  *** create new data

  *** delete data

  *** compose into row

  - cancel edit
  - correct
  - validate

  *** table/external constraints
  
  ValidatorPlus - correct & validate
  Editor - display with family of callback interfaces

  Cell
  - external data source
  - editor with correction and validation applied
  - stateless. dirty and maybe original provided via props
  - saves on change
  - restore on escape
  - display validation errors
  - expose status to css: dirty or not, focus or not (?), valid or not.
  - external validation/constraints

  CellLockable
  - same as cell
  - ability to lock (ie. render in read only mode)

  Row (for existing data)
  - render multiple cells
  - state mods at the datum/class level (eg. remove #7, here's a new version of TagGroup #12)
  - row status: {locked, iofail, open, dirty}

  Row (for new data)
  - render multiple cells
  - state mods at the datum/class level (eg. abort new, get values )
  - row status: {locked, iofail, open, dirty}

  Deletion
  - restore/delete button
  - filter soft-deleted rows

  TableConstraint

  ---------------------------------------------------------------

  Separate class or fn for each piece of behaviour

*/

  case class EditorCallbacks[D, Callback](onChange: D => Callback,
                                          onCancel: Callback,
                                          onEditFinished: Callback)

  case class EditorInput[D, Callback](data: D,
                                      cssClass: String,
                                      editing: Option[EditorCallbacks[D, Callback]])

  type Editor[D, Callback, V] = EditorInput[D, Callback] => V

  // errors + editor
  //type WithError[E, V] = EditorInput[D, Callback] => V

}