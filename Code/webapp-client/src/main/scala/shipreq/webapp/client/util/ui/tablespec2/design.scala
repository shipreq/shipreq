package shipreq.webapp.client.util.ui.tablespec2

import shipreq.webapp.base.validation.ValidatorPlus

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
  Solution Ideas

  - Have a separate class or fn for each piece of behaviour.
  - Where ∀-types are concerned, rather than polluting the entire type hierarchy consider using abstract type members.
  - Types can be data representation like ADT, maybe impl should be considered separately.
  - Consider possible shape changes of each type.
  - Consider composability of each type.

*/

  case class EditorCallbacks[A, C](onChange: A => C,
                                    onCancel: C,
                                    onEditFinished: C) {
    def contramap[X](f: X => A): EditorCallbacks[X, C] =
      copy[X, C](onChange = onChange compose f)
  }

  case class EditorInput[A, B, C](data: A,
                                    cssClass: String,
                                    editable: Option[EditorCallbacks[B, C]]) {
    def map[X](f: A => X): EditorInput[X, B, C] =
      copy(f(data))

    def contramapOutput[X](f: X => B): EditorInput[A, X, C] =
      copy(editable = editable.map(_ contramap f))
  }

  case class Editor[A, B, C, V](render: EditorInput[A, B, C] => V) {
    def contramap[X](f: X => A): Editor[X, B, C, V] =
      Editor(i => render(i map f))

    def mapOutput[X](f: B => X): Editor[A, X, C, V] =
      Editor(i => render(i contramapOutput f))
  }

  type EditorE[E, A, B, C, V] = E => Editor[A, B, C, V]
}
