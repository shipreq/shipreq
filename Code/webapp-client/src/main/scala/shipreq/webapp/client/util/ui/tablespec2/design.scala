package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react.ScalazReact.ReactS
import japgolly.scalajs.react.ScalazReact.ReactST
import shipreq.webapp.base.validation.ValidatorPlus
import monocle._
import scalaz.effect.IO
import scalaz._, Scalaz._

object design {

  /*

  *** edit data

  *** create new data

  *** delete data

  *** compose cells into row

  - cancel edit
  - correct
  - validate

  *** table/external constraints

  Row (for existing data)
  - [ ] external data source
  - [ ] editor with correction and validation applied
  - [ ] stateless. dirty and maybe original provided via props
  - [ ] saves on change
  - [ ] restore on escape
  - [ ] display validation errors
  - [ ] expose status to css: dirty or not, focus or not (?), valid or not.
  - [ ] external validation/constraints

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

  DataSource

  ---------------------------------------------------------------
  Done

  ValidatorPlus - correct & validate
  Editor - display with family of callback interfaces

  Cell
  - [/] external data source
  - [/] editor with correction and validation applied
  - [/] stateless. dirty and maybe original provided via props
  - [/] saves on change
  - [/] restore on escape
  - [/] display validation errors
  - [ ] expose status to css: dirty or not, focus or not (?), valid or not.
  - [/] external validation/constraints

  CellLockable
  - same as cell
  - ability to lock (ie. render in read only mode)

  ---------------------------------------------------------------
  Solution Ideas

  - Have a separate class or fn for each piece of behaviour.
  - Where ∀-types are concerned, rather than polluting the entire type hierarchy consider using abstract type members.
  - Types can be data representation like ADT, maybe impl should be considered separately.
  - Consider possible shape changes of each type.
  - Consider composability of each type.

*/

  case class EditorCallbacks[A, C[_]](onChange: A => C[Unit],
                                   onCancel: C[Unit],
                                   onEditFinished: C[Unit]) {

    def contramap[X](f: X => A): EditorCallbacks[X, C] =
      copy[X, C](onChange = onChange compose f)

    def contrafmap[X](f: X => C[A])(implicit F: Bind[C]): EditorCallbacks[X, C] =
      copy[X, C](onChange = x => F.bind(f(x))(a => this.onChange(a)))

//    def ***[B, D](f: EditorCallbacks[B, D]): EditorCallbacks[(A, B), (C, D)] =
//      EditorCallbacks(
//        i => (onChange(i._1), f.onChange(i._2)),
//        (onCancel, f.onCancel),
//        (onEditFinished, f.onEditFinished))
  }

  case class EditorInput[A, B, C[_], Q](ctx: Q,
                                      data: A,
                                  cssClass: String,
                                  editable: Option[EditorCallbacks[B, C]]) {

    def mapCtx[X](f: Q => X): EditorInput[A, B, C, X] =
      copy(ctx = f(ctx))

    def mapInput[X](f: A => X): EditorInput[X, B, C, Q] =
      copy(data = f(data))

    def contramapOutput[X](f: X => B): EditorInput[A, X, C, Q] =
      copy(editable = editable.map(_ contramap f))

//    def ***[A2, B2, C2](f: EditorInput[A2, B2, C2]): EditorInput[(A, A2), (B, B2), (C, C2)] =
//      EditorInput(
//        (data, f.data),
//        cssClass + " " + f.cssClass,
//        editable.flatMap(a => f.editable.map(b => a *** b))) // No!
  }

  case class Editor[A, B, C[_], Q, V](render: EditorInput[A, B, C, Q] => V) {
    def contramapCtx[X](f: X => Q): Editor[A, B, C, X, V] =
      Editor(i => render(i mapCtx f))

    def contramapInput[X](f: X => A): Editor[X, B, C, Q, V] =
      Editor(i => render(i mapInput f))

    def mapOutput[X](f: B => X): Editor[A, X, C, Q, V] =
      Editor(i => render(i contramapOutput f))


//    def ***[A2, B2, C2, V2](f: Editor[A2, B2, C2, V2]): Editor[(A, A2), (B, B2), (C, C2), (V, V2)] =
//      Editor(i => {
//        val i1: EditorInput[A, B, C](i.data._1, i.cssClass, i.editable.map(_.))
//        val v1 = render(???)
//        val v2 = f.render(???)
//        (v1, v2)
//      })
  }

  type EditorE[E, A, B, C[_], Q, V] = E => Editor[A, B, C, Q, V]

  def editors2[A,B,C[_]: Bind,Q,  A1,B1,Q1,V1,  A2,B2,Q2,V2](
                e1: Editor[A1,B1,C,Q1,V1], e2: Editor[A2,B2,C,Q2,V2],
                q1: Q => Q1, q2: Q => Q2,
                a1: A => A1, a2: A => A2,
                b1: (B, B1) => B, b2: (B, B2) => B,
                dirty: C[B])
  : Editor[A, B, C, Q, (V1,V2)] = {
    val conv = new EIConv[A, B, Q, C](dirty)
    Editor(i => {
      val v1 = conv(e1, q1, a1, b1)(i)
      val v2 = conv(e2, q2, a2, b2)(i)
      (v1, v2)
    })
  }
//  Editor(i => {
//    def b1u: B1 => C[B] = i => dirty.map(b1(_, i))
//    def b2u: B2 => C[B] = i => dirty.map(b2(_, i))
//    val i1 = EditorInput[A1, B1, C, Q1](q1(i.ctx), a1(i.data), i.cssClass, i.editable.map(_ contrafmap b1u))
//    val i2 = EditorInput[A2, B2, C, Q2](q2(i.ctx), a2(i.data), i.cssClass, i.editable.map(_ contrafmap b2u))
//    val v1 = e1.render(i1)
//    val v2 = e2.render(i2)
//    (v1, v2)
//  })

  class EIConv[A,B,Q, C[_]: Bind](dirty: C[B]) {
    def apply[A1, B1, Q1, V1](e1: Editor[A1,B1,C,Q1,V1], q1: Q => Q1, a1: A => A1, b1: (B, B1) => B): EditorInput[A,B,C,Q] => V1 =
      i => {
      val b1u: B1 => C[B] = i => dirty.map(b1(_, i))
      val i1 = EditorInput[A1, B1, C, Q1](q1(i.ctx), a1(i.data), i.cssClass, i.editable.map(_ contrafmap b1u))
      e1.render(i1)
    }
  }
  trait QConv[Q, I, QI[_]] {
    def apply[A](l: SimpleLens[I, A]): Q => QI[A]
  }

  def editors2i[I,C[_]: Bind,Q,QI[_], I1,V1, I2,V2](e1: Editor[I1,I1,C,QI[I1],V1], e2: Editor[I2,I2,C,QI[I2],V2],
                                                    f1: SimpleLens[I, I1], f2: SimpleLens[I, I2],
                                                    qconv: QConv[Q, I, QI],
                                                    dirty: C[I]) : Editor[I, I, C, Q, (V1, V2)] =
    editors2(e1, e2,
      qconv(f1), qconv(f2),
      f1.get, f2.get,
      f1.set, f2.set,
      dirty)

  // -------------------------------------------------------------------------------------------------------------------

  // external data (dirty and clean)
  // wire up editors - composition for EditorInput ?
  // correct, validate, save on edit finish
  def nopCB[S] = ReactS.retT[IO, S, Unit](())

  trait Row {
    type Ctx
    type S // State|Store
    type C[A] = ReactST[IO, S, A]
    type Clean
    type Dirty
    type Validated

    def clean: Ctx => Clean
    def dirty: C[Dirty] // Read all stores in row, to build X

    def validate: (Ctx, Dirty) => Option[Validated] // Verify X

    def saveRequired: (Clean, Validated) => Boolean // check store for last clean and abort if NOP

    def save: Validated => C[Unit]
    def lock: C[Unit]

    def onChange(ctx: Ctx): C[Unit] =
      dirty.flatMap(d =>
        validate(ctx, d) match {
          case Some(v) if saveRequired(clean(ctx), v) =>
            save(v) >> lock
          case _ =>
            nopCB[S]
        }
      )
  }

  // turn multiple editors into row
  // row has differnt callbacks


}
