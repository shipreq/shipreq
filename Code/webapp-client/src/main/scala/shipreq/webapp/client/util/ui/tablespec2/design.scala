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

    def mapC[X[_]](f: C ~> X): EditorCallbacks[A, X] =
      EditorCallbacks[A, X](a => f(onChange(a)),
      f(onCancel),
      f(onEditFinished))
  }

  case class EditorInput[A, B, C[_], W](world: W,
                                        data: A,
                                        cssClass: String,
                                        editable: Option[EditorCallbacks[B, C]]) {

    def mapInput[X](f: A => X): EditorInput[X, B, C, W] =
      copy(data = f(data))

    def contramapOutput[X](f: X => B): EditorInput[A, X, C, W] =
      copy(editable = editable.map(_ contramap f))

    def mapC[X[_]](f: C ~> X): EditorInput[A, B, X, W] =
      copy(editable = editable.map(_ mapC f))

    def mapWorld[X](f: W => X): EditorInput[A, B, C, X] =
      copy(world = f(world))
  }

  case class Editor[A, B, C[_], W, V](render: EditorInput[A, B, C, W] => V) {
    def contramapInput[X](f: X => A): Editor[X, B, C, W, V] =
      Editor(i => render(i mapInput f))

    def mapOutput[X](f: B => X): Editor[A, X, C, W, V] =
      Editor(i => render(i contramapOutput f))

    def contramapC[X[_]](f: X ~> C): Editor[A, B, X, W, V] =
      Editor(i => render(i mapC f))

    def contramapWorld[X](f: X => W): Editor[A, B, C, X, V] =
      Editor(i => render(i mapWorld f))
  }

  type EditorE[E, A, B, C[_], W, V] = E => Editor[A, B, C, W, V]

  def editors2_sameWorld[A,B,C[_]: Bind,W,  A1,B1,V1,  A2,B2,V2](
                                                              e1: Editor[A1,B1,C,W,V1], e2: Editor[A2,B2,C,W,V2],
                                                              a1: A => A1, a2: A => A2,
                                                              b1: (B, B1) => B, b2: (B, B2) => B,
                                                              dirty: C[B])
  : Editor[A, B, C, W, (V1,V2)] = {
    val conv = new EIConv_sameWorld[A, B, W, C](dirty)
    Editor(i => {
      val v1 = conv(e1, a1, b1)(i)
      val v2 = conv(e2, a2, b2)(i)
      (v1, v2)
    })
  }

  def editors2[A,B,C[_]: Bind,W,  A1,B1,W1,V1,  A2,B2,W2,V2](
                e1: Editor[A1,B1,C,W1,V1], e2: Editor[A2,B2,C,W2,V2],
                w1: W => W1, w2: W => W2,
                a1: A => A1, a2: A => A2,
                b1: (B, B1) => B, b2: (B, B2) => B,
                dirty: C[B])
  : Editor[A, B, C, W, (V1,V2)] = {
    val conv = new EIConv[A, B, W, C](dirty)
    Editor(i => {
      val v1 = conv(e1, w1, a1, b1)(i)
      val v2 = conv(e2, w2, a2, b2)(i)
      (v1, v2)
    })
  }

  class EIConv_sameWorld[A,B,W, C[_]: Bind](dirty: C[B]) {
    def apply[A1, B1, V1](e1: Editor[A1,B1,C,W,V1], a1: A => A1, b1: (B, B1) => B): EditorInput[A,B,C,W] => V1 =
      i => {
        val b1u: B1 => C[B] = i => dirty.map(b1(_, i))
        val i1 = EditorInput[A1, B1, C, W](i.world, a1(i.data), i.cssClass, i.editable.map(_ contrafmap b1u))
        e1.render(i1)
      }
  }
  class EIConv[A,B,W, C[_]: Bind](dirty: C[B]) {
    def apply[A1, B1, W1, V1](e1: Editor[A1,B1,C,W1,V1], w1: W => W1, a1: A => A1, b1: (B, B1) => B): EditorInput[A,B,C,W] => V1 =
      i => {
      val b1u: B1 => C[B] = i => dirty.map(b1(_, i))
      val i1 = EditorInput[A1, B1, C, W1](w1(i.world), a1(i.data), i.cssClass, i.editable.map(_ contrafmap b1u))
      e1.render(i1)
    }
  }

  trait WConv[WA, A, W[_]] {
    def apply[B](l: SimpleLens[A, B]): WA => W[B]
  }

  def editors2i[I,C[_]: Bind,W,WI[_], I1,V1, I2,V2](e1: Editor[I1,I1,C,WI[I1],V1], e2: Editor[I2,I2,C,WI[I2],V2],
                                                    f1: SimpleLens[I, I1], f2: SimpleLens[I, I2],
                                                    wconv: WConv[W, I, WI],
                                                    dirty: C[I]) : Editor[I, I, C, W, (V1, V2)] =
    editors2(e1, e2,
      wconv(f1), wconv(f2),
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

    def onChange(world: Ctx): C[Unit] =
      dirty.flatMap(d =>
        validate(world, d) match {
          case Some(v) if saveRequired(clean(world), v) =>
            save(v) >> lock
          case _ =>
            nopCB[S]
        }
      )
  }

  // turn multiple editors into row
  // row has differnt callbacks

}


object tryagain {

/*
  case class EditorCallbacks[A, C[_]](onChange: A => C[Unit],
                                      onCancel: C[Unit],
                                      onEditFinished: C[Unit]) {

    def contramap[X](f: X => A)                         : EditorCallbacks[X, C] = copy[X, C](onChange = onChange compose f)
    def contrafmap[X](f: X => C[A])(implicit F: Bind[C]): EditorCallbacks[X, C] = copy[X, C](onChange = x => F.bind(f(x))(a => this.onChange(a)))
    def mapC[X[_]](f: C ~> X)                           : EditorCallbacks[A, X] = EditorCallbacks[A, X](a => f(onChange(a)), f(onCancel), f(onEditFinished))
  }

  case class EditorInput[A, B, C[_]](data: A,
                                     cssClass: String,
                                     editable: Option[EditorCallbacks[B, C]]) {

    def mapInput[X](f: A => X)       : EditorInput[X, B, C] = copy(data = f(data))
    def contramapOutput[X](f: X => B): EditorInput[A, X, C] = copy(editable = editable.map(_ contramap f))
    def mapC[X[_]](f: C ~> X)        : EditorInput[A, B, X] = copy(editable = editable.map(_ mapC f))
  }

  case class Editor[A, B, C[_], V](render: EditorInput[A, B, C] => V) {
    def contramapInput[X](f: X => A): Editor[X, B, C, V] = Editor(i => render(i mapInput f))
    def mapOutput[X](f: B => X)     : Editor[A, X, C, V] = Editor(i => render(i contramapOutput f))
    def contramapC[X[_]](f: X ~> C) : Editor[A, B, X, V] = Editor(i => render(i mapC f))
  }
  */

  case class EditorCallbacks[A, C](onChange: A => C,
                                   onCancel: C,
                                   onEditFinished: C) {

    def contramap[X](f: X => A): EditorCallbacks[X, C] = copy[X, C](onChange = onChange compose f)
    def mapC[X](f: C => X)     : EditorCallbacks[A, X] = EditorCallbacks[A, X](a => f(onChange(a)), f(onCancel), f(onEditFinished))
  }

  case class EditorInput[A, B, C](data: A,
                                  cssClass: String,
                                  editable: Option[EditorCallbacks[B, C]]) {

    def mapInput[X](f: A => X)       : EditorInput[X, B, C] = copy(data = f(data))
    def contramapOutput[X](f: X => B): EditorInput[A, X, C] = copy(editable = editable.map(_ contramap f))
    def mapC[X](f: C => X)           : EditorInput[A, B, X] = copy(editable = editable.map(_ mapC f))

    def dimap[X, Y](f: A => X, g: Y => B): EditorInput[X, Y, C] =
      copy(data = f(data), editable = editable.map(_ contramap g))
  }

  case class Editor[A, B, C, V](render: EditorInput[A, B, C] => V) {
    def contramapInput[X](f: X => A): Editor[X, B, C, V] = Editor(i => render(i mapInput f))
    def mapOutput[X](f: B => X)     : Editor[A, X, C, V] = Editor(i => render(i contramapOutput f))
    def contramapC[X](f: X => C) : Editor[A, B, X, V] = Editor(i => render(i mapC f))

    def compose[A2,B2,C2,V2](that: Editor[A2,B2,C2,V2]) = Editor[(A,A2), B \/ B2, (C, C2), (V, V2)](
      i => {
        def ah1(e: EditorCallbacks[B \/ B2, (C,C2)]) = EditorCallbacks[B, C](
          b => e.onChange(-\/(b))._1,
          e.onCancel._1,
          e.onEditFinished._1)
        def ah2(e: EditorCallbacks[B \/ B2, (C,C2)]) = EditorCallbacks[B2, C2](
          b => e.onChange(\/-(b))._2,
          e.onCancel._2,
          e.onEditFinished._2)
        val i1 = EditorInput[A, B, C](i.data._1, i.cssClass, i.editable.map(ah1))
        val i2 = EditorInput[A2, B2, C2](i.data._2, i.cssClass, i.editable.map(ah2))
        val vv: (V, V2) = (this.render(i1), that.render(i2))
        vv
      })

    import shapeless._

    def compose2[A2,B2,C2,V2](that: Editor[A2,B2,C2,V2]) = Editor[
      A :: A2 :: HNil,
      B :+: B2 :+: CNil,
      C :: C2 :: HNil,
      V :: V2 :: HNil](
      i => {
        type Bs = B :+: B2 :+: CNil
        type EC = EditorCallbacks[Bs, C :: C2 :: HNil]
        def ah1(e: EC) = EditorCallbacks[B, C](
          b => e.onChange(Coproduct[Bs](b)).head,
          e.onCancel.head,
          e.onEditFinished.head)
        def ah2(e: EC) = EditorCallbacks[B2, C2](
          b => e.onChange(Coproduct[Bs](b)).tail.head,
          e.onCancel.tail.head,
          e.onEditFinished.tail.head)
        val i1 = EditorInput[A, B, C](i.data.head, i.cssClass, i.editable.map(ah1))
        val i2 = EditorInput[A2, B2, C2](i.data.tail.head, i.cssClass, i.editable.map(ah2))
        val vv: V :: V2 :: HNil = this.render(i1) :: that.render(i2) :: HNil
        vv
      })

  }

  type EditorE[E, A, B, C, V] = E => Editor[A, B, C, V]

  import shapeless._
  import ops.hlist._

  trait tst[AX,BX,CX,VX, AY,BY,CY,VY, AZ,BZ,CZ,VZ] {
    val x: Editor[AX,BX,CX,VX]
    val y: Editor[AY,BY,CY,VY]
    val z: Editor[AZ,BZ,CZ,VZ]

    val a = x compose y compose z

    val es = x :: y :: z :: HNil


    val h: String :: Int :: Boolean :: HNil = ???
    val fns: (String => Long) :: (Int => Long) :: (Boolean => Long) :: HNil

//    class ApplyFns extends Poly {
//      implicit def f1:
//    }

//    def test[L <: HList](l: L)(implicit ihc: IsHCons[L]): Unit = {
//      ihc.
//      val h = l.head
//      println(h)
//      l.tail match {
//        case HNil => ()
//        case t => test(t)
//      }
//    }
  }


}