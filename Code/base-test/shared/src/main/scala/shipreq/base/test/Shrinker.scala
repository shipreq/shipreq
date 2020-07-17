package shipreq.base.test

import scala.collection.View
import scala.collection.immutable.IndexedSeqOps
import shipreq.base.util.NonEmptyArraySeq

final case class Shrinker[A](shrink: A => View[RoseTree[A]]) extends AnyVal {

  def emap(f: A => A): Shrinker[A] =
    new Shrinker[A](shrink(_).map(_.map(f)))

  def zoom[B](f: B => A)(g: (B, A) => B): Shrinker[B] =
    new Shrinker(b => shrink(f(b)).map(_.map(a => g(b, a))))

  def filter(f: A => Boolean): Shrinker[A] =
    new Shrinker[A](shrink(_).filter(t => f(t.value)))

  def tuple2: Shrinker[(A, A)] =
    Shrinker.tuple2(this, this)

  def start(a: A): RoseTree[A] =
    RoseTree(a, shrink(a))
}

object Shrinker {

  def id[A]: Shrinker[A] =
    Shrinker(_ => View.empty)

  def recursive[A](f: (A => RoseTree[A]) => Shrinker[A]): Shrinker[A] = {
    var self: Shrinker[A] = apply(null)
    self = f(a => RoseTree(a, self.shrink(a)))
    self
  }

  def constValues[A: UnivEq](values: A*): Shrinker[A] = {
    val as = values.toSet
    Shrinker(a => View.fromIteratorProvider(() => (as - a).iterator.map(RoseTree(_, View.empty))))
  }

  def iterator[A](f: A => Iterator[RoseTree[A]]): Shrinker[A] =
    Shrinker(a => View.fromIteratorProvider(() => f(a)))

  def combine[A](ss: Shrinker[A]*): Shrinker[A] = {
    val fs = ss.toVector
    var self: Shrinker[A] = apply(null)
    self =
      fs.length match {
        case 0 => Shrinker(_ => View.empty)
        case 1 => fs.head
        case len =>
          new Shrinker[A](root => {
            val views = fs.map(_.shrink(root))
            View.fromIteratorProvider(() => {
              var idx = 0
              val is = views.map(_.iterator)
              new Iterator[RoseTree[A]] {
                override def hasNext = is.exists(_.hasNext)
                override def next() = {
                  @tailrec def go(): RoseTree[A] = {
                    val it = is(idx)
                    idx = (idx + 1) % len
                    if (it.hasNext)
                      it.next()
                    else
                      go()
                  }
                  val childTree = go()

                  // Discard children (by f) and recreate (by fs)
                  val child = childTree.value
                  RoseTree(child, self.shrink(child))
                }
              }
            })
          })
      }
    self
  }

  def tuple2[A, B](sa: Shrinker[A], sb: Shrinker[B]): Shrinker[(A, B)] =
    combine(
      sa.zoom[(A, B)](_._1)((t, a) => (a, t._2)),
      sb.zoom[(A, B)](_._2)((t, a) => (t._1, a)),
    )

  def tuple3[A, B, C](sa: Shrinker[A], sb: Shrinker[B], sc: Shrinker[C]): Shrinker[(A, B, C)] =
    combine(
      sa.zoom[(A, B, C)](_._1)((t, a) => (a, t._2, t._3)),
      sb.zoom[(A, B, C)](_._2)((t, a) => (t._1, a, t._3)),
      sc.zoom[(A, B, C)](_._3)((t, a) => (t._1, t._2, a)),
    )

  def tuple4[A, B, C, D](sa: Shrinker[A], sb: Shrinker[B], sc: Shrinker[C], sd: Shrinker[D]): Shrinker[(A, B, C, D)] =
    combine(
      sa.zoom[(A, B, C, D)](_._1)((t, a) => (a, t._2, t._3, t._4)),
      sb.zoom[(A, B, C, D)](_._2)((t, a) => (t._1, a, t._3, t._4)),
      sc.zoom[(A, B, C, D)](_._3)((t, a) => (t._1, t._2, a, t._4)),
      sd.zoom[(A, B, C, D)](_._4)((t, a) => (t._1, t._2, t._3, a)),
    )

  lazy val char: Shrinker[Char] = {
    val pool = Set[Char](' ', 'a', '0').view
    recursive[Char] { f =>
      apply { c =>
        pool.filter(_ < c).map(f)
      }
    }
  }

  lazy val removeChars: Shrinker[String] =
    recursive[String] { f =>
      iterator { root =>
        root.indices.iterator.map { idx =>
          val child = root.patch(idx, Nil, 1)
          f(child)
        }
      }
    }

  def shrinkChars(f: Shrinker[Char]): Shrinker[String] =
    recursive[String] { r =>
      iterator { root =>
        root.indices.iterator.flatMap { idx =>
          f.shrink(root(idx)).map { newChar =>
            val child = root.updated(idx, newChar.value)
            r(child)
          }
        }
      }
    }

  def string(sc: Shrinker[Char]): Shrinker[String] =
    combine(removeChars, shrinkChars(sc))

  lazy val string: Shrinker[String] =
    string(char)

  lazy val nonEmptyRemoveChars: Shrinker[String] =
    recursive[String] { f =>
      iterator { root =>
        if (root.length <= 1)
          Iterator.empty
        else
          root.indices.iterator.map { idx =>
            val child = root.patch(idx, Nil, 1)
            f(child)
          }
      }
    }

  def nonEmptyString(sc: Shrinker[Char]): Shrinker[String] =
    combine(nonEmptyRemoveChars, shrinkChars(sc))

  lazy val nonEmptyString: Shrinker[String] =
    nonEmptyString(char)

  def removeElements[F[x] <: IndexedSeqOps[x, F, F[x]], A]: Shrinker[F[A]] =
    recursive[F[A]] { f =>
      iterator { root =>
        root.indices.iterator.map { idx =>
          val child = root.patch(idx, Nil, 1)
          f(child)
        }
      }
    }

  def shrinkElements[F[x] <: IndexedSeqOps[x, F, F[x]], A](f: Shrinker[A]): Shrinker[F[A]] =
    recursive[F[A]] { r =>
      iterator { root =>
        root.indices.iterator.flatMap { idx =>
          f.shrink(root(idx)).map { newElement =>
            val child = root.updated(idx, newElement.value)
            r(child)
          }
        }
      }
    }

  def neasRemoveElements[A]: Shrinker[NonEmptyArraySeq[A]] = {
    val w = removeElements[ArraySeq, A]
    recursive[NonEmptyArraySeq[A]] { r =>
      iterator { root =>
        if (root.length == 1)
          Iterator.empty
        else
          w.shrink(root.whole).iterator.flatMap(t =>
            NonEmptyArraySeq.option(t.value).iterator.map(r)
          )
      }
    }
  }

  def neasShrinkElements[A](f: Shrinker[A]): Shrinker[NonEmptyArraySeq[A]] =
    recursive[NonEmptyArraySeq[A]] { r =>
      iterator { root =>
        root.indices.iterator.flatMap { idx =>
          f.shrink(root.unsafeApply(idx)).map { newElement =>
            val child = root.updated(idx, newElement.value)
            r(child)
          }
        }
      }
    }

  def maybe[A](accept: A => Boolean, f: A => A): Shrinker[A] =
    recursive[A] { r =>
      iterator { root =>
        val ok = try accept(root) catch {case _: Throwable => false}
        if (ok) {
          val a = f(root)
          Iterator.single(r(a))
        } else
          Iterator.empty
      }
    }

}