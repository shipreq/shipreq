package shipreq.utils.lib

import scala.collection.GenTraversable
import ShowSrc.State

/**
 * Converts an A into source code.
 */
trait ShowSrc[A] {
  def apply(state: State, a: A): Unit

  //def typeOf(a: A): String

  def prep(s: String): ShowSrc[A] =
    ShowSrc.prep(s)(apply)

  def narrow[B <: A]: ShowSrc[B] =
    this.asInstanceOf[ShowSrc[B]]
}

object ShowSrc {

  def generate[A](a: A)(implicit s: ShowSrc[A]): (Option[String], String) = {
    val state = State.empty
    s(state, a)
    val body = state.sb.toString()
    val head: Option[String] =
      if (state.prepared.isEmpty)
        None
      else {
        val sb = new StringBuilder
        for (p <- state.prepared) {
          sb append p
          sb append '\n'
        }
        Some(sb.toString())
      }
    (head, body)
  }

  def generateBlock[A: ShowSrc](a: A): String = {
    val (head, body) = generate(a)
    head match {
      case Some(h) => s"{\n$h$body\n}"
      case None    => body
    }
  }

  private def indent(i: Int, subj: String) = {
    val ind = "  " * i
    ind + subj.replaceAll("(?<=\n)(?!\n)", ind)
  }

  def generateVar[A: ShowSrc](name: String, a: A): String = {
    val (head, body) = generate(a)
    head match {
      case Some(h) => s"val $name = {\n${indent(1, h + body)}\n}\n"
      case None    => s"val $name =\n${indent(1, body)}\n"
    }
  }

  // ===================================================================================================================

  private val nop = (_: Any) => ()

  def apply[A](f: (State, A) => Unit): ShowSrc[A] =
    apply(nop, f)

  def apply[A](onUse: State => Unit, f: (State, A) => Unit): ShowSrc[A] =
    new ShowSrc[A] {
      override def apply(state: State, a: A): Unit = {
        onUse(state)
        f(state, a)
      }
    }

  def const[A](f: State => Unit): ShowSrc[A] =
    apply((sb, _a) => f(sb))

  def prep[A](s: String)(f: (State, A) => Unit): ShowSrc[A] =
    apply(_ prep s, f)

  // ===================================================================================================================

  implicit def stateToSB(s: State): StringBuilder = s.sb

  object State {
    def empty = State(collection.mutable.SortedSet.empty, new StringBuilder)
  }

  case class State(prepared: collection.mutable.SortedSet[String], sb: StringBuilder) {

    def prep(s: String): Unit =
      prepared += s

    def <~[A](a: A)(implicit s: ShowSrc[A]): Unit = s(this, a)

    def parens(f: State => Unit): Unit = {
      sb append '('
      f(this)
      sb append ')'
    }

    def arg[A: ShowSrc](preArg: String, a: A, post: Char = ','): Unit = {
      sb append preArg
      this <~ a
      sb append post
    }

    def fn1[A: ShowSrc](name: String, a: A, preArg: String = ""): Unit = {
      sb append name
      sb append '('
      arg(preArg, a, ')')
    }

    def fn2[A: ShowSrc, B: ShowSrc](name: String, a: A, b: B, preArg: String = ""): Unit = {
      sb append name
      sb append '('
      arg(preArg, a)
      arg(preArg, b, ')')
    }

    def fn3[A: ShowSrc, B: ShowSrc, C: ShowSrc](name: String, a: A, b: B, c: C, preArg: String = ""): Unit = {
      sb append name
      sb append '('
      arg(preArg, a)
      arg(preArg, b)
      arg(preArg, c, ')')
    }

    def fn4[A: ShowSrc, B: ShowSrc, C: ShowSrc, D: ShowSrc](name: String, a: A, b: B, c: C, d: D, preArg: String = ""): Unit = {
      sb append name
      sb append '('
      arg(preArg, a)
      arg(preArg, b)
      arg(preArg, c)
      arg(preArg, d, ')')
    }

    def fn5[A: ShowSrc, B: ShowSrc, C: ShowSrc, D: ShowSrc, E: ShowSrc](name: String, a: A, b: B, c: C, d: D, e: E, preArg: String = ""): Unit = {
      sb append name
      sb append '('
      arg(preArg, a)
      arg(preArg, b)
      arg(preArg, c)
      arg(preArg, d)
      arg(preArg, e, ')')
    }

    def fn6[A: ShowSrc, B: ShowSrc, C: ShowSrc, D: ShowSrc, E: ShowSrc, F: ShowSrc](name: String, a: A, b: B, c: C, d: D, e: E, f: F, preArg: String = ""): Unit = {
      sb append name
      sb append '('
      arg(preArg, a)
      arg(preArg, b)
      arg(preArg, c)
      arg(preArg, d)
      arg(preArg, e)
      arg(preArg, f, ')')
    }

    def cc1[A: ShowSrc](name: String, t: Option[A], preArg: String = ""): Unit = {
      val a = t.get
      fn1(name,a,preArg)
    }
    def cc2[A: ShowSrc, B: ShowSrc](name: String, t: Option[(A,B)], preArg: String = ""): Unit = {
      val (a,b) = t.get
      fn2(name,a,b,preArg)
    }
    def cc3[A: ShowSrc, B: ShowSrc, C: ShowSrc](name: String, t: Option[(A,B,C)], preArg: String = ""): Unit = {
      val (a,b,c) = t.get
      fn3(name,a,b,c,preArg)
    }
    def cc4[A: ShowSrc, B: ShowSrc, C: ShowSrc, D: ShowSrc](name: String, t: Option[(A,B,C,D)], preArg: String = ""): Unit = {
      val (a,b,c,d) = t.get
      fn4(name,a,b,c,d,preArg)
    }
    def cc5[A: ShowSrc, B: ShowSrc, C: ShowSrc, D: ShowSrc, E: ShowSrc](name: String, t: Option[(A,B,C,D,E)], preArg: String = ""): Unit = {
      val (a,b,c,d,e) = t.get
      fn5(name,a,b,c,d,e,preArg)
    }
    def cc6[A: ShowSrc, B: ShowSrc, C: ShowSrc, D: ShowSrc, E: ShowSrc, F: ShowSrc](name: String, t: Option[(A,B,C,D,E,F)], preArg: String = ""): Unit = {
      val (a,b,c,d,e,f) = t.get
      fn6(name,a,b,c,d,e,f,preArg)
    }

    def intercalate[S[x] <: GenTraversable[x], A: ShowSrc](as: S[A], sep: State => Unit) = {
      var first = true
      for (a <- as) {
        if (first)
          first = false
        else
          sep(this)
        this <~ a
      }
    }

    def varargs[S[x] <: GenTraversable[x], A: ShowSrc](as: S[A]) =
      parens(_.intercalate(as, _ append ','))

    def fnN[S[x] <: GenTraversable[x], A: ShowSrc](name: String, as: S[A]) = {
      this append name
      varargs(as)
    }
  }
}
