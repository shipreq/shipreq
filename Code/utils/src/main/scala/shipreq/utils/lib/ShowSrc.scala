package shipreq.utils.lib

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.GenTraversable
import ShowSrc.State

/**
 * Converts an A into source code.
 */
trait ShowSrc[A] {
  def apply(state: State, a: A): Unit

  def init(s: String): ShowSrc[A] =
    ShowSrc.init(s)(apply)

  def narrow[B <: A]: ShowSrc[B] =
    this.asInstanceOf[ShowSrc[B]]

  def intoTmpVar = intoVar("tmp")

  def intoVar(name: String): ShowSrc[A] = {
    val self = this
    new ShowSrc[A] {
      override def apply(state: State, a: A): Unit =
        state.intoVar(name, self(_, a))
    }
  }
}

object ShowSrc {

  private def nonEmptyStr(f: StringBuilder => Unit): Option[String] = {
    val sb = new StringBuilder
    f(sb)
    Some(sb.toString.trim).filter(_.nonEmpty)
  }

  private def indent(i: Int)(subj: String) = {
    val ind = "  " * i
    ind + subj.replaceAll("(?<=\n)(?!\n)", ind)
  }

  def generate[A](a: A, tmpvarDecl: String = "def")(implicit s: ShowSrc[A]): (Option[String], Option[String], String) = {
    val state = State.empty
    s(state, a)
    val result = state.toResult

    val head = nonEmptyStr { sb =>
      for (p <- result.initLines) {
        sb append p
        sb append '\n'
      }
    }

    val tmpvars = nonEmptyStr { sb =>
      for ((k,v) <- result.tmpVars) {
        sb append tmpvarDecl
        sb append ' '
        sb append k
        sb append " = "
        sb append v
        sb append '\n'
        sb append '\n'
      }
    }

    (head, tmpvars, result.body)
  }

  /**
   * Both the JVM and Scala have limits on the size of an expression.
   * If this fails, use [[generateObject()]].
   */
  def generateExpr[A: ShowSrc](a: A): String = {
    val (head, tmpvars, body) = generate(a)
    val l = head.toList ::: tmpvars.toList ::: body :: Nil
    if (l.size == 1)
      l.head
    else
      l.mkString("{\n", "\n", "\n}")
  }

  def generateObject[A: ShowSrc](pkg: String, obj: String, term: String)(a: A): String = {
    val (head, tmpvars, body) = generate(a, "def")

    // Stupid issue #2: SBT incremental compiler crashes on objects with too many methods
    val partitions = {
      val sep = "\n\n"

      val tmpvarGroups =
        (tmpvars map indent(1) getOrElse "").split(sep).grouped(1000).toVector

      tmpvarGroups.zipWithIndex.map{case (xs, i) =>
        val o = s"__${obj}_${i + 1}"
        s"""
          |object $o {
          |${xs mkString sep}
          |}
          |import ${o}._
        """.stripMargin.trim
      }.mkString("\n\n")
    }

    s"""
       |package $pkg
       |
       |${head getOrElse ""}
       |
       |$partitions
       |
       |object $obj {
       |  val $term =
       |${indent(2)(body)}
       |}
     """.stripMargin.trim
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

  def init[A](s: String)(f: (State, A) => Unit): ShowSrc[A] =
    apply(_ init s, f)

  // ===================================================================================================================

  case class Result(initLines: Stream[String],
                    tmpVars  : Stream[(String, String)],
                    body     : String)

  implicit def stateToSB(s: State): StringBuilder = s.sb

  object State {
    def empty = new State(
      mutable.SortedSet.empty,
      mutable.MutableList.empty,
      mutable.Set.empty,
      mutable.Map.empty,
      new StringBuilder)
  }

  class State(initLines   : mutable.SortedSet[String],
              tmpVars     : mutable.MutableList[(String, String)],
              tmpVarNames : mutable.Set[String],
              tmpVarValues: mutable.Map[String, String],
              val sb      : StringBuilder) {

    // Stupid issue #1: JVM & compile crash when expressions are too long
    private def getFreeName(name: String): String = {
      @tailrec
      def go(i: Int): String = {
        val n = name + i
        if (tmpVarNames contains n)
          go(i + 1)
        else
          n
      }
      if (tmpVarNames contains name)
        go(2)
      else
        name
    }

    def intoVar(name: String, f: State => Unit): Unit = {
      val state2 = new State(initLines, tmpVars, tmpVarNames, tmpVarValues, new StringBuilder)
      f(state2)
      val varValue = state2.sb.toString
      val varName2 =
        tmpVarValues.get(varValue) getOrElse {
          val varName = getFreeName(name)
          tmpVars += (varName -> varValue)
          tmpVarNames += varName
          tmpVarValues.update(varValue, varName)
          varName
        }
      sb append varName2
    }

    def toResult = Result(initLines.toStream, tmpVars.toStream, sb.toString)

    def init(s: String): Unit =
      initLines += s

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

    def fn7[A: ShowSrc, B: ShowSrc, C: ShowSrc, D: ShowSrc, E: ShowSrc, F: ShowSrc, G: ShowSrc](name: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, preArg: String = ""): Unit = {
      sb append name
      sb append '('
      arg(preArg, a)
      arg(preArg, b)
      arg(preArg, c)
      arg(preArg, d)
      arg(preArg, e)
      arg(preArg, f)
      arg(preArg, g, ')')
    }

    def fn8[A: ShowSrc, B: ShowSrc, C: ShowSrc, D: ShowSrc, E: ShowSrc, F: ShowSrc, G: ShowSrc, H: ShowSrc](name: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, preArg: String = ""): Unit = {
      sb append name
      sb append '('
      arg(preArg, a)
      arg(preArg, b)
      arg(preArg, c)
      arg(preArg, d)
      arg(preArg, e)
      arg(preArg, f)
      arg(preArg, g)
      arg(preArg, h, ')')
    }

    def fn9[A: ShowSrc, B: ShowSrc, C: ShowSrc, D: ShowSrc, E: ShowSrc, F: ShowSrc, G: ShowSrc, H: ShowSrc, I: ShowSrc](name: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, preArg: String = ""): Unit = {
      sb append name
      sb append '('
      arg(preArg, a)
      arg(preArg, b)
      arg(preArg, c)
      arg(preArg, d)
      arg(preArg, e)
      arg(preArg, f)
      arg(preArg, g)
      arg(preArg, h)
      arg(preArg, i, ')')
    }

    def fn10[A: ShowSrc, B: ShowSrc, C: ShowSrc, D: ShowSrc, E: ShowSrc, F: ShowSrc, G: ShowSrc, H: ShowSrc, I: ShowSrc, J: ShowSrc](name: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, preArg: String = ""): Unit = {
      sb append name
      sb append '('
      arg(preArg, a)
      arg(preArg, b)
      arg(preArg, c)
      arg(preArg, d)
      arg(preArg, e)
      arg(preArg, f)
      arg(preArg, g)
      arg(preArg, h)
      arg(preArg, i)
      arg(preArg, j, ')')
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
    def cc7[A: ShowSrc, B: ShowSrc, C: ShowSrc, D: ShowSrc, E: ShowSrc, F: ShowSrc, G: ShowSrc](name: String, t: Option[(A,B,C,D,E,F,G)], preArg: String = ""): Unit = {
      val (a,b,c,d,e,f,g) = t.get
      fn7(name,a,b,c,d,e,f,g,preArg)
    }
    def cc8[A: ShowSrc, B: ShowSrc, C: ShowSrc, D: ShowSrc, E: ShowSrc, F: ShowSrc, G: ShowSrc, H: ShowSrc](name: String, t: Option[(A,B,C,D,E,F,G,H)], preArg: String = ""): Unit = {
      val (a,b,c,d,e,f,g,h) = t.get
      fn8(name,a,b,c,d,e,f,g,h,preArg)
    }
    def cc9[A: ShowSrc, B: ShowSrc, C: ShowSrc, D: ShowSrc, E: ShowSrc, F: ShowSrc, G: ShowSrc, H: ShowSrc, I: ShowSrc](name: String, t: Option[(A,B,C,D,E,F,G,H,I)], preArg: String = ""): Unit = {
      val (a,b,c,d,e,f,g,h,i) = t.get
      fn9(name,a,b,c,d,e,f,g,h,i,preArg)
    }
    def cc10[A: ShowSrc, B: ShowSrc, C: ShowSrc, D: ShowSrc, E: ShowSrc, F: ShowSrc, G: ShowSrc, H: ShowSrc, I: ShowSrc, J: ShowSrc](name: String, t: Option[(A,B,C,D,E,F,G,H,I,J)], preArg: String = ""): Unit = {
      val (a,b,c,d,e,f,g,h,i,j) = t.get
      fn10(name,a,b,c,d,e,f,g,h,i,j,preArg)
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
