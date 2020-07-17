package shipreq.base.util

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.AsciiTable
import java.time.Duration
object Debug {

  def printStackTrace(filter: String = "shipreq"): Unit = {
    val t = new RuntimeException()
      .stackTraceAsString
      .linesIterator
      .drop(2)
      .filter(_ contains filter)
      .map(t => s"\u001b[96m$t${Console.RESET}")
      .mkString("\n")
    printInIsolation(t, sep = true)
  }

  def printInIsolation(str: String, sep: Boolean = false): Unit =
    synchronized {
      System.out.flush()
      System.err.flush()
      Thread.sleep(10)

      if (sep) {
        val s = "=" * 120
        println(s)
        println(str)
        println(s)
      } else
        println(str)

      System.out.flush()
      System.err.flush()
      Thread.sleep(60)
    }

  trait Implicits {
    final implicit def debugAnyExt[A](a: A): DebugAnyExt[A] =
      new DebugAnyExt(a)
  }

  object Implicits extends Implicits

  final class DebugAnyExt[A](private val self: A) extends AnyVal {
    def tapPrint()                          : A = {println(self); self}
    def tapPrint(title: Any)                : A = {println(s"$title: $self"); self}
    def tapPrintF(f: A => Any)              : A = {println(f(self)); self}
    def printStackTraceWhen(f: A => Boolean): A = {if (f(self)) Debug.printStackTrace(); self}
  }

  private val blank = Console.INVISIBLE + " " + Console.RESET

  // ===================================================================================================================

  def logDuration[A](run: => A): A =
    logDuration(None, run)

  def logDuration[A](name: String, run: => A): A =
    logDuration(Some(name), run)

  def logDuration[A](name: Option[String], run: => A): A = {
    val start = System.nanoTime()
    try
      run
    finally {
      val end = System.nanoTime()
      val dur = Duration.ofNanos(end - start)
      name match {
        case Some(n) => println(s"$n completed in ${dur.conciseDesc}")
        case None    => println(s"Completed in ${dur.conciseDesc}")
      }
    }
  }

  // ===================================================================================================================

  private final class MutInt(init: Int) {
    var value = init
  }

  def CallCounter(): CallCounter =
    new CallCounter

  final class CallCounter {
    private[this] val lock = new AnyRef
    private var stats = Map.empty[String, MutInt]

    def clear(): Unit =
      lock.synchronized {
        stats = Map.empty[String, MutInt]
      }

    def inc(name: String): Unit = {
      val n = name.trim
      lock.synchronized {
        stats.get(n) match {
          case Some(m) => m.value += 1
          case None    => stats = stats.updated(n, new MutInt(1))
        }
      }
    }

    def apply[A](name: String)(a: => A): A = {
      inc(name)
      a
    }

    def report(): String =
      lock.synchronized {
        val rows = MutableArray(stats).sortBy(_._1).iterator().map { case (n, m) => Seq(n, "%,10d".format(m.value))}.toList
        val content = Seq("NAME", "COUNT") :: rows
        AsciiTable(content)
      }

    def printReport(): Unit = {
      printInIsolation(report())
    }

    def results(): Map[String, Int] =
      lock.synchronized(stats.mapValuesNow(_.value))
  }

  // ===================================================================================================================

  def StackTraceCounter(): StackTraceCounter =
    new StackTraceCounter

  final class StackTraceCounter {
    private[this] val cc = CallCounter()

    def clear(): Unit =
      cc.clear()

    def here(): Unit = {
      val stackTrace = new RuntimeException().stackTraceAsString
      cc.inc(stackTrace)
    }

    def report(): String = {
      val results = cc.results()
      val n = 3
      val top = MutableArray(results).sortBy(-_._2).iterator().take(n).map { case (s, c) => s"$c hits -- $s" }.toList
      val c = results.size
      s"""$c stack traces
         |$blank
         |Top ${n.min(c)}:
         |$blank
         |${top.mkString(s"\n$blank\n")}
         |$blank
         |""".stripMargin
    }

    def printReport(): Unit = {
      printInIsolation(report())
    }
  }

  // ===================================================================================================================

  trait ABTest {
    def clear(): Unit
    val addA: String => Unit
    val addB: String => Unit
    def complete(): Unit
  }

  object ABTest {
    object Off extends ABTest {
      override def clear(): Unit = ()
      override val addA = _ => ()
      override val addB = addA
      override def complete(): Unit = ()
    }

    def apply(): ABTest =
      new ABTest {
        private[this] val lock = new AnyRef
        private[this] val a = collection.mutable.ArrayBuffer.empty[String]
        private[this] val b = collection.mutable.ArrayBuffer.empty[String]
        private[this] var lastOk = -1

        override def clear(): Unit =
          lock.synchronized {
            a.clear()
            b.clear()
          }

        override val addA: String => Unit = add(_, true)
        override val addB: String => Unit = add(_, false)

        private def add(s: String, addToA: Boolean): Unit =
          lock.synchronized {
            (if (addToA) a else b) += s
            val limit = a.length.min(b.length)

            // min
            @tailrec def check(i: Int): Unit =
              if (i < limit) {
                val x = a(i)
                val y = b(i)
                if (x != y) {
                  val n = 8
                  val err =
                    new AssertionError(
                      s"""
                         |Discrepancy found at [$i]:
                         |$blank
                         |A: $x
                         |B: $y
                         |$blank
                         |As:
                         |${a.toVector.take(i + n).zipWithIndex.drop(i - n).map {case (s, i) => s"  a[$i] $s"}.mkString("\n")}
                         |$blank
                         |Bs:
                         |${b.toVector.take(i + n).zipWithIndex.drop(i - n).map {case (s, i) => s"  b[$i] $s"}.mkString("\n")}
                         |$blank
                         |""".stripMargin)
                  err.printStackTrace()
                  Thread.sleep(100)
                  throw err
                }
                lastOk = i
                check(i + 1)
              }

            check(lastOk + 1)
          }

        override def complete(): Unit =
          lock.synchronized {
            val x = a.length
            val y = b.length
            if (x != y) {
              val min = x.min(y)
              throw new AssertionError(
                s"""
                   |Discrepancy found! A had $x steps but B had $y steps.
                   |$blank
                   |Extra steps:
                   |${(a.drop(min) ++ b.drop(min)).mkString("\n")}
                   |$blank
                   |""".stripMargin)
            }
          }
      }
  }
}
