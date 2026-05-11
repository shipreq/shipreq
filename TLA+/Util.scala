import scala.Console
import tlc2.value.impl._

final class UtilScala {
  import UtilScala._

  var normaliseNatsTotal = 0
  var normaliseNatsReduced = 0

  val fieldFilterCache = collection.mutable.Map.empty[String, String => Boolean]

  // TODO: Test with and without thread locals
  def normaliseNats(value    : Value,
                    maxGap   : Int,
                    setSize  : Int,
                    fieldsStr: String): Value = {

    val fieldFilterOrNull: String => Boolean =
      if (fieldsStr.isEmpty)
        null
      else
        fieldFilterCache.getOrElseUpdate(fieldsStr, {
          val fields = fieldsStr.stripPrefix("-").split(',').iterator.map(_.trim).filter(_.nonEmpty).toSet
          if (fieldsStr.startsWith("-"))
            !fields.contains(_)
          else
            fields.contains
        })

    val normaliser = new IntNormaliser(setSize)
    foreachInt(fieldFilterOrNull, normaliser.add)(value)
    val reduce = normaliser.reduction(maxGap)

    if (LogNormaliseIntStats) {
      normaliseNatsTotal += 1
      val reduced = reduce != null
      if (reduced) normaliseNatsReduced += 1
      printf(
        "[NormaliseNats] Reduced %d/%d (%.2f%%) -- %s%s\n",
        normaliseNatsReduced, normaliseNatsTotal,
        normaliseNatsReduced.toDouble / normaliseNatsTotal * 100,
        if (reduced) "+" else "-",
        normaliser.toList.mkString("[", ",", "]")
      )
    }

    if (reduce == null)
      value
    else {
      val result = mapInt(fieldFilterOrNull, reduce)(value)
      if (LogChanges)
        mutexPrintln {
          s"""|${"=" * 160}
              |BEFORE: $value
              |AFTER : $result
              |""".stripMargin.trim
        }
      result
    }
  }
}

object UtilScala {

  // final class Ref[A <: AnyRef](val value: A) {
  //   override val hashCode = value.hashCode
  //   override def equals(x: Any) = x match {
  //     case y: AnyRef => value eq y
  //     case _         => false
  //   }
  // }

  final val LogNormaliseIntStats = false
  final val LogChanges           = false

  def fail(msg: String): Nothing =
    throw new RuntimeException(Console.WHITE + Console.MAGENTA_B + msg + Console.RESET)

  private def cantHandle(a: Any): Nothing = {
    val msg =
      s"""|Don't know how to handle ${a.getClass.getSimpleName}:
          |$a
          |""".stripMargin.trim
    fail(msg)
  }

  private def assertEq[A](name: String, actual: A, expect: A): Unit =
    if (actual != expect)
      fail(s"$name failed. $actual != $expect")

  def mutexPrintln(msg: String): Unit =
    synchronized {
      val s = System.out
      s.flush()
      s.println(msg)
      s.flush()
    }

  // ===================================================================================================================
  def foreachInt(fieldFilterOrNull: String => Boolean, onInt: Int => Unit): Value => Unit = {
    var run: Value => Unit = null

    def valueVec(vs: ValueVec): Unit = {
      var i = vs.size
      while (i > 0) {
        i -= 1
        val v = vs.elementAt(i)
        run(v)
      }
    }

    run = {
      case a: RecordValue =>
        if (null == fieldFilterOrNull)
          a.values.foreach(run)
        else {
          var i = a.names.length
          while (i > 0) {
            i -= 1
            val name = a.names(i).toString
            if (fieldFilterOrNull(name))
              run(a.values(i))
          }
        }

      case a: FcnLambdaValue => run(a.toFcnRcd)
      case a: FcnRcdValue    => a.values.foreach(run)
      case a: SetEnumValue   => valueVec(a.elems)
      case a: TupleValue     => a.elems.foreach(run)
      case a: IntValue       => onInt(a.`val`)

      case _: BoolValue
         | _: ModelValue
         | _: StringValue =>
        ()

      case a =>
        cantHandle(a)
    }

    run
  }

  // ===================================================================================================================
  def mapInt(fieldFilterOrNull: String => Boolean, f: Int => Int): Value => Value = {
    var run: Value => Value = null

    def valueVec(vs: ValueVec): ValueVec = {
      var i = vs.size
      if (i == 0)
        vs
      else {
        val elems = new Array[Value](i)
        while (i > 0) {
          i -= 1
          val v = vs.elementAt(i)
          elems(i) = run(v)
        }
        new ValueVec(elems)
      }
    }

    run = value => value match {
      case a: RecordValue =>
        val newValues =
          if (null == fieldFilterOrNull)
            a.values.map(run)
          else {
            var i = a.names.length
            val vs = new Array[Value](i)
            while (i > 0) {
              i -= 1
              val name = a.names(i).toString
              var value = a.values(i)
              if (fieldFilterOrNull(name))
                value = run(value)
              vs(i) = value
            }
            vs
          }
        new RecordValue(a.names, newValues, a.isNormalized())

      case a: FcnRcdValue =>
        val newValues = a.values.map(run)
        new FcnRcdValue(a.domain, newValues, a.isNormalized())

      case a: FcnLambdaValue =>
        run(a.toFcnRcd)

      case a: SetEnumValue => new SetEnumValue(valueVec(a.elems), a.isNormalized())
      case a: TupleValue   => new TupleValue(a.elems.map(run))
      case a: IntValue     => IntValue.gen(f(a.`val`))

      case _: BoolValue
         | _: ModelValue
         | _: StringValue =>
        value

      case a =>
        cantHandle(a)
    }

    run
  }

  // ===================================================================================================================

  final class IntNormaliser(size: Int) {
    private[this] val set = new Array[Boolean](size)

    def add(n: Int): Unit =
      if (n < size) {
        if (!set(n))
          set(n) = true
      } else
        fail(s"IntNormaliser($size).add($n)")

    def reduction(maxGap: Int): Int => Int = {
      var i = 1 // skipping zero is a special case (eg. with maxGap=1, {0,2} should remain as {0,2})
      var prevA = 0 // previous value that exists (before translation)
      var prevB = 0 // previous value that exists (after translation)
      var maps: Array[Int] = null

      while (i < size) {
        // println(s"i=$i, e=${set(i)}, prev=$prevA|$prevB, maps=${Option(maps).map(_.mkString(",")).orNull}")

        if (set(i)) {
          @inline def delta = i - prevA
          @inline def gap   = delta - 1
          if (gap > maxGap) {
            // println("  !")
            if (maps == null) {
              maps = new Array(size)
              var j = prevA
              while (j >= 0) {
                maps(j) = j
                j -= 1
              }
            }
            prevB = prevB + maxGap + 1
            maps(i) = prevB
          } else {
            if (maps == null) {
              prevB = i
            } else {
              prevB += delta
              maps(i) = prevB
            }
          }

          prevA = i

        }

        i += 1
      }

      if (maps == null)
        null
      else
        maps.apply
    }

    // For debugging
    def toList: List[Int] =
      set.iterator.zipWithIndex.filter(_._1).map(_._2).toList
  }

  private def testReduction(maxGap: Int)(inputs: Int*)(expect: Int*): Unit = {
    val name = s"testReduction($maxGap)(${inputs.mkString(",")})"
    // println("==============================================================================")
    // println(name)
    val s = new IntNormaliser(12)
    inputs.foreach(s.add)
    val r = Option(s.reduction(maxGap))
    assertEq(s"$name.reducable", r.isDefined, expect.nonEmpty)
    for (f <- r) {
      val mapped = inputs.map(f)
      assertEq(s"$name.mapped", mapped, expect)
    }
  }

  testReduction(0)()()
  testReduction(0)(0)()
  testReduction(0)(0, 1)()
  testReduction(0)(0, 1, 2)()
  testReduction(0)(0, 1, 2, 3)()
  testReduction(0)(0, 1, 3)(0, 1, 2)
  testReduction(0)(0, 2, 3)(0, 1, 2)
  testReduction(0)(0, 2)(0, 1)
  testReduction(0)(0, 3)(0, 1)
  testReduction(0)(0, 3, 4)(0, 1, 2)
  testReduction(0)(0, 3, 5)(0, 1, 2)
  testReduction(0)(0, 3, 6)(0, 1, 2)
  testReduction(0)(1)()
  testReduction(0)(1, 2)()
  testReduction(0)(1, 2, 3)()
  testReduction(0)(1, 3)(1, 2)
  testReduction(0)(2)(1)
  testReduction(0)(2, 4)(1, 2)
  testReduction(0)(3, 5)(1, 2)
  testReduction(0)(0, 1, 5, 8, 9)(0, 1, 2, 3, 4)

  testReduction(1)()()
  testReduction(1)(0)()
  testReduction(1)(1)()
  testReduction(1)(0, 1)()
  testReduction(1)(0, 1, 2)()
  testReduction(1)(0, 1, 3)()
  testReduction(1)(0, 1, 4)(0, 1, 3)
  testReduction(1)(0, 2, 3)()
  testReduction(1)(0, 2, 4)()
  testReduction(1)(0, 2, 5)(0, 2, 4)
  testReduction(1)(0, 2, 6, 9)(0, 2, 4, 6)
  testReduction(1)(0, 3, 6, 9)(0, 2, 4, 6)
  testReduction(1)(0, 1, 2, 6, 9)(0, 1, 2, 4, 6)

}