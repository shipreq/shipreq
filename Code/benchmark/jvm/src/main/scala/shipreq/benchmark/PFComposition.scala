/*
[info] PFComposition.orElse1             thrpt   36  174789825.402 ± 1503423.742  ops/s
[info] PFComposition.orElseInline1       thrpt   36  169948449.053 ± 3534914.044  ops/s
[info] PFComposition.lift1               thrpt   36  152079710.394 ± 1084893.679  ops/s
[info] PFComposition.applyOrElseInline1  thrpt   36  127526474.778 ± 2071584.461  ops/s
[info] PFComposition.applyOrElse1        thrpt   36  102426037.904 ± 2281727.587  ops/s
[info] PFComposition.isDefinedAt1        thrpt   36  100617858.792 ± 1328794.236  ops/s
[info] PFComposition.unlift1             thrpt   36   99891235.565 ± 4072235.577  ops/s
[info] PFComposition.liftF1              thrpt   36   62827360.005 ± 1757164.333  ops/s

[info] PFComposition.isDefinedAt2        thrpt   36   68889301.418 ±  751733.123  ops/s
[info] PFComposition.orElseInline2       thrpt   36   60408887.495 ±  745611.677  ops/s
[info] PFComposition.orElse2             thrpt   36   60276059.923 ± 1966441.329  ops/s
[info] PFComposition.applyOrElseInline2  thrpt   36   59909677.483 ±  649334.241  ops/s
[info] PFComposition.lift2               thrpt   36   53761119.867 ±  437599.853  ops/s
[info] PFComposition.applyOrElse2        thrpt   36   43435342.375 ±  893902.488  ops/s
[info] PFComposition.liftF2              thrpt   36   40376984.837 ±  327349.490  ops/s
[info] PFComposition.unlift2             thrpt   36   37073923.229 ± 1268551.875  ops/s


package shipreq.benchmark

import org.openjdk.jmh.annotations._
import upickle._

@State(Scope.Benchmark)
//@OutputTimeUnit(TimeUnit.NANOSECONDS)
class PFComposition {

  def readInt: Reader[Int] =
    Reader { case Js.Num(n) => n.toInt }

  def readObjInt: Reader[Int] = {
    val b = readInt
    Reader { case Js.Obj(x) => val v = x._2; x._1 match { case "i" => b read v } }
  }

  val case1: Js.Value = Js.Num(10)
  val case2: Js.Value = Js.Obj("i" -> Js.Num(20))

  // -------------------------------------------------------------------------------------------------------------------
  // Partial function composition

  val isDefinedAtR: Reader[Int] = {
    val a = readInt
    val b = readInt
    Reader {
      case j if a.read.isDefinedAt(j) => a read j
      case Js.Obj(x) => val v = x._2; x._1 match { case "i" => b read v }
    }
  }

  val orElseR: Reader[Int] =
    Reader(readInt.read orElse readObjInt.read)

  val orElseInlineR: Reader[Int] = {
    val b = readInt
    Reader(readInt.read orElse {
      case Js.Obj(x) => val v = x._2; x._1 match { case "i" => b read v }
    })
  }

  val applyOrElseR: Reader[Int] = {
    val a = readInt
    val b = readObjInt
    Reader { case j => a.read.applyOrElse(j, b.read) }
  }

  val applyOrElseInlineR: Reader[Int] = {
    val a = readInt
    val b = readInt
    Reader {
      case j => a.read.applyOrElse[Js.Value, Int](j, {
        case Js.Obj(x) => val v = x._2; x._1 match { case "i" => b read v }
      })
    }
  }

  val liftR: Reader[Int] = {
    val a = readInt.read.lift
    val b = readObjInt
    Reader { case j => a(j) getOrElse b.read(j) }
  }

  val liftFR: Reader[Int] = {
    val a = readInt.read.lift
    val b = readObjInt
    val f: Js.Value => Int =
      j => a(j) getOrElse b.read(j)
    Reader { case j => f(j) }
  }

  val unliftR: Reader[Int] = {
    val a = readInt.read.lift
    val b = readObjInt.read.lift
    val f: Js.Value => Option[Int] =
      j => a(j) orElse b(j)
    Reader(Function unlift f)
  }

  @Benchmark def isDefinedAt1 = isDefinedAtR read case1
  @Benchmark def isDefinedAt2 = isDefinedAtR read case2

  @Benchmark def orElse1 = orElseR read case1
  @Benchmark def orElse2 = orElseR read case2

  @Benchmark def orElseInline1 = orElseInlineR read case1
  @Benchmark def orElseInline2 = orElseInlineR read case2

  @Benchmark def applyOrElse1 = applyOrElseR read case1
  @Benchmark def applyOrElse2 = applyOrElseR read case2

  @Benchmark def applyOrElseInline1 = applyOrElseInlineR read case1
  @Benchmark def applyOrElseInline2 = applyOrElseInlineR read case2

  @Benchmark def lift1 = liftR read case1
  @Benchmark def lift2 = liftR read case2

  @Benchmark def liftF1 = liftFR read case1
  @Benchmark def liftF2 = liftFR read case2

  @Benchmark def unlift1 = unliftR read case1
  @Benchmark def unlift2 = unliftR read case2
}
*/
