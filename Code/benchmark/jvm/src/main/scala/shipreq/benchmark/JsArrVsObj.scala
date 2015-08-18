/*
[info] # Run complete. Total time: 01:20:50
[info]
[info] Benchmark               Mode  Cnt         Score        Error  Units

[info] JsArrVsObj.readA_Obj1  thrpt  200   4268778.110 ±  59336.153  ops/s
[info] JsArrVsObj.readA_Obj2  thrpt  200   4265031.815 ±  60296.856  ops/s
[info] JsArrVsObj.readA_Obj3  thrpt  200   4031309.780 ±  48037.305  ops/s
[info] JsArrVsObj.readA_Arr   thrpt  200   3571579.404 ±  50828.729  ops/s

[info] JsArrVsObj.readB_Obj1  thrpt  200   4108725.947 ±  36445.007  ops/s
[info] JsArrVsObj.readB_Obj2  thrpt  200   4064018.336 ±  46238.409  ops/s
[info] JsArrVsObj.readB_Obj3  thrpt  200   4014742.835 ±  42992.726  ops/s
[info] JsArrVsObj.readB_Arr   thrpt  200   3506751.439 ±  36754.403  ops/s

[info] JsArrVsObj.write_Arr   thrpt  200  11034834.697 ± 264629.610  ops/s
[info] JsArrVsObj.write_Obj3  thrpt  200  10340364.105 ± 146583.223  ops/s
[info] JsArrVsObj.write_Obj1  thrpt  200  10143715.511 ± 146178.443  ops/s
[info] JsArrVsObj.write_Obj2  thrpt  200  10046712.675 ± 146830.890  ops/s

On/A W:  93.7 %
O1/A R: 118.4 %
O2/A R: 117.7 %

________________________________________________________________________________________________________________________
package shipreq.benchmark

import org.openjdk.jmh.annotations._
import upickle._

@State(Scope.Benchmark)
//@OutputTimeUnit(TimeUnit.NANOSECONDS)
class JsArrVsObj {
  
  sealed trait Stuff
  case class StuffA(i: Int) extends Stuff
  case class StuffB(i: Int) extends Stuff

  val rwInt: ReadWriter[Int] =
    ReadWriter(Js.Num(_), { case Js.Num(n) => n.toInt })

  final val A = "A"
  val rwStuffArrA: ReadWriter[StuffA] =
    ReadWriter(
      s => Js.Arr(Js.Str(A), rwInt.write(s.i)),
      { case Js.Arr(Js.Str(A), i) => StuffA(rwInt.read(i)) })
  val rwStuffObj1A: ReadWriter[StuffA] =
    ReadWriter(
    s => Js.Obj(A -> rwInt.write(s.i)),
    { case Js.Obj(kv) if kv._1 == A => StuffA(rwInt.read(kv._2)) })
  val rwStuffObj2A: ReadWriter[StuffA] =
    ReadWriter(
    s => Js.Obj(A -> rwInt.write(s.i)),
    { case Js.Obj((A, v)) => StuffA(rwInt.read(v)) })
  val rwStuffObj3A: ReadWriter[StuffA] =
    ReadWriter(
    s => Js.Obj(A -> rwInt.write(s.i)),
    { case o: Js.Obj if o.value.head._1 == A => StuffA(rwInt.read(o.value.head._2)) })

  final val B = "B"
  val rwStuffArrB: ReadWriter[StuffB] =
    ReadWriter(
    s => Js.Arr(Js.Str(B), rwInt.write(s.i)),
    { case Js.Arr(Js.Str(B), i) => StuffB(rwInt.read(i)) })
  val rwStuffObj1B: ReadWriter[StuffB] =
    ReadWriter(
    s => Js.Obj(B -> rwInt.write(s.i)),
    { case Js.Obj(kv) if kv._1 == B => StuffB(rwInt.read(kv._2)) })
  val rwStuffObj2B: ReadWriter[StuffB] =
    ReadWriter(
    s => Js.Obj(B -> rwInt.write(s.i)),
    { case Js.Obj((B, v)) => StuffB(rwInt.read(v)) })
  val rwStuffObj3B: ReadWriter[StuffB] =
    ReadWriter(
    s => Js.Obj(B -> rwInt.write(s.i)),
    { case o: Js.Obj if o.value.head._1 == B => StuffB(rwInt.read(o.value.head._2)) })

  def merge(a: ReadWriter[StuffA], b: ReadWriter[StuffB]): ReadWriter[Stuff] =
    ReadWriter({
      case s: StuffA => a.write(s)
      case s: StuffB => b.write(s)
    }, a.read orElse b.read)

  val scalaA: Stuff = StuffA(7)
  val scalaB: Stuff = StuffB(7)

  val rwStuffArr = merge(rwStuffArrA, rwStuffArrB)
  val jsonA_Arr: String = Fns.write(scalaA)(rwStuffArr)
  val jsonB_Arr: String = Fns.write(scalaB)(rwStuffArr)
  @Benchmark def write_Arr = Fns.write(scalaA)(rwStuffArr)
  @Benchmark def readA_Arr = Fns.read(jsonA_Arr)(rwStuffArr)
  @Benchmark def readB_Arr = Fns.read(jsonB_Arr)(rwStuffArr)

  val rwStuffObj1 = merge(rwStuffObj1A, rwStuffObj1B)
  val jsonA_Obj1: String = Fns.write(scalaA)(rwStuffObj1)
  val jsonB_Obj1: String = Fns.write(scalaB)(rwStuffObj1)
  @Benchmark def write_Obj1 = Fns.write(scalaA)(rwStuffObj1)
  @Benchmark def readA_Obj1 = Fns.read(jsonA_Obj1)(rwStuffObj1)
  @Benchmark def readB_Obj1 = Fns.read(jsonB_Obj1)(rwStuffObj1)

  val rwStuffObj2 = merge(rwStuffObj2A, rwStuffObj2B)
  val jsonA_Obj2: String = Fns.write(scalaA)(rwStuffObj2)
  val jsonB_Obj2: String = Fns.write(scalaB)(rwStuffObj2)
  @Benchmark def write_Obj2 = Fns.write(scalaA)(rwStuffObj2)
  @Benchmark def readA_Obj2 = Fns.read(jsonA_Obj2)(rwStuffObj2)
  @Benchmark def readB_Obj2 = Fns.read(jsonB_Obj2)(rwStuffObj2)

  val rwStuffObj3 = merge(rwStuffObj3A, rwStuffObj3B)
  val jsonA_Obj3: String = Fns.write(scalaA)(rwStuffObj3)
  val jsonB_Obj3: String = Fns.write(scalaB)(rwStuffObj3)
  @Benchmark def write_Obj3 = Fns.write(scalaA)(rwStuffObj3)
  @Benchmark def readA_Obj3 = Fns.read(jsonA_Obj3)(rwStuffObj3)
  @Benchmark def readB_Obj3 = Fns.read(jsonB_Obj3)(rwStuffObj3)
}
*/