import sbt._

case class Gen1(n: Int, A: String, a: String) {
  val ab = s"$a: $A"
}

class GenN(n: Int, typePrefix: String = "") {
  val ABCs   = (1 to n).map(_ + 64).map(typePrefix + _.toChar)
  val abcs   = ABCs.map(_.toLowerCase)
  val ABC    = ABCs.mkString(",")
  val abc    = abcs.mkString(",")
  val aAbBcC = abcs.map(a => s"$a: ${a.toUpperCase}").mkString(", ")
  def map(t: Gen1 => String) = (1 to n).map(i => Gen1(i, ABCs(i - 1), abcs(i - 1)))
}

object GenBoilerplateForStoreCache {

  implicit class JSLE_String(private val s: String) extends AnyVal {
    def indent(i: String): String =
      if (i.isEmpty)
        s
      else
        i + s.replace("\n", "\n" + i)

    def indent(spaces: Int): String =
      if (spaces <= 0)
        s
      else
        indent(" " * spaces)
  }

  final case class ForN(storeCache : String,
                        scApplyN   : String,
                        logic      : String,
                        logicApplyN: String,
                        logicFnN   : String)

  def apply(outputDir: File): File = {

    val pkg = "shipreq.base.util.storecache"

    val groups =
      (1 to 22).toList.map { n =>
        val sources = new GenN(n, "S")
        val values  = new GenN(n)

        val lDefs = (1 to n).map(i => s"l$i: Logic1[In, ${sources.ABCs(i - 1)}, ${values.ABCs(i - 1)}]").mkString(",\n    ")
        val scValDefs = (1 to n).map(i => s"val s$i: StoreCache1[In, ${sources.ABCs(i - 1)}, ${values.ABCs(i - 1)}]").mkString(",\n    ")
        val scDefs = scValDefs.replaceAll("val ", "")
        val _ls = (1 to n).map(i => s"l$i")
        val _scs = (1 to n).map(i => s"s$i")
        val scs = _scs.mkString(", ")
        val ls = _ls.mkString(", ")
        val vs = (1 to n).map(i => s"s$i.value").mkString(", ")
        val tupleParts = (1 to n).map(i => s"x._$i").mkString(", ")

        val valueTypesWithQE = values.ABCs.map(a => s"$a: QuickEq").mkString(", ")
        val allTypeParams = sources.ABCs.zip(values.ABCs).map {case (s, v) => s"$s,$v"}.mkString(", ")

        def lazyValApply(src: Int => String, yld: Seq[String] => String) = {
          val forArgs = (1 to n).map(i => s"v$i <- ${src(i)}").mkString("; ")
          val yldClause = yld((1 to n).map(i => s"v$i"))
          s"for {$forArgs} yield $yldClause"
        }

        val storeCache =
          s"""
             |private[storecache] final class StoreCache$n[In, $allTypeParams, Z](
             |    $scValDefs,
             |    val lo: LazyVal[Z],
             |    mapOut: (${values.ABC}) => Z) extends StoreCache[In, Z] {
             |
             |  type Self[II, ZZ] = StoreCache$n[II, $allTypeParams, ZZ]
             |
             |  override def value: Z =
             |    lo.value
             |
             |  override def contramap[X](ff: X => In): Self[X, Z] =
             |    new StoreCache$n(${_scs.map(_ + ".contramap(ff)").mkString(", ")}, lo, mapOut)
             |
             |  override def map[X](ff: Z => X): Self[In, X] =
             |    new StoreCache$n($scs, lo.map(ff), (${values.abc}) => ff(mapOut(${values.abc})))
             |}
             |""".stripMargin

        val scApplyN =
          s"""
             |final def apply$n[In, $allTypeParams, Z](
             |    $scDefs)(
             |    mapOut: (${values.ABC}) => Z) = {
             |  val lo = ${lazyValApply(i => s"s$i.lazyVal", _.mkString("mapOut(", ", ", ")"))}
             |  new StoreCache$n($scs, lo, mapOut)
             |}
             |""".stripMargin

        val logic =
          s"""
             |private[storecache] final class Logic$n[In, $allTypeParams, Z](
             |    $lDefs,
             |    mapOut: (${values.ABC}) => Z) extends StoreCache.Logic[In, Z] {
             |
             |  type Self[II, ZZ] = Logic$n[II, $allTypeParams, ZZ]
             |
             |  override type Cache = StoreCache$n[In, $allTypeParams, Z]
             |
             |  override def contramap[X](ff: X => In): Self[X, Z] =
             |    new Logic$n(${_ls.map(_ + ".contramap(ff)").mkString(", ")}, mapOut)
             |
             |  override def map[X](ff: Z => X): Self[In, X] =
             |    new Logic$n($ls, (${values.abc}) => ff(mapOut(${values.abc})))
             |
             |  override def initStrict(i: In): Cache = {
             |${(1 to n).map(i => s"    val s$i = l$i.initStrict(i)").mkString("\n")}
             |    StoreCache.apply$n($scs)(mapOut)
             |  }
             |
             |  override def nextStrict(prev: Cache, i: In): Cache = {
             |${(1 to n).map(i => s"    val x$i = l$i.nextStrict(prev.s$i, i)").mkString("\n")}
             |    if (${(1 to n).map(i => s"(x$i eq prev.s$i)").mkString(" && ")})
             |      prev
             |    else
             |      StoreCache.apply$n(${(1 to n).map(i => s"x$i").mkString(", ")})(mapOut)
             |  }
             |
             |  override def initLazy(i: => In): Cache = {
             |${(1 to n).map(i => s"    val s$i = l$i.initLazy(i)").mkString("\n")}
             |    StoreCache.apply$n($scs)(mapOut)
             |  }
             |
             |  override def nextLazyFull(prev: Cache, i: => In): Next[Cache] = {
             |${(1 to n).map(i => s"    val n$i = l$i.nextLazyFull(prev.s$i, i)").mkString("\n")}
             |${(1 to n).map(i => s"    val s$i = n$i.value").mkString("\n")}
             |    val changed = LazyVal.exists(${(1 to n).map(i => s"n$i.changed").mkString(", ")})(Identity.apply)
             |    val prevLo = prev.lo
             |    val lo = changed.flatMap { isChanged =>
             |      if (isChanged)
             |        LazyVal(mapOut(${(1 to n).map(i => s"s$i.value").mkString(", ")}))
             |      else
             |        prevLo
             |    }
             |    Next(new StoreCache$n(${(1 to n).map(i => s"s$i").mkString(", ")}, lo, mapOut), changed)
             |  }
             |}
             |""".stripMargin

        val logicApplyN =
          s"""
             |final def apply$n[In, $allTypeParams, Z](
             |    $lDefs)(
             |    mapOut: (${values.ABC}) => Z): StoreCache.Logic[In, Z] =
             |  new Logic$n($ls, mapOut)
             |""".stripMargin

        val logicFnN =
          s"""
             |final def fn$n[$valueTypesWithQE, Z](f: (${values.ABC}) => Z): Logic1[(${values.ABC}), (${values.ABC}), Z] =
             |  StoreCache.Logic[(${values.ABC}), Z](x => f($tupleParts))
             |""".stripMargin

        ForN(storeCache, scApplyN, logic, logicApplyN, logicFnN)
      }

    val content =
      s"""
         |package $pkg
         |
         |import shipreq.base.util._
         |
         |${groups.drop(1).map(_.storeCache.trim).mkString("\n\n")}
         |
         |abstract class StoreCacheBoilerplate private[storecache]() {
         |
         |${groups.drop(1).map(_.scApplyN.trim.indent(2)).mkString("\n\n")}
         |}
         |
         |${groups.drop(1).map(_.logic.trim).mkString("\n\n")}
         |
         |abstract class StoreCacheLogicBoilerplate private[storecache]() {
         |
         |${groups.drop(1).map(_.logicApplyN.trim.indent(2)).mkString("\n\n")}
         |
         |${groups.drop(1).map(_.logicFnN.trim.indent(2)).mkString("\n\n")}
         |}
        """.stripMargin.trim

    val file = (outputDir / pkg.replace('.', '/') / "StoreCacheBoilerplate.scala").asFile
    IO.write(file, content)
    println(s"Generated ${file.getAbsolutePath}")
    file
  }
}
