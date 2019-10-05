package shipreq.base.test

import japgolly.microlibs.nonempty._
import java.util.concurrent.atomic.AtomicInteger
import nyaya.gen._
import nyaya.prop.CycleDetector
import nyaya.util.Multimap
import scala.annotation.tailrec
import scala.collection.AbstractIterator
import shipreq.base.util._
import shipreq.base.util.univeq._
import SizeSpec.DisableDefault._

object BaseUtilGen {

  implicit def NevToNonEmptySeq[A] = Gen.ToNonEmptySeq[NonEmptyVector[A], A](_.whole)
  implicit def NesToNonEmptySeq[A] = Gen.ToNonEmptySeq[NonEmptySet   [A], A](_.whole.toVector)

  implicit def BaseUtilGen_GenExt[A](g: Gen[A]) = new BaseUtilGen_GenExt(g.run)
  class BaseUtilGen_GenExt[A](private val _g: Gen.Run[A]) extends AnyVal {
    private implicit def g = Gen(_g)

    def nev(implicit ss: SizeSpec): Gen[NonEmptyVector[A]] = {
      val single = g map NonEmptyVector.one
      g.vector(ss).flatMap(vs =>
        NonEmptyVector.maybe(vs, single)(Gen.pure))
    }

    def nes(implicit ss: SizeSpec, ev: UnivEq[A]): Gen[NonEmptySet[A]] = {
      val single = g map (NonEmptySet one _)
      g.set(ss).flatMap(vs =>
        NonEmptySet.maybe(vs, single)(Gen.pure))
    }

    def unique_!(implicit ev: UnivEq[A]): Gen[A] =
      liftIterator_!(uniqueIterator)

    def uniqueIterator(implicit ev: UnivEq[A]): Gen[Iterator[A]] = {
      val MaxTries = 10000
      Gen { ctx =>
        new AbstractIterator[A] {
          val seen = UnivEq.emptyMutableSet[A]

          @tailrec
          def nextUnique(rem: Int): A = {
            val a = g.run(ctx)
            if (seen contains a) {
              if (rem == 1)
                sys.error(s"Failed to generate a unique value after $MaxTries tries. Last = $a")
              else
                nextUnique(rem - 1)
            } else {
              seen += a
              a
            }
          }

          override def hasNext = true
          override def next() = nextUnique(MaxTries)
        }
      }
    }
  }

  implicit class BaseUtilGen_OptionGenExt[A](private val o: Option[Gen[A]]) extends AnyVal {
    def setE(implicit ss: SizeSpec): Gen[Set[A]] =
      o.fold(Gen pure Set.empty[A])(_.set(ss))
  }

  def liftIterator_![A](g: Gen[Iterator[A]]): Gen[A] = {
    var it: Iterator[A] = null
    Gen { ctx =>
      if (it eq null)
        it = g.run(ctx)
      it.next()
    }
  }

  def counter(start: Int = 1): Gen[Int] = {
    val i = new AtomicInteger(start)
    Gen { _ =>
      i.getAndIncrement()
    }
  }

  // ===================================================================================================================

  def genISubset[A: UnivEq](g: Gen[NonEmptySet[A]]): Gen[ISubset[A]] =
    Gen.chooseGen(
      Gen pure ISubset.All(),
      g map ISubset.Only.apply,
      g map ISubset.Not.apply)

  def genMTrie[K: UnivEq, V](genK: Gen[K], genV: Gen[V], maxDepth: Int)(implicit ss: SizeSpec): Gen[MTrie.Trie[K, V]] = {
    val valueN   = genV map MTrie.Value[K, V]
    val valueO   = valueN.option
    val midDepth = maxDepth >> 1

    def level(depth: Int): Gen[MTrie.Trie[K, V]] =
      if (depth <= 1)
        Gen pure Map.empty
      else {
        val branch  = Gen.apply2(MTrie.Branch[K, V])(valueO, level(depth - 1))
        val branchN = branch.flatMap[MTrie.Node[K, V]](b => if (b.next.nonEmpty) Gen.pure(b) else b.value.fold(valueN)(Gen.pure))
        val node    = Gen.chooseGen(branchN, valueN, if (depth > midDepth) branchN else valueN)
        node.mapBy(genK)(ss)
      }

    level(maxDepth)
  }

  def genNonEmptySetDiff[A: UnivEq](g: Gen[A])(implicit ss: SizeSpec): Gen[SetDiff.NE[A]] = {
    val set = g.set(ss)
    val attempt =
      for {
        a <- set
        b <- set
      } yield SetDiff(a, b &~ a)
    attempt.flatMap(d =>
      NonEmpty(d) match {
        case Some(ne) => Gen pure ne
        case None     => g.map(a => NonEmpty.force(SetDiff(Set.empty[A], Set(a))))
      }
    )
  }

  private def genDigraphUniMap[A](ga: Gen[A])(implicit ss: SizeSpec) =
    ga.set1(ss).mapBy(ga)(ss)

  def genDigraphUni[A: UnivEq](ga: Gen[A])(implicit ss: SizeSpec): Gen[Digraph.UniDir[A]] =
    genDigraphUniMap(ga)(ss).map(Multimap(_))

  def genDigraphBi[A: UnivEq](ga: Gen[A])(implicit ss: SizeSpec): Gen[Digraph.BiDir[A]] =
    genDigraphUni(ga)(implicitly, ss).map(Digraph.BiDir(_))

  def genDagUni[A: UnivEq](fix: Digraph.FixAcyclic[A, _])(ga: Gen[A])(implicit ss: SizeSpec): Gen[Digraph.UniDir[A]] =
    genDigraphUniMap(ga)(ss).map(m => Multimap(preventCycles(fix.cycleDetector)(m)))

  def genDagBi[A: UnivEq](fix: Digraph.FixAcyclic[A, _])(ga: Gen[A])(implicit ss: SizeSpec): Gen[Digraph.BiDir[A]] =
    genDagUni(fix)(ga)(implicitly, ss).map(Digraph.BiDir(_))

  def genDigraphUniO[A: UnivEq](og: Option[Gen[A]])(implicit ss: SizeSpec): Gen[Digraph.UniDir[A]] =
    og.fold(Gen pure Digraph.emptyUniDir)(genDigraphUni(_)(implicitly, ss))

  def genDigraphBiO[A: UnivEq](og: Option[Gen[A]])(implicit ss: SizeSpec): Gen[Digraph.BiDir[A]] =
    og.fold(Gen pure Digraph.emptyBiDir)(genDigraphBi(_)(implicitly, ss))

  def genDagUniO[A: UnivEq](fix: Digraph.FixAcyclic[A, _])(og: Option[Gen[A]])(implicit ss: SizeSpec): Gen[Digraph.UniDir[A]] =
    og.fold(Gen pure Digraph.emptyUniDir)(genDagUni(fix)(_)(implicitly, ss))

  def genDagBiO[A: UnivEq](fix: Digraph.FixAcyclic[A, _])(og: Option[Gen[A]])(implicit ss: SizeSpec): Gen[Digraph.BiDir[A]] =
    og.fold(Gen pure Digraph.emptyBiDir)(genDagBi(fix)(_)(implicitly, ss))

  def preventCycles[A, B](cd: CycleDetector[Map[A, B], A])(m: Map[A, B]): Map[A, B] = {
    @tailrec
    def go(m: Map[A, B] /*, i: Int = 0*/): Map[A, B] =
      cd.findCycle(m) match {
        case None =>
          // println(s"No cycles after $i attempts @ size ${m.keyCount}→${m.valueCount}")
          m
        case Some((_, b)) =>
          // println(s"Found cycle #$i [$a→$b] in ${m.m}")
          // preventCycles(m.del(a, b).del(b, a), i + 1) // better but slowwwwww
          go(m - b /*, i + 1*/)
      }
    go(m)
  }

  // -------------------------------------------------------------------------------------------------------------------
  import VectorTree._

  def genVectorTree[A](genValue: Gen[A], maxDepth: Int)(implicit ss: SizeSpec): Gen[VectorTree[A]] = {
    def children(rem: Int): Gen[Children[A]] =
      if (rem == 0)
        Gen pure noChildren
      else
        Gen.lift2(genValue, children(rem - 1))(Node.apply).vector(ss)
    children(maxDepth).map(VectorTree(_))
  }

  def genVectorTreeLoc(implicit ss: SizeSpec): Gen[Location] =
    Gen.int.vector1(ss).map(NonEmptyVector.force)

  def genVectorTreeParLoc(implicit ss: SizeSpec): Gen[ParentLocation] =
    Gen.int.vector(ss).map(ParentLocation.fromVector)

  implicit class VectorTreeGenExt[A](private val tree: VectorTree[A]) extends AnyVal {
    def genLocation: Option[Gen[Location]] =
      Gen tryGenChoose tree.locIterator

    def genParentLocation: Gen[ParentLocation] =
      Gen chooseNE NonEmptyVector[ParentLocation](ParentLocation.Empty, tree.locIterator.map(_.asParentLoc).toVector)
  }
}
