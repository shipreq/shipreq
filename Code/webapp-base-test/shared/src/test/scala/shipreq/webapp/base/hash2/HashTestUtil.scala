package shipreq.webapp.base.hash2

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.univeq._
import nyaya.gen._
import scalaz.syntax.applicative._
import shipreq.base.test.BaseUtilGen._

object HashTestUtil {
  import EvoHashModule.SchemeId

  final case class XorAlgorithm(a: Hash.Algorithm, xor: Int) extends Hash.Algorithm {
    private def modI(i: Int): Int =
      i ^ xor

    private def modH[A](h: HashFn[A]): HashFn[A] =
      new HashFn(a => modI(h hashFn a))

    override implicit val hashBoolean                   = modH(a.hashBoolean)
    override implicit val hashChar                      = modH(a.hashChar)
    override implicit val hashLong                      = modH(a.hashLong)
    override implicit val hashString                    = modH(a.hashString)
    override implicit val hashInt                       = modH(a.hashInt)
    override implicit def hashMap[K: HashFn, V: HashFn] = modH(a.hashMap)
    override implicit def hashSet[A: HashFn]            = modH(a.hashSet)
    override implicit def hashList[A: HashFn]           = modH(a.hashList)
    override implicit def hashVector[A: HashFn]         = modH(a.hashVector)

    override protected def _hashPair[
      @specialized(Int, Long, Char, Boolean) A: HashFn,
      @specialized(Int, Long, Char, Boolean) B: HashFn]: HashFn[(A, B)] =
      modH(a.hashPair)

    override def hashUnordered[T[x] <: TraversableOnce[x], A: HashFn] = modH(a.hashUnordered)
    override def joinHashes(hashes: List[Int]) = modI(a.joinHashes(hashes))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class RandomHashData[S: UnivEq, D](allScopes: NonEmptyVector[S]) {
    val types = new EvoHashModule.Types[S, D]
    import types._

    val dudVersionedHashFn: VersionedHashFn =
      EvoHashModule.VersionedHashFn.init(HashFn const 0)

    val genScope: Gen[Scope] =
      Gen.chooseNE(allScopes)

    val genScopes: Gen[NonEmptyVector[Scope]] =
      Gen.subset1(allScopes.whole).map(NonEmptyVector.force)

    def genScopeTo[A](scheme: Scheme, g: Gen[A]): Gen[ScopeMap[A]] =
      Gen.subset(scheme.hashFns.keySet).flatMap(genScopeTo(_, g))

    def genScopeTo[A](scopes: Set[Scope], g: Gen[A]): Gen[ScopeMap[A]] =
      Gen.traverse(scopes.toList)(s => g.map((s, _))).map(_.toMap)

    val genScheme: Gen[SchemeId => Scheme] =
      for {
        scopes <- genScopes
        hashFns <- genScopeTo(scopes.whole.toSet, Gen pure dudVersionedHashFn)
      } yield EvoHashModule.Scheme.withoutId(hashFns)

    val genEvolutionOps: StateGen[Schemes, NonEmptyVector[EvolutionOp]] =
      StateGen.genA(hs =>
        genScopes.flatMap(scopes =>
          Gen.traverse(scopes.whole)(s =>
            if (hs.latest.hashFns.contains(s))
              Gen.choose(
                EvolutionOp.Evolve(s -> HashFn.const[Data](0)),
                EvolutionOp.Drop(s))
            else
              Gen.pure(
                EvolutionOp.Add(s -> HashFn.const[Data](0))))
            .map(NonEmptyVector.force)))

    val genEvolve: StateGen[Schemes, Unit] =
      genEvolutionOps.flatMap(ops =>
        StateGen.mod(_.addEvolution(ops.head, ops.tail: _*)))

    val genSchemes: Gen[Schemes] =
      for {
        init <- genScheme
        evos <- Gen.chooseInt(4)
        hs   <- genEvolve.replicateM_(evos).exec(EvoHashModule.Schemes.one(init))
      } yield hs

    val genHash: Gen[Option[Int]] = {
      val g = Gen.int.map(Some(_))
      Gen.chooseGen(Gen.int.option, List.fill(3)(g): _*)
    }

    def genRecsByScheme(scheme: Scheme): Gen[(Scheme, ScopeMap[Option[Int]])] =
      genScopeTo(scheme, genHash).map((scheme, _))

    def genHashRecs(schemes: Schemes): Gen[HashRecs] =
      for {
        ss <- Gen.chooseGen(Gen.subset(schemes.schemes.whole).map(_.toList), Gen.pure(schemes.latest :: Nil))
        rs <- Gen.traverse(ss)(genRecsByScheme)
      } yield rs.toMap
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class FakeData(a: Int, b: Int, c: Int, d: Int)

  sealed trait FakeScope
  object FakeScope {
    case object A extends FakeScope
    case object B extends FakeScope
    case object C extends FakeScope
    case object D extends FakeScope
    implicit def univEqFakeScope: UnivEq[FakeScope] = UnivEq.derive
  }

  object FakeModule extends EvoHashModule[FakeScope, FakeData] {
    private val h1 = InternalHash.hashInt
    private val h2 = XorAlgorithm(InternalHash, 1).hashInt

    // A B C1 -
    // - | C2 D
    override val schemeRegistry =
      initSchemes(
        FakeScope.A -> h1.contramap[FakeData](_.a),
        FakeScope.B -> h1.contramap[FakeData](_.b),
        FakeScope.C -> h1.contramap[FakeData](_.c))
      .addEvolution(
        EvolutionOp.Drop  (FakeScope.A),
        EvolutionOp.Evolve(FakeScope.C -> h2.contramap[FakeData](_.c)),
        EvolutionOp.Add   (FakeScope.D -> h2.contramap[FakeData](_.d)))

    val batcher: HashLogic.Batcher[Scope, Data, (HashRecs, Int), Int] =
      HashLogic.Batcher(_._2, _._1, schemeRegistry)
  }
}
