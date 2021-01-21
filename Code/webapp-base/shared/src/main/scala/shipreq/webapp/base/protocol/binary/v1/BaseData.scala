package shipreq.webapp.base.protocol.binary.v1

import boopickle.Decoder
import boopickle.DefaultBasic._
import japgolly.microlibs.nonempty.NonEmpty
import japgolly.microlibs.recursion.Fix
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.nio.ByteBuffer
import java.time.Instant
import monocle.Iso
import nyaya.util.{MultiValues, Multimap}
import scala.reflect.ClassTag
import scalaz.Isomorphism.<=>
import scalaz.{Functor, \&/}
import shipreq.base.util._
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.Version
import shipreq.webapp.base.protocol.binary._
import shipreq.webapp.base.util._

object BaseData {

  /** Used to add a codec version to a binary protocol whilst retaining backwards-compatibility with the unversioned
    * case.
    */
  final val VersionHeader = -99988999

  def writeVersion(ver: Int)(implicit state: PickleState): Unit = {
    assert(ver > 0) // v1.0 is the default and doesn't need a version header
    state.enc.writeInt(VersionHeader)
    state.enc.writeInt(ver)
  }

  def unsupportedVer(ver: Int, maxSupportedVer: Int): Nothing =
    throw UnsupportedVersionException(found = Version.v1(ver), maxSupported = Version.v1(maxSupportedVer))

  def readByVersion[A](maxSupportedVer: Int)(f: PartialFunction[Int, A])(implicit state: UnpickleState): A = {
    assert(maxSupportedVer > 0)

    def unsupportedVer(ver: Int): Nothing =
      BaseData.unsupportedVer(ver, maxSupportedVer)

    def readVer(ver: Int): A =
      f.applyOrElse[Int, A](ver, unsupportedVer)

    state.dec.peek(_.readInt) match {
      case VersionHeader =>
        state.dec.readInt
        val ver = state.dec.readInt
        if (ver <= 0)
          throw CorruptData
        if (ver > maxSupportedVer) // preempt using the partial function in case maxSupportedVer is incorrect
          unsupportedVer(ver)
        readVer(ver)
      case _ =>
        readVer(0)
    }
  }

  // ===================================================================================================================
  // Extension classes

  implicit final class AnyRefPicklerExt[A <: AnyRef](private val p: Pickler[A]) extends AnyVal {

    @nowarn("cat=unused")
    def reuseByUnivEq(implicit ev: UnivEq[A]) =
      new PickleWithReuse[A](p, true)

    def reuseByRef =
      new PickleWithReuse[A](p, false)

    def imap[B](iso: Iso[A, B]): Pickler[B] =
      p.xmap(iso.get)(iso.reverseGet)

    def narrow[B <: A: ClassTag]: Pickler[B] =
      p.xmap[B]({
        case b: B => b
        case a    => throw new IllegalArgumentException("Illegal supertype: " + a)
      })(b => b)
  }

  final class PickleWithReuse[A <: AnyRef](p: Pickler[A], byUnivEq: Boolean) extends Pickler[A] {
    private[this] val getP: (PickleState, A) => Option[Int] = if (byUnivEq) _ immutableRefFor _  else _ identityRefFor _
    private[this] val getU: (UnpickleState, Int) => A       = if (byUnivEq) _.immutableFor[A](_) else _.identityFor[A](_)
    private[this] val setP: (PickleState  , A) => Unit      = if (byUnivEq) _ addImmutableRef _  else _ addIdentityRef _
    private[this] val setU: (UnpickleState, A) => Unit      = if (byUnivEq) _ addImmutableRef _  else _ addIdentityRef _

    override def pickle(value: A)(implicit state: PickleState): Unit = {
      val ref = getP(state, value)
      if (ref.isDefined)
        state.enc.writeInt(-ref.get)
      else {
        state.enc.writeInt(0)
        p.pickle(value)
        setP(state, value)
      }
    }
    override def unpickle(implicit state: UnpickleState): A =
      state.dec.readIntCode match {
        case Right(i) =>
          if (i == 0) {
            val value = p.unpickle
            setU(state, value)
            value
          } else
            getU(state, -i)
        case Left(_) =>
          throw new IllegalArgumentException("Unknown coding")
      }
  }

  implicit final class DecoderExt(private val self: Decoder) extends AnyVal {
    def buf: ByteBuffer =
      self match {
        case a: boopickle.DecoderSpeed => a.buf
        case a: boopickle.DecoderSize  => a.buf
      }

    def peek[A](f: Decoder => A): A = {
      val b = buf
      val p = b.position()
      try f(self) finally b.position(p)
    }
  }

  // ===================================================================================================================
  // Polymorphic definitions
  // (non-implicit, "pickle" prefix)

  private object _pickleNothing extends Pickler[AnyRef] {
    override def pickle(obj: AnyRef)(implicit state: PickleState): Unit = ()
    override def unpickle(implicit state: UnpickleState): Nothing = throw new RuntimeException("This case is illegal.")
  }

  def pickleNothing[A <: AnyRef]: Pickler[A] =
    _pickleNothing.asInstanceOf[Pickler[A]]

  def pickleBool[A](iso: Boolean <=> A): Pickler[A] =
    transformPickler(iso.to)(iso.from)

  def pickleDigraphBiDir[A: Pickler: UnivEq]: Pickler[Digraph.BiDir[A]] =
    transformPickler(Digraph.BiDir.apply[A])(_.forwards)(pickleDigraphUniDir)

  def pickleDigraphUniDir[A: Pickler: UnivEq]: Pickler[Digraph.UniDir[A]] =
    pickleMultimap[A, Set, A]

  def pickleDisj[L: Pickler, R: Pickler]: Pickler[L \/ R] =
    new Pickler[L \/ R] {
      private[this] final val KeyR = 0
      private[this] final val KeyL = 1
      override def pickle(a: \/[L, R])(implicit state: PickleState): Unit =
        a match {
          case \/-(r) => state.enc.writeByte(KeyR); state.pickle(r)
          case -\/(l) => state.enc.writeByte(KeyL); state.pickle(l)
        }
      override def unpickle(implicit state: UnpickleState): \/[L, R] =
        state.dec.readByte match {
          case KeyR => \/-(state.unpickle[R])
          case KeyL => -\/(state.unpickle[L])
        }
    }

  def pickleEnum[V: UnivEq](nev: NonEmptyVector[V], firstValue: Int = 0): Pickler[V] =
    new Pickler[V] {
      private[this] val fromInt = nev.whole
      private[this] val toInt   = nev.whole.mapToOrder
      assert(toInt.size == nev.length, s"Duplicates found in $nev")
      override def pickle(v: V)(implicit state: PickleState): Unit = {
        val i = toInt(v) + firstValue
        state.enc.writeInt(i)
      }
      override def unpickle(implicit state: UnpickleState): V =
        state.dec.readIntCode match {
          case Right(i) => fromInt(i - firstValue)
          case Left(_)  => throw new IllegalArgumentException("Invalid coding")
        }
    }

  def pickleFix[F[_] : Functor](implicit p: Pickler[F[Unit]]): Pickler[Fix[F]] =
    new Pickler[Fix[F]] {
      override def pickle(f: Fix[F])(implicit state: PickleState): Unit = {

        // val fUnit = Functor[F].void(f.unfix)
        // p.pickle(fUnit)
        // Functor[F].map(f.unfix)(pickle)

        // Compared to ↑, this ↓ is generally on-par for small trees, and around 30% faster for larger, deeper trees

        val fields = new collection.mutable.ArrayBuffer[Fix[F]](32)
        val fUnit = Functor[F].map(f.unfix) { a =>
          fields += a
          ()
        }
        p.pickle(fUnit)
        fields.foreach(pickle)

        ()
      }

      override def unpickle(implicit state: UnpickleState) = {
        val fUnit = p.unpickle
        Fix(Functor[F].map(fUnit)(_ => unpickle))
      }
    }

  def pickleIMap[K: UnivEq, V: Pickler](empty: IMap[K, V]): Pickler[IMap[K, V]] =
    transformPickler(empty ++ (_: Iterable[V]))(_.values)

  def pickleIor[A: Pickler, B: Pickler]: Pickler[A \&/ B] =
    new Pickler[A \&/ B] {
      import \&/._
      private[this] final val KeyThis = 0
      private[this] final val KeyThat = 1
      private[this] final val KeyBoth = 2
      override def pickle(i: A \&/ B)(implicit state: PickleState): Unit =
        i match {
          case This(a)    => state.enc.writeByte(KeyThis); state.pickle(a)
          case That(b)    => state.enc.writeByte(KeyThat); state.pickle(b)
          case Both(a, b) => state.enc.writeByte(KeyBoth); state.pickle(a); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): A \&/ B =
        state.dec.readByte match {
          case KeyThis => This(state.unpickle[A])
          case KeyThat => That(state.unpickle[B])
          case KeyBoth =>
            val a = state.unpickle[A]
            val b = state.unpickle[B]
            Both(a, b)
        }
    }

  def pickleIsoBoolValues[B <: IsoBool[B], A: Pickler]: Pickler[IsoBool.Values[B, A]] =
    transformPickler[IsoBool.Values[B, A], (A, A)](x => IsoBool.Values(pos = x._1, neg = x._2))(x => (x.pos, x.neg))

  def pickleLazily[A](f: => Pickler[A]): Pickler[A] = {
    lazy val p = f
    new Pickler[A] {
      override def pickle(a: A)(implicit state: PickleState): Unit = p.pickle(a)
      override def unpickle(implicit state: UnpickleState)  : A    = p.unpickle
    }
  }

  def pickleMap[K: Pickler, V: Pickler]: Pickler[Map[K, V]] =
    mapPickler[K, V, Map]

  def pickleMMTreeRelations[I: Pickler]: Pickler[MMTree.Relations[I]] =
    new Pickler[MMTree.Relations[I]] {
      import MMTree._
      private[this] implicit val picklerParents: Pickler[Parents[I]] = pickleMap
      private[this] implicit val picklerChildren: Pickler[Children[I]] = iterablePickler
      override def pickle(a: Relations[I])(implicit state: PickleState): Unit = {
        state.pickle(a.parents)
        state.pickle(a.children)
      }
      override def unpickle(implicit state: UnpickleState): Relations[I] = {
        val parents  = state.unpickle[Parents[I]]
        val children = state.unpickle[Children[I]]
        Relations(parents, children)
      }
    }

  implicitly[Pickler[Vector[Int]]]
  def pickleArraySeq[A](implicit pa: Pickler[A], ct: ClassTag[A]): Pickler[ArraySeq[A]] =
    // Can't use boopickle.BasicPicklers.ArrayPickler here because internally, it uses writeRawInt to write length,
    // where as IterablePickler uses writeInt. We need to be compatible because we're switching out a Vector for an
    // ArraySeq in some impls without affecting the codec.
    boopickle.BasicPicklers.IterablePickler[A, ArraySeq]

  def pickleMultimap[K: UnivEq, L[_], V](implicit p: Pickler[Map[K, L[V]]], l: MultiValues[L]): Pickler[Multimap[K, L, V]] =
    p.xmap(Multimap(_))(_.m) // TODO optimise

  def pickleNES[A: UnivEq](implicit p: Pickler[Set[A]]): Pickler[NonEmptySet[A]] =
    pickleNonEmpty(_.whole)

  def pickleNEV[A](implicit p: Pickler[Vector[A]]): Pickler[NonEmptyVector[A]] =
    pickleNonEmpty(_.whole)

  def pickleNEA[A](implicit p: Pickler[ArraySeq[A]]): Pickler[NonEmptyArraySeq[A]] =
    pickleNonEmpty(_.whole)

  def pickleNonEmpty[N, E](f: N => E)(implicit p: Pickler[E], proof: NonEmpty.Proof[E, N]): Pickler[N] =
    p.xmap(NonEmpty require_! _)(f)

   def pickleNonEmptyMono[A](implicit p: Pickler[A], proof: NonEmpty.ProofMono[A]): Pickler[NonEmpty[A]] =
    pickleNonEmpty(_.value)

  def pickleObfuscated[A]: Pickler[Obfuscated[A]] =
    transformPickler(Obfuscated.apply[A])(_.value)

  def pickleSetDiff[A: UnivEq](implicit rw: Pickler[Set[A]]): Pickler[SetDiff[A]] =
    new Pickler[SetDiff[A]] {
      override def pickle(a: SetDiff[A])(implicit state: PickleState): Unit = {
        state.pickle(a.removed)
        state.pickle(a.added)
      }
      override def unpickle(implicit state: UnpickleState): SetDiff[A] = {
        val removed = state.unpickle[Set[A]]
        val added   = state.unpickle[Set[A]]
        SetDiff(removed, added)
      }
    }

  def pickleTaggedI[A <: TaggedTypes.TaggedInt](apply: Int => A): Pickler[A] =
    transformPickler(apply)(_.value)

  def pickleTaggedL[A <: TaggedTypes.TaggedLong](apply: Long => A): Pickler[A] =
    transformPickler(apply)(_.value)

  def pickleTaggedS[A <: TaggedTypes.TaggedString](apply: String => A): Pickler[A] =
    transformPickler(apply)(_.value)

  def pickleTrie[K: Pickler, V: Pickler]: Pickler[MTrie.Trie[K, V]] = {
    import MTrie.{Branch, Node, Trie, Value}

    implicit val picklerValue: Pickler[Value[K, V]] =
      transformPickler(Value.apply[K, V])(_.value)

    implicit val picklerOptionValue = optionPickler(picklerValue)

    implicit lazy val branch: Pickler[Branch[K, V]] =
      new Pickler[Branch[K, V]] {
        override def pickle(a: Branch[K, V])(implicit state: PickleState): Unit = {
          state.pickle(a.value)
          state.pickle(a.next)
        }
        override def unpickle(implicit state: UnpickleState): Branch[K, V] = {
          val value = state.unpickle[Option[Value[K, V]]]
          val next  = state.unpickle[Trie[K, V]]
          Branch(value, next)
        }
      }

    implicit lazy val node: Pickler[Node[K, V]] =
      new Pickler[Node[K, V]] {
        private[this] final val KeyBranch = 'b'
        private[this] final val KeyValue  = 'v'
        override def pickle(a: Node[K, V])(implicit state: PickleState): Unit =
          a match {
            case b: Branch[K, V] => state.enc.writeByte(KeyBranch); state.pickle(b)
            case b: Value [K, V] => state.enc.writeByte(KeyValue ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Node[K, V] =
          state.dec.readByte match {
            case KeyBranch => state.unpickle[Branch[K, V]]
            case KeyValue  => state.unpickle[Value [K, V]]
          }
      }

    implicit lazy val trie: Pickler[Trie[K, V]] = pickleLazily(pickleMap)

    trie
  }

  def pickleVectorTree[A: Pickler]: Pickler[VectorTree[A]] = {
    import VectorTree._
    object N extends Pickler[Node[A]] {
      val ch = iterablePickler[Node[A], Vector](this, implicitly)
      override def pickle(node: Node[A])(implicit state: PickleState): Unit = {
        state.pickle(node.value)
        state.pickle(node.children)(ch)
      }
      override def unpickle(implicit state: UnpickleState): Node[A] = {
        val a = state.unpickle[A]
        val c = state.unpickle(ch)
        Node.apply(a, c)
      }
    }
    N.ch.xmap(VectorTree.apply)(_.children)
  }

  // ===================================================================================================================
  // Concrete picklers for base data type
  // (implicit lazy vals, "pickler" prefix)

  implicit lazy val picklerImpossible: Pickler[Impossible] =
    pickleNothing

  implicit lazy val picklerDirection: Pickler[Direction] =
    pickleBool(Forwards)

  implicit lazy val picklerEmailAddr: Pickler[EmailAddr] =
    transformPickler(EmailAddr.apply)(_.value)

  implicit lazy val picklerErrorMsg: Pickler[ErrorMsg] =
    transformPickler(ErrorMsg.apply)(_.value)

  implicit lazy val picklerErrorMsgOrUnit: Pickler[ErrorMsg \/ Unit] =
    pickleDisj

  implicit lazy val picklerInstant: Pickler[Instant] =
    new Pickler[Instant] {
      // EpochSecond is stored as a packed long (typically 5 bytes instead of 8 raw)
      // Nano is stored as a raw int (4 bytes instead of typically 5 packed, P(27%) 4 packed)
      override def pickle(i: Instant)(implicit state: PickleState): Unit = {
        state.enc.writeLong(i.getEpochSecond)
        state.enc.writeRawInt(i.getNano)
      }
      override def unpickle(implicit state: UnpickleState): Instant = {
        val epochSecond = state.dec.readLong
        val nano        = state.dec.readRawInt
        Instant.ofEpochSecond(epochSecond, nano)
      }
    }

  implicit lazy val picklerNonEmptyVectorInt: Pickler[NonEmptyVector[Int]] =
    pickleNEV

  implicit lazy val picklerPermission: Pickler[Permission] =
    pickleBool(Allow)

  implicit lazy val picklerPersonName: Pickler[PersonName] =
    transformPickler(PersonName.apply)(_.value)

  implicit lazy val picklerPlainTextPassword: Pickler[PlainTextPassword] =
    transformPickler(PlainTextPassword.apply)(_.value)

  implicit lazy val picklerProjectIdPublic: Pickler[ProjectId.Public] =
    pickleObfuscated

  implicit lazy val picklerVerificationToken: Pickler[VerificationToken] =
    transformPickler(VerificationToken.apply)(_.value)

  implicit lazy val picklerVerificationTokenStatus: Pickler[VerificationToken.Status] =
    new Pickler[VerificationToken.Status] {
      override def pickle(a: VerificationToken.Status)(implicit state: PickleState): Unit =
        a match {
          case VerificationToken.Status.Expired => state.enc.writeByte(0)
          case VerificationToken.Status.Invalid => state.enc.writeByte(1)
          case VerificationToken.Status.Valid   => state.enc.writeByte(2)
        }
      override def unpickle(implicit state: UnpickleState): VerificationToken.Status =
        state.dec.readByte match {
          case 0 => VerificationToken.Status.Expired
          case 1 => VerificationToken.Status.Invalid
          case 2 => VerificationToken.Status.Valid
        }
    }

  implicit lazy val picklerUsername: Pickler[Username] =
    transformPickler(Username.apply)(_.value)

  implicit lazy val picklerUsernameOrEmailAddr: Pickler[Username \/ EmailAddr] =
    pickleDisj

  implicit lazy val picklerValidity: Pickler[Validity] =
    pickleBool(Valid)

  implicit lazy val picklerVectorTreeParentLocation: Pickler[VectorTree.ParentLocation] =
    new Pickler[VectorTree.ParentLocation] {

      implicit val picklerVectorTreeAt: Pickler[VectorTree.ParentLocation.At] =
        transformPickler(VectorTree.ParentLocation.At.apply)(_.loc)

      private[this] final val KeyAt    = 'a'
      private[this] final val KeyEmpty = 'e'
      override def pickle(a: VectorTree.ParentLocation)(implicit state: PickleState): Unit =
        a match {
          case b: VectorTree.ParentLocation.At    => state.enc.writeByte(KeyAt   ); state.pickle(b)
          case VectorTree.ParentLocation.Empty    => state.enc.writeByte(KeyEmpty)
        }
      override def unpickle(implicit state: UnpickleState): VectorTree.ParentLocation =
        state.dec.readByte match {
          case KeyAt    => state.unpickle[VectorTree.ParentLocation.At]
          case KeyEmpty => VectorTree.ParentLocation.Empty
        }
    }

  implicit lazy val pickleAssetManifestStaticAssetCdn =
    transformPickler(AssetManifest.StaticAssetCdn.apply)(_.value)

  implicit lazy val pickleAssetManifest =
    transformPickler(AssetManifest.apply)(_.staticAssetCdn)
}
