package shipreq.webapp.base.hash

import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.base.util._

object HashTestUtil {

  class XorAlgorithm(a: Hash.Algorithm, xor: Int) extends Hash.Algorithm {
    private def modI(i: Int): Int =
      i ^ xor

    private def modH[A](h: Hash[A]): Hash[A] =
      new Hash(a => modI(h hash a))

    override implicit val hashBoolean                = modH(a.hashBoolean)
    override implicit val hashChar                   = modH(a.hashChar)
    override implicit val hashLong                   = modH(a.hashLong)
    override implicit val hashString                 = modH(a.hashString)
    override implicit val hashInt                    = modH(a.hashInt)
    override implicit def hashMap[K: Hash, V: Hash]  = modH(a.hashMap)
    override implicit def hashSet[A: Hash]           = modH(a.hashSet)
    override implicit def hashList[A: Hash]          = modH(a.hashList)
    override implicit def hashPair[A: Hash, B: Hash] = modH(a.hashPair)
    override implicit def hashVector[A: Hash]        = modH(a.hashVector)

    override def hashUnordered[T[x] <: TraversableOnce[x], A: Hash] = modH(a.hashUnordered)
    override def joinHashes(hashes: List[Int]) = modI(a.joinHashes(hashes))
  }

  def fakeHashSchemeV(xor: Int, id: Char, scopeValidity: HashScope => Validity): HashScheme = {
    val invalidScopes = Option(scopeValidity) match {
      case None    => HashScheme.latest.invalidScopes
      case Some(f) => HashScope.all.iterator.filter(f(_) is Invalid).toSet
    }
    val hasher = new HashScheme.Latest(new XorAlgorithm(MurmurHash3, xor))
    HashScheme(hasher, HashSchemeId(id), invalidScopes)
  }

  def fakeHashSchemeV(id: Char, scopeValidity: HashScope => Validity): HashScheme = fakeHashSchemeV(id.toInt, id, scopeValidity)

  def fakeHashScheme(xor: Int, id: Char): HashScheme = fakeHashSchemeV(xor, id, null)
  def fakeHashScheme(id: Char): HashScheme = fakeHashSchemeV(id, null)

  val hashSchemes: NonEmptyVector[HashScheme] =
    (1 to 3).map(i => fakeHashScheme((32 + i).toChar)).toVector ++: HashScheme.allOldToNew

  // Ensure no duplicate IDs
  assert(hashSchemes.iterator.map(_.id).toSet.size == hashSchemes.length)

}
