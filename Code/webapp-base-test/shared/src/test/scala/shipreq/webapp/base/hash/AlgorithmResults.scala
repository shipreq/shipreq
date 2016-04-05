package shipreq.webapp.base.hash

import japgolly.univeq.UnivEq

case class AlgorithmResults(booleanT     : Int,
                            booleanF     : Int,
                            int          : Int,
                            long         : Int,
                            string       : Int,
                            pair         : Int,
                            map          : Int,
                            set          : Int,
                            list         : Int,
                            vector       : Int,
                            hashUnordered: Int,
                            joinHashes   : Int)

object AlgorithmResults {
  implicit def equality: UnivEq[AlgorithmResults] = UnivEq.derive

  def calc(a: Hash.Algorithm) = {
    import a._
    AlgorithmResults(
      booleanT      = a.hashBoolean                      hash true,
      booleanF      = a.hashBoolean                      hash false,
      int           = a.hashInt                          hash 1000,
      long          = a.hashLong                         hash 3000000000L,
      string        = a.hashString                       hash "stringy",
      pair          = a.hashPair      [Int, String]      hash ((3, "omg")),
      map           = a.hashMap       [Int, String]      hash Map(3 -> "yay", 4 -> "no"),
      set           = a.hashSet       [String]           hash Set("more", "on"),
      list          = a.hashList      [String]           hash List("\u2048", "hehe\nno!"),
      vector        = a.hashVector    [String]           hash Vector("\u00f2", "\n? w"),
      hashUnordered = a.hashUnordered [Iterable, String] hash Map(3 -> "yay", 4 -> "no").values,
      joinHashes    = a joinHashes                            List(1,2,4))
  }
}
