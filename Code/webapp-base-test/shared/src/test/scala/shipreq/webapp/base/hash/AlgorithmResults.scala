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
      booleanT      = a.hashBoolean                      apply true,
      booleanF      = a.hashBoolean                      apply false,
      int           = a.hashInt                          apply 1000,
      long          = a.hashLong                         apply 3000000000L,
      string        = a.hashString                       apply "stringy",
      pair          = a.hashPair      [Int, String]      apply ((3, "omg")),
      map           = a.hashMap       [Int, String]      apply Map(3 -> "yay", 4 -> "no"),
      set           = a.hashSet       [String]           apply Set("more", "on"),
      list          = a.hashList      [String]           apply List("\u2048", "hehe\nno!"),
      vector        = a.hashVector    [String]           apply Vector("\u00f2", "\n? w"),
      hashUnordered = a.hashUnordered [Iterable, String] apply Map(3 -> "yay", 4 -> "no").values,
      joinHashes    = a joinHashes                             List(1,2,4))
  }
}
