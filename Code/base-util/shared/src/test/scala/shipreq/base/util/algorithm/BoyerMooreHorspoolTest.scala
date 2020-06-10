package shipreq.base.util.algorithm

object BoyerMooreHorspoolTest extends TextSearchTest(
  new BoyerMooreHorspool(_).search
)
