package shipreq.webapp.member.feature.autocomplete.strategies

import scala.collection.View

trait QueryModule {

  final type Query[A] = String => Iterable[A]

  object Query {

    /**
     * Prevents auto-complete when the search term is empty.
     * Prevents showing all options without criteria.
     *
     * Note that you can prevent this in your `match` regex.
     */
    def ignoreEmptyTerm[A](f: Query[A]): Query[A] =
      term =>
        if (term.isEmpty)
          Nil
        else
          f(term)

    /**
     * Prevents auto-complete when the only result just what the user already has typed.
     */
    def ignorePerfectMatch[A](query: Query[A])(perfectMatch: (String, A) => Boolean): Query[A] =
      term => {
        val r = query(term)
        if (r.sizeCompare(1) == 0 && perfectMatch(term, r.head))
          Nil
        else
          r
      }

    def ignorePerfectMatchStr(query: Query[String]): Query[String] =
      ignorePerfectMatch(query)(_ == _)

    /**
     * Normalises term and options before comparison.
     *
     * @param options Pre-sorted options.
     */
    def normalisedStringQuery[A](norm: String => String, cmp: (String, String) => Boolean, options: Iterable[String]): Query[String] = {
      val os = View.from(options.iterator.map(s => (norm(s), s)))
      term => {
        val t2 = norm(term)
        os.filter(o => cmp(o._1, t2)).take(MaxResults).map(_._2)
      }
    }

    /**
     * Matches options containing the search string, where case is ignored.
     *
     * @param options Pre-sorted options.
     */
    def caseInsensitiveContains(options: Iterable[String]): Query[String] =
      ignorePerfectMatchStr(
        normalisedStringQuery(_.toLowerCase, _ contains _, options))

    /**
     * Matches options containing the search string, where case is ignored.
     *
     * @param options Pre-sorted options.
     */
    def caseInsensitiveStartsWith(options: Iterable[String]): Query[String] =
      ignorePerfectMatchStr(
        normalisedStringQuery(_.toLowerCase, _ startsWith _, options))
  }
}
