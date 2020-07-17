package shipreq.webapp.base.feature.autocomplete.strategies

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar, PlainText, TextSearch}
import shipreq.webapp.base.ui.BaseStyles.{autoComplete => *}

final case class ReqItem(reqId  : ReqId,
                         pubid  : Pubid,
                         reqType: ReqType,
                         title  : String) {

  val pubidStr     = PlainText.pubid(reqType, pubid.pos)
  val pubidStrNorm = ReqItem.normaliseReqPubid(pubidStr)
  val sortKey      = (reqType.mnemonic.value, pubid.pos.value)

  private[strategies] def candidate =
    RefStrategies.Candidate.standard(title = pubidStr, desc = title)
}

object ReqItem {
  implicit def univEqReqItem: UnivEq[ReqItem] =
    UnivEq.derive

  @inline private[strategies] def normaliseReqPubid(s: String): String =
    Grammar.pubid.seqFormat.normEach(s)
}

/** [REF] */
private[strategies] object RefStrategies {

  final case class Candidate(title: String, replacement: String, render: () => VdomNode) {
    def renderToStr: String =
      ReactDOMServer.renderToStaticMarkup(render())
  }

  object Candidate {

    def standard(title: String, desc: String): Candidate =
      standard(
        title       = title,
        desc        = desc,
        replacement = title,
      )

    def standard(title: String, desc: String, replacement: String): Candidate =
      apply(
        title       = title,
        replacement = replacement,
        render      = () => li(title = title, desc = desc))

    private def li(title: String, desc: String): VdomElement =
      <.div(
        <.div(*.itemTitle, title),
        <.div(*.itemDesc, desc))
  }

  type Candidates = String => Iterator[Candidate]

  def apply(candidates: Candidates): Contextualise => Strategies =
    Context.reflink[Candidate](
      mainRegex      = """([^\s\]]+?)""",
      replacementA   = _.replacement,
      replacementEnd = " ",
      rest           = _.search(candidates(_).take(MaxResults)).template((c, _) => c.renderToStr),
    )

  def combineCandidates(cs: Candidates*): Candidates =
    term => cs.iterator.flatMap(_(term))

  // ===================================================================================================================

  type ReqItems = ArraySeq[ReqItem]

  def reqItems(p: Project, pt: PlainText.ForProject.AnyCtx, legal: Iterable[Req]): ReqItems =
    MutableArray(
      legal
        .iterator
        .filter(_.live(p.config.reqTypes) is Live)
        .map(req => ReqItem(req.id, req.pubid, p.config.reqTypes.need(req.pubid.reqTypeId), pt.reqTitle(req))))
      .sortBy(_.sortKey)
      .arraySeq

  def candidatesByPubid(items: ArraySeq[ReqItem]): Candidates =
    term => {
      val np = ReqItem.normaliseReqPubid(term)
      items.iterator.filter(_.pubidStrNorm.contains(np)).map(_.candidate)
    }

  def candidatesByTitle(items: ArraySeq[ReqItem], textSearch: TextSearch): Candidates = {
    val reqIdSet: Set[ReqId] =
      items.iterator.map(_.reqId).toSet

    val searchTitles =
      textSearch.ignoreCaseNoWhitespace
        .filterReqsIds(reqIdSet)
        .titlesOnly

    term => {
      val titles = searchTitles.searchAll(term).take(MaxResults).map(_.id).toSet
      items.iterator.filter(i => titles.contains(i.reqId)).map(_.candidate)
    }
  }
}
