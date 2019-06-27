package shipreq.webapp.base.data

import scalaz.std.anyVal.intInstance
import scalaz.std.vector.vectorMonoid
import shipreq.base.util.univeq._
import shipreq.webapp.base.text.Atom._
import shipreq.webapp.base.text.Text

/** Scanning includes dead reqs.
  *
  * @param tagRefs  Live/Dead refers to the requirement context; not the life-state of the tag itself.
  * @param issuesInReqs  Live/Dead refers to the requirement context; not the life-state of the issue itself.
  * @param codeRefs ReqCodes referenced in anything anywhere (including text in dead custom-text fields).
  */
final class AtomScan(val tagRefs          : LiveDeadStatMap[ReqId, Set[ReqTextLoc.And[ApplicableTagId]]],
                     val issuesInReqs     : LiveDeadStatMap[ReqId, Vector[ReqTextLoc.And[AnyIssue]]],
                     val issuesInRcgs     : LiveDeadStatMap[ReqCodeId, Vector[Text.CodeGroupTitle.Issue]],
                     val contentRefsInReqs: LiveDeadStatMap[ReqId, Vector[ReqTextLoc.And[AnyContentRef]]],
                     val contentRefsInRcgs: LiveDeadStatMap[ReqCodeId, Vector[AnyContentRef]],
                     val reqRefs          : Set[ReqId],
                     val codeRefs         : Set[ReqCodeId],
                     val useCaseStepRefs  : Set[UseCaseStepId]
                    ) {

  lazy val issueCounts: LiveDeadStatMap[CustomIssueTypeId, Int] = {
    val r = new LiveDeadStatMap.Builder[CustomIssueTypeId, Int]
    issuesInReqs.countByValues(r, _.iterator.map(_.value.typ))
    issuesInRcgs.countByValues(r, _.iterator.map(_.typ))
    r.result()
  }
}

// TODO AtomScan doesn't scan deletion reasons
object AtomScan {

  private implicit val tagSetMonoid = scalazMonoidSet[ReqTextLoc.And[ApplicableTagId]]

  def apply(p: Project): AtomScan = {
    val tagRefs           = new LiveDeadStatMap.Builder[ReqId, Set[ReqTextLoc.And[ApplicableTagId]]]
    val issuesInReqs      = new LiveDeadStatMap.Builder[ReqId, Vector[ReqTextLoc.And[AnyIssue]]]
    val issuesInRcgs      = new LiveDeadStatMap.Builder[ReqCodeId, Vector[Text.CodeGroupTitle.Issue]]
    val contentRefsInReqs = new LiveDeadStatMap.Builder[ReqId, Vector[ReqTextLoc.And[AnyContentRef]]]
    val contentRefsInRcgs = new LiveDeadStatMap.Builder[ReqCodeId, Vector[AnyContentRef]]
    val reqRefs           = UnivEq.setBuilder[ReqId]
    val codeRefs          = UnivEq.setBuilder[ReqCodeId]
    val useCaseStepRefs   = UnivEq.setBuilder[UseCaseStepId]

    def scan(live     : Live,
             loc      : ReqTextLoc,
             reqId    : ReqId     = null,
             reqCodeId: ReqCodeId = null)
            (text     : TraversableOnce[AnyAtom]): Unit = {

      def go(as: TraversableOnce[AnyAtom]): Unit =
        as foreach {
          case _: Literal         # Literal
             | _: PlainTextMarkup # EmailAddress
             | _: PlainTextMarkup # WebAddress
             | _: PlainTextMarkup # MathTeX
             | _: NewLine         # BlankLine => ()

          case a: ContentRef#ReqRef =>
            if (reqId     ne null) contentRefsInReqs(reqId).mod(live)(_ :+ ReqTextLoc.And(loc, a))
            if (reqCodeId ne null) contentRefsInRcgs(reqCodeId).mod(live)(_ :+ a)
            reqRefs += a.value

          case a: ContentRef#CodeRef =>
            if (reqId     ne null) contentRefsInReqs(reqId).mod(live)(_ :+ ReqTextLoc.And(loc, a))
            if (reqCodeId ne null) contentRefsInRcgs(reqCodeId).mod(live)(_ :+ a)
            codeRefs += a.value

          case a: ContentRef#UseCaseStepRef =>
            if (reqId     ne null) contentRefsInReqs(reqId).mod(live)(_ :+ ReqTextLoc.And(loc, a))
            if (reqCodeId ne null) contentRefsInRcgs(reqCodeId).mod(live)(_ :+ a)
            useCaseStepRefs += a.value

          case a: Issue#Issue =>
            if (reqId     ne null) issuesInReqs(reqId).mod(live)(_ :+ ReqTextLoc.And(loc, a))
            if (reqCodeId ne null) issuesInRcgs(reqCodeId).mod(live)(_ :+ a.asInstanceOf[Text.CodeGroupTitle.Issue]) // TODO prove
            go(a.desc)

          case a: TagRef#TagRef =>
            if (reqId ne null) tagRefs(reqId).mod(live)(_ + ReqTextLoc.And(loc, a.value))

          case a: ListMarkup#UnorderedList =>
            a.items foreach go
        }

      go(text)
    }

    // Parse reqs
    val rts = p.config.reqTypes
    p.content.reqs.reqIterator.foreach {

      case r: GenericReq =>
        scan(r.live(rts), ReqTextLoc.Title, reqId = r.id)(r.title)

      case uc: UseCase =>
        scan(uc.liveUC, ReqTextLoc.Title, reqId = uc.id)(uc.title)
        for (s <- uc.stepIterator)
          scan(uc.liveUC, ReqTextLoc.UseCaseStep(s.id), reqId = uc.id)(s.titleExplicitly)
    }

    // Parse custom-text-field text
    val customTextFieldText = p.content.reqText
    val liveTextFields      = p.config.liveCustomTextFields.map(_.id).toSet
    for {
      (tf, textByReqId) ← customTextFieldText
      live              = Live when (liveTextFields contains tf)
      (id, txt)         ← textByReqId
    } {
      scan(live, ReqTextLoc.CustomTextField(tf), reqId = id)(txt.whole)
    }

    // Parse ReqCode groups
    for (g <- p.content.reqCodes.groups) {
      @inline def loc: ReqTextLoc = null // Tags are not allowed in CodeGroupTitle
      scan(g.live, loc, reqCodeId = g.id)(g.title)
    }

    new AtomScan(
      tagRefs.result(),
      issuesInReqs.result(),
      issuesInRcgs.result(),
      contentRefsInReqs.result(),
      contentRefsInRcgs.result(),
      reqRefs.result(),
      codeRefs.result(),
      useCaseStepRefs.result())
  }
}