package shipreq.webapp.base.data

import shipreq.base.util.fp.Monoid.Implicits._
import shipreq.base.util.univeq._
import shipreq.webapp.base.text.Atom._
import shipreq.webapp.base.text.Text

/** Scanning includes dead reqs.
  *
  * Scanning excludes deletion reasons, and manual issues.
  *
  * @param tagRefs  Live/Dead refers to the requirement context; not the life-state of the tag itself.
  * @param issuesInReqs  Live/Dead refers to the requirement context; not the life-state of the issue itself.
  * @param codeRefs ReqCodes referenced in anything anywhere (including text in dead custom-text fields).
  */
final class AtomScan(_tagRefs          : LiveDeadStatMap[ReqId, Set[LocAndValue[LocationOf.Tag.InReq, ApplicableTagId]]],
                     _issuesInReqs     : LiveDeadStatMap[ReqId, Vector[LocAndValue[LocationOf.Text.InReq, AnyIssue]]],
                     _issuesInRcgs     : LiveDeadStatMap[ReqCodeGroupId, Vector[Text.CodeGroupTitle.Issue]],
                     _contentRefsInReqs: LiveDeadStatMap[ReqId, Vector[LocAndValue[LocationOf.Text.InReq, AnyContentRef]]],
                     _contentRefsInRcgs: LiveDeadStatMap[ReqCodeGroupId, Vector[LocAndValue[LocationOf.Text.InReqCodeGroup, AnyContentRef]]],
                     _reqRefs          : Set[ReqId],
                     _codeRefs         : Set[ReqCodeId],
                     _useCaseStepRefs  : Set[UseCaseStepId]
                    ) {

  CC.inc("AtomScan.instance")

  lazy val issueCounts: LiveDeadStatMap[CustomIssueTypeId, Int] = {
    CC.inc("AtomScan.issueCounts")
    val r = new LiveDeadStatMap.Builder[CustomIssueTypeId, Int]
    issuesInReqs.countByValues(r, _.iterator.map(_.value.typ))
    issuesInRcgs.countByValues(r, _.iterator.map(_.typ))
    r.result()
  }

  lazy val tagRefs           = CC("AtomScan.read.tagRefs          ")(_tagRefs          )
  lazy val issuesInReqs      = CC("AtomScan.read.issuesInReqs     ")(_issuesInReqs     )
  lazy val issuesInRcgs      = CC("AtomScan.read.issuesInRcgs     ")(_issuesInRcgs     )
  lazy val contentRefsInReqs = CC("AtomScan.read.contentRefsInReqs")(_contentRefsInReqs)
  lazy val contentRefsInRcgs = CC("AtomScan.read.contentRefsInRcgs")(_contentRefsInRcgs)
  lazy val reqRefs           = CC("AtomScan.read.reqRefs          ")(_reqRefs          )
  lazy val codeRefs          = CC("AtomScan.read.codeRefs         ")(_codeRefs         )
  lazy val useCaseStepRefs   = CC("AtomScan.read.useCaseStepRefs  ")(_useCaseStepRefs  )
}

object AtomScan {

  def apply(p: Project): AtomScan = {
    val tagRefs           = new LiveDeadStatMap.Builder[ReqId,          Set[LocAndValue[LocationOf.Tag.InReq, ApplicableTagId]]]
    val issuesInReqs      = new LiveDeadStatMap.Builder[ReqId,          Vector[LocAndValue[LocationOf.Text.InReq, AnyIssue]]]
    val issuesInRcgs      = new LiveDeadStatMap.Builder[ReqCodeGroupId, Vector[Text.CodeGroupTitle.Issue]]
    val contentRefsInReqs = new LiveDeadStatMap.Builder[ReqId,          Vector[LocAndValue[LocationOf.Text.InReq, AnyContentRef]]]
    val contentRefsInRcgs = new LiveDeadStatMap.Builder[ReqCodeGroupId, Vector[LocAndValue[LocationOf.Text.InReqCodeGroup, AnyContentRef]]]
    val reqRefs           = UnivEq.setBuilder[ReqId]
    val codeRefs          = UnivEq.setBuilder[ReqCodeId]
    val useCaseStepRefs   = UnivEq.setBuilder[UseCaseStepId]

    def scanReqText(live : Live,
                    reqId: ReqId,
                    loc  : Location.Text)
                   (text : IterableOnce[AnyAtom]): Unit = {

      def go(as: IterableOnce[AnyAtom]): Unit =
        as.iterator foreach {
          case _: Literal         # Literal
             | _: NewLine         # BlankLine
             | _: PlainTextMarkup # EmailAddress
             | _: PlainTextMarkup # Monospace
             | _: PlainTextMarkup # TeX
             | _: PlainTextMarkup # WebAddress
             | _: CodeBlock       # CodeBlock
            => ()

          case a: ContentRef#ReqRef =>
            contentRefsInReqs(reqId).mod(live)(_ :+ LocAndValue(loc, a))
            reqRefs += a.value

          case a: ContentRef#CodeRef =>
            contentRefsInReqs(reqId).mod(live)(_ :+ LocAndValue(loc, a))
            codeRefs += a.value

          case a: ContentRef#UseCaseStepRef =>
            contentRefsInReqs(reqId).mod(live)(_ :+ LocAndValue(loc, a))
            useCaseStepRefs += a.value

          case a: Issue#Issue =>
            issuesInReqs(reqId).mod(live)(_ :+ LocAndValue(loc, a))
            go(a.desc)

          case a: TagRef#TagRef =>
            tagRefs(reqId).mod(live)(_ + LocAndValue(loc, a.value))

          case a: ListMarkup#UnorderedList =>
            a.items foreach go
        }

      go(text)
    }

    def scanReqCodeGroupText(live     : Live,
                             reqCodeId: ReqCodeGroupId,
                             loc      : LocationOf.Text.InReqCodeGroup)
                            (text     : IterableOnce[AnyAtom]): Unit = {

      def go(as: IterableOnce[AnyAtom]): Unit =
        as.iterator foreach {
          case _: Literal         # Literal
             | _: NewLine         # BlankLine
             | _: PlainTextMarkup # EmailAddress
             | _: PlainTextMarkup # Monospace
             | _: PlainTextMarkup # TeX
             | _: PlainTextMarkup # WebAddress
             | _: CodeBlock       # CodeBlock
            => ()

          case a: ContentRef#ReqRef =>
            contentRefsInRcgs(reqCodeId).mod(live)(_ :+ LocAndValue(loc, a))
            reqRefs += a.value

          case a: ContentRef#CodeRef =>
            contentRefsInRcgs(reqCodeId).mod(live)(_ :+ LocAndValue(loc, a))
            codeRefs += a.value

          case a: ContentRef#UseCaseStepRef =>
            contentRefsInRcgs(reqCodeId).mod(live)(_ :+ LocAndValue(loc, a))
            useCaseStepRefs += a.value

          case a: Issue#Issue =>
            a match {
              case i: Text.CodeGroupTitle.Issue => issuesInRcgs(reqCodeId).mod(live)(_ :+ i)
              case _                            => ()
            }
            go(a.desc)

          case a: TagRef#TagRef =>
            assert(false, s"ReqCodes shouldn't have tags! Found $a in $reqCodeId")

          case a: ListMarkup#UnorderedList =>
            a.items foreach go
        }

      go(text)
    }

    // Parse reqs
    val rts = p.config.reqTypes
    p.content.reqs.reqIterator().foreach {

      case r: GenericReq =>
        scanReqText(r.live(rts), r.id, Location.Text.Title)(r.title)

      case uc: UseCase =>
        scanReqText(uc.liveUC, uc.id, Location.Text.Title)(uc.title)
        for (s <- uc.stepIterator)
          scanReqText(uc.liveUC, uc.id, Location.Text.UseCaseStep(s.id))(s.titleExplicitly)
    }

    // Parse custom-text-field text
    val customTextFieldText = p.content.reqText
    val liveTextFields      = p.config.liveCustomTextFields.map(_.id).toSet
    for {
      (tf, textByReqId) <- customTextFieldText
      live              = Live when (liveTextFields contains tf)
      (id, txt)         <- textByReqId
    } {
      scanReqText(live, id, Location.Text.CustomTextField(tf))(txt.whole)
    }

    // Parse ReqCode groups
    for (g <- p.content.reqCodes.groups)
      scanReqCodeGroupText(g.live, g.id, Location.Text.Title)(g.title)

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