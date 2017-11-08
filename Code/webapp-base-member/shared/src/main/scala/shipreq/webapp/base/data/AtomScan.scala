package shipreq.webapp.base.data

import scalaz.std.vector.vectorMonoid
import shipreq.base.util.univeq._
import shipreq.webapp.base.text.Atom._
import AtomScan.IssueLoc

/**
 * Scanning includes dead reqs.
 *
 * @param codeRefs ReqCodes referenced in anything anywhere (including text in dead custom-text fields).
 */
class AtomScan(val tagRefs        : LiveDeadStatMap[ReqId, Set[ApplicableTagId]],
               val issues         : LiveDeadStatMap[IssueLoc, Vector[AnyIssue]],
               val reqRefs        : Set[ReqId],
               val codeRefs       : Set[ReqCodeId],
               val useCaseStepRefs: Set[UseCaseStepId]) {

  lazy val issueCounts: LiveDeadStatMap[CustomIssueTypeId, Int] =
    issues.countByValues(_.toStream.map(_.typ))
}

// TODO AtomScan doesn't scan deletion reasons
object AtomScan {

  sealed trait IssueLoc
  case class InReq(id: ReqId)     extends IssueLoc
  case class InRCG(id: ReqCodeId) extends IssueLoc
  implicit def issueLocEq: UnivEq[IssueLoc] = UnivEq.derive

  private implicit val tagSetMonoid = monoidSet[ApplicableTagId]

  def apply(p: Project): AtomScan = {
    val tagRefs         = new LiveDeadStatMap.Builder[ReqId, Set[ApplicableTagId]]
    val issues          = new LiveDeadStatMap.Builder[IssueLoc, Vector[AnyIssue]]
    val reqRefs         = UnivEq.setBuilder[ReqId]
    val codeRefs        = UnivEq.setBuilder[ReqCodeId]
    val useCaseStepRefs = UnivEq.setBuilder[UseCaseStepId]

    def scan(live     : Live,
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

          case a: ReqRef#ReqRef =>
            reqRefs += a.value

          case a: ReqRef#CodeRef =>
            codeRefs += a.value

          case a: UseCaseStepRef#UseCaseStepRef =>
            useCaseStepRefs += a.value

          case a: Issue#Issue =>
            if (reqId     ne null) issues(InReq(reqId))    .mod(live)(_ :+ a)
            if (reqCodeId ne null) issues(InRCG(reqCodeId)).mod(live)(_ :+ a)
            go(a.desc)

          case a: TagRef#TagRef =>
            if (reqId ne null) tagRefs(reqId).mod(live)(_ + a.value)

          case a: ListMarkup#UnorderedList =>
            a.items foreach go
        }

      go(text)
    }

    // Parse reqs
    val rts = p.config.reqTypes
    p.content.reqs.reqIterator.foreach {

      case r: GenericReq =>
        scan(r.live(rts), reqId = r.id)(r.title)

      case uc: UseCase =>
        scan(uc.liveUC, reqId = uc.id)(uc.title)
        scan(uc.liveUC, reqId = uc.id)(uc.stepIterator.flatMap(_.titleExplicitly))
    }

    // Parse custom-text-field text
    val customTextFieldText = p.content.reqText
    val liveTextFields      = p.config.liveCustomTextFields.map(_.id).toSet
    for {
      (tf, textByReqId) ← customTextFieldText
      live              = Live when (liveTextFields contains tf)
      (id, txt)         ← textByReqId
    } {
      scan(live, reqId = id)(txt.whole)
    }

    // Parse ReqCode groups
    for (g <- p.content.reqCodes.groups)
      scan(g.live, reqCodeId = g.id)(g.title)

    new AtomScan(tagRefs.result(), issues.result(), reqRefs.result(), codeRefs.result(), useCaseStepRefs.result())
  }
}