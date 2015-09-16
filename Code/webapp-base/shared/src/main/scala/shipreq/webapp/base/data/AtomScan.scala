package shipreq.webapp.base.data

import scalaz.std.vector.vectorMonoid
import shipreq.base.util.UnivEq
import shipreq.webapp.base.text.{Atom, Text}
import Atom._
import AtomScan.IssueLoc

/**
 * Scanning includes dead reqs.
 *
 * @param codeRefs ReqCodes referenced in anything anywhere (including text in dead custom-text fields).
 */
class AtomScan(val tagRefs : LDStats[ReqId, Set[ApplicableTagId]],
               val issues  : LDStats[IssueLoc, Vector[AnyIssue]],
               val codeRefs: Set[ReqCodeId]) {

  lazy val issueCounts: LDStats[CustomIssueTypeId, Int] =
    issues.countByValues(_.toStream.map(_.typ))
}

object AtomScan {

  sealed trait IssueLoc
  case class InReq(id: ReqId)     extends IssueLoc
  case class InRCG(id: ReqCodeId) extends IssueLoc
  implicit def issueLocEq: UnivEq[IssueLoc] = UnivEq.deriveAuto

  private implicit val tagSetMonoid = UnivEq.setMonoid[ApplicableTagId]

  def apply(p: Project): AtomScan = {
    val tagRefs  = new LDStats.Builder[ReqId, Set[ApplicableTagId]]
    val issues   = new LDStats.Builder[IssueLoc, Vector[AnyIssue]]
    var codeRefs = UnivEq.emptySet[ReqCodeId]

    def scan(live     : Live,
             reqId    : ReqId     = null,
             reqCodeId: ReqCodeId = null)
            (text: Text.AnyOptional): Unit = {

      def go(as: Text.AnyOptional): Unit =
        as foreach {
          case _: Literal         # Literal
             | _: ReqRef          # ReqRef
             | _: PlainTextMarkup # EmailAddress
             | _: PlainTextMarkup # WebAddress
             | _: PlainTextMarkup # MathTeX
             | _: NewLine         # BlankLine => ()

          case a: ReqRef#CodeRef =>
            codeRefs += a.value

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
    val rts = p.config.customReqTypes
    p.reqs.reqs.values.foreach {
      case r: GenericReq =>
        scan(r.live(rts), reqId = r.id)(r.title)
    }

    // Parse custom-text-field text
    val customTextFieldText = p.reqText
    val liveTextFields      = p.config.liveCustomTextFields.map(_.id).toSet
    for {
      (tf, textByReqId) ← customTextFieldText
      live              = Live <~ (liveTextFields contains tf)
      (id, txt)         ← textByReqId
    } {
      scan(live, reqId = id)(txt.whole)
    }

    // Parse ReqCode groups
    for (gi <- p.reqCodes.activeGroups)
      scan(Live, reqCodeId = gi.id)(gi.group.title)

    new AtomScan(tagRefs.result(), issues.result(), codeRefs)
  }
}