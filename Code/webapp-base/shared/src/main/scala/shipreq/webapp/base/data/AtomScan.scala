package shipreq.webapp.base.data

import japgolly.nyaya.util.Multimap
import shipreq.base.util.UnivEq
import shipreq.webapp.base.text.{Atom, Text}
import Atom._

/**
 * Scans dead reqs.
 * Doesn't scan dead custom-text fields.
 */
class AtomScan(val tagsInReqText    : Multimap[ReqId,     Set,    ApplicableTagId],
               val issuesInReqText  : Multimap[ReqId,     Vector, AnyIssue],
               val issuesInGroupText: Multimap[ReqCodeId, Vector, AnyIssue])

object AtomScan {
  def apply(p: Project): AtomScan = {
    var tagsR  : Multimap[ReqId,     Set,    ApplicableTagId] = UnivEq.emptySetMultimap
    var issuesR: Multimap[ReqId,     Vector, AnyIssue]        = UnivEq.emptyMultimap
    var issuesG: Multimap[ReqCodeId, Vector, AnyIssue]        = UnivEq.emptyMultimap

    def parseR(k: ReqId)(atom: AnyAtom): Unit =
      atom match {
        case i: AnyIssue      => issuesR = issuesR.add(k, i)
        case t: TagRef#TagRef => tagsR = tagsR.add(k, t.value)
        case _                => ()
      }

    def parseG(k: ReqCodeId)(atom: AnyAtom): Unit =
      atom match {
        case i: AnyIssue => issuesG = issuesG.add(k, i)
        case _           => ()
      }

    def parse(parser: AnyAtom => Unit, text: Text.AnyOptional): Unit =
      text foreach parser

    // Parse reqs
    p.reqs.data.reqs.values.foreach {
      case r: GenericReq =>
        parse(parseR(r.id), r.title)
    }

    // Parse custom-text-field text
    val customTextFieldText = p.reqText.data
    val liveTextFields      = p.config.liveCustomTextFields.map(_.id).toSet
    for {
      (tf, m2) <- customTextFieldText if liveTextFields.contains(tf)
      (reqId, txt) <- m2
    } parse(parseR(reqId), txt.whole)

    // Parse ReqCode groups
    for (gi <- p.reqCodes.data.activeGroups)
      parse(parseG(gi.id), gi.group.title)

    new AtomScan(tagsR, issuesR, issuesG)
  }
}