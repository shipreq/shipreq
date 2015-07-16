package shipreq.webapp.base.data

import japgolly.nyaya.util.Multimap
import shipreq.base.util.UnivEq
import shipreq.webapp.base.text.{Atom, Text}
import Atom._

/**
 * Scanning includes dead reqs.
 *
 * @param tagsInReqText Excludes text in dead custom-text fields.
 * @param issuesInReqText Excludes text in dead custom-text fields.
 * @param codeRefs ReqCodes referenced in anything anywhere (including text in dead custom-text fields).
 */
class AtomScan(val tagsInReqText    : Multimap[ReqId,     Set,    ApplicableTagId],
               val issuesInReqText  : Multimap[ReqId,     Vector, AnyIssue],
               val issuesInGroupText: Multimap[ReqCodeId, Vector, AnyIssue],
               val codeRefs         : Set[ReqCodeId])

object AtomScan {
  def apply(p: Project): AtomScan = {
    var tagsR   : Multimap[ReqId,     Set,    ApplicableTagId] = UnivEq.emptySetMultimap
    var issuesR : Multimap[ReqId,     Vector, AnyIssue]        = UnivEq.emptyMultimap
    var issuesG : Multimap[ReqCodeId, Vector, AnyIssue]        = UnivEq.emptyMultimap
    var codeRefs: Set[ReqCodeId]                               = UnivEq.emptySet

    def parseR(k: ReqId)(atom: AnyAtom): Unit =
      atom match {
        case a: AnyIssue       => issuesR = issuesR.add(k, a)
        case a: TagRef#TagRef  => tagsR = tagsR.add(k, a.value)
        case a: ReqRef#CodeRef => codeRefs += a.value
        case _                 => ()
      }

    def parseD(atom: AnyAtom): Unit =
      atom match {
        case a: ReqRef#CodeRef => codeRefs += a.value
        case _                 => ()
      }

    def parseG(k: ReqCodeId)(atom: AnyAtom): Unit =
      atom match {
        case a: AnyIssue       => issuesG = issuesG.add(k, a)
        case a: ReqRef#CodeRef => codeRefs += a.value
        case _                 => ()
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
      (tf, m2) <- customTextFieldText
      (reqId, txt) <- m2
    } {
      if (liveTextFields contains tf)
        parse(parseR(reqId), txt.whole)
      else
        parse(parseD, txt.whole)
    }

    // Parse ReqCode groups
    for (gi <- p.reqCodes.data.activeGroups)
      parse(parseG(gi.id), gi.group.title)

    new AtomScan(tagsR, issuesR, issuesG, codeRefs)
  }
}