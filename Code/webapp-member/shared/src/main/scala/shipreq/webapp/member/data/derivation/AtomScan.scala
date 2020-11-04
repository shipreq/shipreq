package shipreq.webapp.member.data.derivation

import scala.collection.mutable
import shipreq.base.util.fp.Monoid.Implicits._
import shipreq.webapp.member.data._
import shipreq.webapp.member.text.Atom._
import shipreq.webapp.member.text.Text

/** Scanning includes dead reqs.
  *
  * Scanning excludes deletion reasons, and manual issues.
  *
  * @param tagRefs  Live/Dead refers to the requirement context; not the life-state of the tag itself.
  * @param issuesInReqs  Live/Dead refers to the requirement context; not the life-state of the issue itself.
  */
final class AtomScan(val tagRefs          : LiveDeadStatMap[ReqId,          Set[LocAndValue[LocationOf.Tag.InReq, ApplicableTagId]]],
                     val issuesInReqs     : LiveDeadStatMap[ReqId,          Vector[LocAndValue[LocationOf.Text.InReq, AnyIssue]]],
                     val issuesInRcgs     : LiveDeadStatMap[ReqCodeGroupId, Vector[Text.CodeGroupTitle.Issue]],
                     val contentRefsInReqs: LiveDeadStatMap[ReqId,          Vector[LocAndValue[LocationOf.Text.InReq, AnyContentRef]]],
                     val contentRefsInRcgs: LiveDeadStatMap[ReqCodeGroupId, Vector[LocAndValue[LocationOf.Text.InReqCodeGroup, AnyContentRef]]],
                     val reqRefs          : Set[ReqId],
                    ) {

  lazy val issueCounts: LiveDeadStatMap[CustomIssueTypeId, Int] = {
    val r = LiveDeadStatMap.Builder.ofInts[CustomIssueTypeId]()
    issuesInReqs.countByValues(r, _.iterator.map(_.value.typ))
    issuesInRcgs.countByValues(r, _.iterator.map(_.typ))
    r.result()
  }
}

object AtomScan {

  def apply(p: Project): AtomScan = {
    val tagRefs           = LiveDeadStatMap.Builder.set[ReqId,          LocAndValue[LocationOf.Tag.InReq, ApplicableTagId]        ]()
    val issuesInReqs      = LiveDeadStatMap.Builder.vec[ReqId,          LocAndValue[LocationOf.Text.InReq, AnyIssue]              ]()
    val issuesInRcgs      = LiveDeadStatMap.Builder.vec[ReqCodeGroupId, Text.CodeGroupTitle.Issue                                 ]()
    val contentRefsInReqs = LiveDeadStatMap.Builder.vec[ReqId,          LocAndValue[LocationOf.Text.InReq, AnyContentRef]         ]()
    val contentRefsInRcgs = LiveDeadStatMap.Builder.vec[ReqCodeGroupId, LocAndValue[LocationOf.Text.InReqCodeGroup, AnyContentRef]]()
    val reqRefs           = UnivEq.setBuilder[ReqId]

    def scanReqText(live : Live,
                    reqId: ReqId,
                    loc  : Location.Text): ArraySeq[AnyAtom] => Unit =
      scan {
        case a: ContentRef # ReqRef =>
          contentRefsInReqs(reqId).add(live, LocAndValue(loc, a))
          reqRefs += a.id

        case a: ContentRef # CodeRef =>
          contentRefsInReqs(reqId).add(live, LocAndValue(loc, a))

        case a: ContentRef # UseCaseStepRef =>
          contentRefsInReqs(reqId).add(live, LocAndValue(loc, a))

        case a: Issue # Issue =>
          issuesInReqs(reqId).add(live, LocAndValue(loc, a))

        case a: TagRef # TagRef =>
          tagRefs(reqId).add(live, LocAndValue(loc, a.value))

        // Leave this as a catch-all rather than a specific list.
        // Yes it means that when a new Atom type is added there's a chance you'll forget to consider this area but
        // this is a hot-path due to ApplyEvent and benchmarks show that a catch-all here is a significant speedup.
        case _ =>
          ()
      }

    def scanReqCodeGroupText(live     : Live,
                             reqCodeId: ReqCodeGroupId,
                             loc      : LocationOf.Text.InReqCodeGroup): ArraySeq[AnyAtom] => Unit =
      scan {
        case a: ContentRef # ReqRef =>
          contentRefsInRcgs(reqCodeId).add(live, LocAndValue(loc, a))
          reqRefs += a.id

        case a: ContentRef # CodeRef =>
          contentRefsInRcgs(reqCodeId).add(live, LocAndValue(loc, a))

        case a: ContentRef # UseCaseStepRef =>
          contentRefsInRcgs(reqCodeId).add(live, LocAndValue(loc, a))

        case a: Issue # Issue =>
          a match {
            case i: Text.CodeGroupTitle.Issue => issuesInRcgs(reqCodeId).add(live, i)
            case _                            => ()
          }

        case a: TagRef # TagRef =>
          assert(false, s"ReqCodes shouldn't have tags! Found $a in $reqCodeId")

        // Leave this as a catch-all rather than a specific list.
        // Yes it means that when a new Atom type is added there's a chance you'll forget to consider this area but
        // this is a hot-path due to ApplyEvent and benchmarks show that a catch-all here is a significant speedup.
        case _ =>
          ()
      }

    val reqTypesPerReqId = mutable.HashMap.empty[GenericReqId, ReqTypeId]

    // Parse generic reqs
    val rts = p.config.reqTypes
    for (r <- p.content.reqs.genericReqs.imap.valuesIterator) {
      reqTypesPerReqId.update(r.id, r.reqTypeId)
      scanReqText(r.live(rts), r.id, Location.Text.Title)(r.title)
    }

    // Parse use cases
    for (uc <- p.content.reqs.useCases.imap.valuesIterator) {
      scanReqText(uc.liveUC, uc.id, Location.Text.Title)(uc.title)
      for (s <- uc.stepIterator)
        scanReqText(uc.liveUC, uc.id, Location.Text.UseCaseStep(s.id))(s.titleExplicitly)
    }

    // Parse custom-text-field text
    val customTextFieldText = p.content.reqText
    val liveTextFields      = p.config.liveCustomTextFieldIdSet
    val naReqTypesPerField  = p.config.naReqTypesPerField

    // Don't use a for-comprehension here
    // https://github.com/scala/bug/issues/11951
    customTextFieldText.data.foreach { case (fieldId, textByReqId) =>
      val fieldLive = Live.when(liveTextFields contains fieldId)
      val naReqTypes = naReqTypesPerField(fieldId)
      textByReqId.foreach { case (id, txt) =>

        @inline def reqTypeId: ReqTypeId =
          id match {
            case r: GenericReqId => reqTypesPerReqId(r)
            case _: UseCaseId    => StaticReqType.UseCase
          }

        @inline def fieldIsNA = naReqTypes.nonEmpty && naReqTypes.contains(reqTypeId)

        val live = fieldLive & Dead.when(fieldIsNA)

        scanReqText(live, id, Location.Text.CustomTextField(fieldId))(txt.whole)
      }
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
    )
  }

  // ===================================================================================================================

  def reqCodeRefs(f: (ArraySeq[AnyAtom] => Unit) => Unit): Set[ReqCodeId] = {
    val b = UnivEq.setBuilder[ReqCodeId]
    val scan = AtomScan.scan {
      case a: ContentRef # CodeRef => b += a.id
      case _                       => ()
    }
    f(scan)
    b.result()
  }

  def useCaseStepRefs(f: (ArraySeq[AnyAtom] => Unit) => Unit): Set[UseCaseStepId] = {
    val b = UnivEq.setBuilder[UseCaseStepId]
    val scan = AtomScan.scan {
      case a: ContentRef # UseCaseStepRef => b += a.value
      case _                              => ()
    }
    f(scan)
    b.result()
  }

  // ===================================================================================================================

  object UseCaseStepRefs {

    def apply(f: Builder => Unit): Set[UseCaseStepId] = {
      val b = newBuilder()
      f(b)
      b.result()
    }

    def newBuilder(): Builder =
      new Builder(UnivEq.setBuilder[UseCaseStepId])

    final class Builder(private val b: mutable.Builder[UseCaseStepId, Set[UseCaseStepId]]) {

      val scan =
        AtomScan.scan {
          case a: ContentRef # UseCaseStepRef => b += a.value
          case _                              => ()
        }

      def result(): Set[UseCaseStepId] =
        b.result()
    }
  }

  // ===================================================================================================================

  def scan(f: AnyAtom => Unit): ArraySeq[AnyAtom] => Unit = {
    var go: ArraySeq[AnyAtom] => Unit = null

    go = as => {
      val array = as.unsafeArray
      var i = array.length
      while (i > 0) {
        i -= 1
        val a = array(i).asInstanceOf[AnyAtom]

        // Self
        f(a)

        // Recursive cases
        a match {

          case a: Issue # Issue =>
            go(a.desc)

          case a: ListMarkup # UnorderedList =>
            val items = a.items.whole.unsafeArray
            var j = items.length
            while (j > 0) {
              j -= 1
              val li = items(j).asInstanceOf[ArraySeq[AnyAtom]]
              go(li)
            }

          case _ =>
            ()
        }
      }
    }

    go
  }
}