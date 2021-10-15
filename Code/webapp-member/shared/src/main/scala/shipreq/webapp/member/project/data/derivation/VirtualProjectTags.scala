package shipreq.webapp.member.project.data.derivation

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.Memo
import scala.collection.immutable.ListMap
import scala.collection.mutable
import shipreq.base.util._
import shipreq.webapp.member.project.data.DataImplicits._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.text.PlainText

/** The resulting tags for all requirements in a project, after automation has been applied.
  *
  * Automation is currently:
  *   - field defaults
  *   - derivative tags (MF-34)
  *
  * This is basically an API to access project tags as seen from the user's point of view.
  */
sealed trait VirtualProjectTags {
  import VirtualProjectTags._

  def apply(reqId: ReqId): ResultsMono

  def apply(reqId: ReqId, filterDead: FilterDead): ResultsLiveDead

  def underTagGroup(tagGroupId: TagGroupId, filterDead: FilterDead): ReqId => Vector[ApplicableTagId]

  def withTagFieldDistFn(distFn: FilterDead => TagFieldDistribution.TagIds): VirtualProjectTags

  final def withTagFieldDist(dist: TagFieldDistribution.TagIds): VirtualProjectTags =
    withTagFieldDistFn(_ => dist)

  def prettyPrint: String

  @elidable(elidable.INFO)
  final override def toString = prettyPrint
}

object VirtualProjectTags {

  /** Results that aren't indexed by anything. */
  sealed trait ResultsMono {
    def conflictingTagGroups: Set[TagGroupId]
    def conflictingTags: Multimap[ApplicableTagId, List, LocationOf.Tag.InReq]
    def naTagsInLiveText: Multimap[ApplicableTagId, List, Location.Text]
    def derivativeTagFactors(field: CustomField.Tag.Id): Set[DerivativeTagFactor]
    def childrenSummary(field: CustomField.Tag.Id): ChildrenSummary
  }

  /** Results indexed by FilterDead. */
  sealed trait ResultsLiveDead {
    def apply(id: ApplicableTagId, f: TagFieldId): VirtualTag

    def set: TagFieldId => Set[ApplicableTagId]
    def ordered: TagFieldId => Vector[ApplicableTagId]

    def defaults: Map[CustomField.Tag.Id, ApplicableTagId]

    def withTagFieldDist(dist: TagFieldDistribution.TagIds): ResultsLiveDead

    def isEmpty: Boolean
    final def nonEmpty = !isEmpty
  }

  sealed trait DerivativeTagFactor

  object DerivativeTagFactor {
    case object EmptySelf extends DerivativeTagFactor

    final case class EmptyRelation(sourceReq: ReqId,
                                   dir      : Direction) extends DerivativeTagFactor

    final case class Self(tag       : ApplicableTagId,
                          provenance: Provenance.NonDerived) extends DerivativeTagFactor

    final case class Relation(sourceReq : ReqId,
                              dir       : Direction,
                              tag       : ApplicableTagId,
                              provenance: Provenance) extends DerivativeTagFactor

    implicit def univEq: UnivEq[DerivativeTagFactor] = UnivEq.derive
  }

  /** From whence thy tags did come. */
  sealed trait Provenance

  object Provenance {
    sealed trait NonDerived extends Provenance
    sealed trait Manual     extends NonDerived

    case object ManualTag    extends Manual
    case object ManualInText extends Manual
    case object Default      extends NonDerived
    case object Derived      extends Provenance

    implicit def univEqM: UnivEq[Manual] = UnivEq.derive
    implicit def univEqN: UnivEq[NonDerived] = UnivEq.derive
    implicit def univEq: UnivEq[Provenance] = UnivEq.derive

    def fromTagLoc(loc: LocationOf.Tag.InReq): Manual =
      loc match {
        case Location.Tags    => ManualTag
        case _: Location.Text => ManualInText
      }

    def fromTagLocs(locs: IterableOnce[LocationOf.Tag.InReq]): Set[Manual] =
      locs.iterator.map(fromTagLoc).toSet
  }

  sealed trait VirtualTag {
    def id            : ApplicableTagId
    def provenances   : Set[Provenance]
    def live          : Live
    def validity      : Validity
    def isManualTag   : Boolean
    def isManualInText: Boolean
    def isDefault     : Boolean
    def isDerived     : Boolean
    def derivationDesc: Option[DerivationDesc]

    @inline final def isDead = live is Dead
  }

  object VirtualTag {
    final class Empty(val id: ApplicableTagId) extends VirtualTag {
        override def provenances    = Set.empty
        override def live           = Live
        override def validity       = Valid
        override def isManualTag    = false
        override def isManualInText = false
        override def isDefault      = false
        override def isDerived      = false
        override def derivationDesc = None
      }
  }

  final case class ProgressBarPortion(tagId  : Option[ApplicableTagId],
                                      tag    : Option[ApplicableTag],
                                      portion: Double,
                                      total  : Int) {

    def pct1    = portion / total.toDouble
    def pct100d = pct1 * 100d
    val pct100s = pct100d.toString + "%"
    def pct100i = Math.round(pct100d).toInt
    def name    = tag.fold("no tag")(_.name)
    val desc    = s"${pct100i}% $name"
  }

  implicit def univEqProgressBarPortion: UnivEq[ProgressBarPortion] = UnivEq.derive

  final case class DerivationDesc(factors: ArraySeq[DerivationDesc.Factor],
                                  steps: ArraySeq[DerivationDesc.DerivStep])

  object DerivationDesc {
    final case class Factor(tag: Option[ApplicableTagId], reqs: String)
    final case class DerivStep(tags: NonEmptyVector[ApplicableTagId])
  }

  sealed trait ChildrenSummary {
    final type ProgressBar = ArraySeq[ProgressBarPortion]
    def progressBar: ProgressBar

    /** The number of live, relevant, unique, transitive children, and this req itself.
      * Also the sum of all aggregated values.
      */
    def total: Int
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private object Mutable {

    type DerivedTags = Multimap[ApplicableTagId, Set, CustomField.Tag.Id]
    val emptyDerivedTags: DerivedTags = Multimap.empty

    @inline def addDerivedTags(prev: DerivedTags, field: CustomField.Tag.Id, tags: Set[ApplicableTagId]): DerivedTags =
      prev.addks(tags, field)

    val emptyDTF = Multimap.empty[ReqId, Set, DerivativeTagFactor]

    object ForReq {
      val emptyInReq  = Multimap.empty[ApplicableTagId, List, LocationOf.Tag.InReq]
      val emptyInText = Multimap.empty[ApplicableTagId, List, Location.Text]
    }

    final class ForReq(val req: Req,
                       val reqLive: Live,
                       nonApplicableTags: Set[ApplicableTagId],
                       tagLive: ApplicableTagId => Live) {
      var conflictingGroups  = Set.empty[TagGroupId]
      var conflictingTags    = ForReq.emptyInReq
      var manualLive         = ForReq.emptyInReq
      var manualDead         = ForReq.emptyInReq
      var manualHiddenNA     = ForReq.emptyInReq
      var deadTagsInLiveText = ForReq.emptyInText
      var naTagsInLiveText   = ForReq.emptyInText
      var liveDefaults       = Map.empty[CustomField.Tag.Id, ApplicableTagId]
      var deadDefaults       = Map.empty[CustomField.Tag.Id, ApplicableTagId] // dead tags in live reqs, not defaults applied to dead reqs
      var liveDerived        = emptyDerivedTags
      var deadDerived        = emptyDerivedTags

      def addManual(id    : ApplicableTagId,
                    loc   : LocationOf.Tag.InReq,
                    ctx   : Live): Unit =
        if (nonApplicableTags.contains(id))
          manualHiddenNA = manualHiddenNA.add(id, loc)
        else
          ctx & tagLive(id) match {
            case Live => manualLive = manualLive.add(id, loc)
            case Dead => manualDead = manualDead.add(id, loc)
          }

      def addTagInLiveText(id: ApplicableTagId, loc: Location.Text): Unit =
        if (tagLive(id) is Dead)
          deadTagsInLiveText = deadTagsInLiveText.add(id, loc)
        else if (nonApplicableTags.contains(id))
          naTagsInLiveText = naTagsInLiveText.add(id, loc)
        else
          manualLive = manualLive.add(id, loc)

      def addDefault(f: CustomField.Tag.Id, id: ApplicableTagId, fieldLive: Live): Unit =
        reqLive match {
          case Live =>
            (fieldLive & tagLive(id)) match {
              case Live => liveDefaults = liveDefaults.updated(f, id)
              case Dead => deadDefaults = deadDefaults.updated(f, id)
            }
          case Dead =>
            deadDefaults = deadDefaults.updated(f, id)
        }
    }

    final case class DerivativeTagField(fieldId       : CustomField.Tag.Id,
                                        derivativeTags: ReqTypeId => DerivativeTags,
                                        naReqTypes    : Set[ReqTypeId],
                                        liveValidTags : ReqTypeId => Set[ApplicableTagId])
  }

  private final class Mutable(val p: Project) {
    import Mutable._

    val deadTags         = p.config.tags.deadApplicableTagIds
    val tagsInText       = p.atomScan.tagRefs
    val tagLive          = (id: ApplicableTagId) => Dead when deadTags.contains(id)
    val reqTags          = p.content.reqTags
    val reqTypes         = p.config.reqTypes
    val fieldRules       = p.config.fieldRules(ShowDead) // ShowDead so that it doesn't replace dead defaults with Optional
    val liveTagDist      = p.config.liveTagFieldDistribution
    val imps             = p.content.implications
    val tagOrderByName   = p.config.tags.orderByName
    val tagOrderByPos    = p.config.tags.orderByPos
    val tagOrderingByPos = p.config.tags.orderingByPos
    val exclusiveGroups  = p.config.tags.exclusiveGroups

    type Data = mutable.HashMap[ReqId, ForReq]

    val data: Data = mutable.HashMap.empty

    // Step 1: Scan all requirements
    for (req <- p.content.reqs.reqIterator()) {
      val nonApplicableTags = p.config.naTags(req.reqTypeId).set
      val b = new ForReq(req, req.live(reqTypes), nonApplicableTags, tagLive)
      data.update(req.id, b)

      // Process current req
      collectManualTags(req, b)
      addDefaults(req, b)
      separateConflictingTags(b)
    }

    // Step 2: Derivative tags
    val (dtFields, dtFactors) = applyDerivativeTags(data)

    // =================================================================================================================
    private def collectManualTags(req: Req, b: ForReq): Unit = {

      // Scan live text
      {
        val liveTextScanner: LocAndValue[LocationOf.Tag.InReq, ApplicableTagId] => Unit =
          req.live(reqTypes) match {
            case Live =>
              t => t.loc match {
                case loc: Location.Text => b.addTagInLiveText(t.value, loc)
                case _                  => b.addManual(t.value, t.loc, ctx = Live)
              }
            case Dead =>
              t => b.addManual(t.value, t.loc, ctx = Dead)
          }

        for (t <- tagsInText(req.id).live)
          liveTextScanner(t)
      }

      // Scan dead text
      for (t <- tagsInText(req.id).dead)
        b.addManual(t.value, t.loc, ctx = Dead)

      // Scan tag fields
      for (t <- reqTags(req.id))
        b.addManual(t, Location.Tags, ctx = Live)
    }

    // =================================================================================================================
    private def addDefaults(req: Req, b: ForReq): Unit = {
      import FieldReqTypeRules.Resolution

      lazy val effectiveTags: Set[ApplicableTagId] =
        b.manualLive.keySet ++ b.deadTagsInLiveText.keySet ++ b.naTagsInLiveText.keySet

      val rules = fieldRules(req.reqTypeId)

      for (f <- p.config.fields.customTagFields) {
        rules.tag(f.id) match {

          case Resolution.DefaultTo(default) =>
            val relevant = liveTagDist.inField(f.id)
            if (!effectiveTags.exists(relevant.contains))
              b.addDefault(f.id, default, f.live(p.config))

          case Resolution.Mandatory
             | Resolution.Optional
             | Resolution.NotApplicable =>
        }
      }
    }

    // =================================================================================================================
    private def separateConflictingTags(b: ForReq): Unit = {
      val conflicts = Util.uniqueDupsNested(b.manualLive.keyIterator)(exclusiveGroups)
      for (tagGroup <- conflicts) {
        val it = b.manualLive.iterator.filter(x => exclusiveGroups(x._1).contains(tagGroup))
        if (it.nonEmpty) {
          b.conflictingGroups += tagGroup
          for ((tag, locs) <- it) {
            b.manualLive = b.manualLive.delk(tag)
            b.conflictingTags = b.conflictingTags.addvs(tag, locs)
          }
        }
      }
    }

    // =================================================================================================================
    // Derivative tags

    // Notes about factors:
    //
    // - a parent should have at least one factor for each transitive child (it will be used to generate colour bars)
    //
    // - self:default should be included every time it's actually relevant in the calculation.
    //
    // - should never be empty when derivative tags is performed on an accepting node, regardless of the outcome
    //
    // - should always be empty when node is not accepting of derivative tags (eg. dead req, N/A req type, etc...)
    //
    type DerivativeTagFactors = Map[CustomField.Tag.Id, MutableRef[Multimap[ReqId, Set, DerivativeTagFactor]]]

    private final val debugDerivativeTags = false

    private def applyDerivativeTags(data: Data) = {

      var factorsPerField: DerivativeTagFactors = Map.empty

      val fields =
        p.config.liveCustomTagFields
        .iterator
        .filter(_.derivativeTags.enabled is Enabled)
        .map { f =>
          factorsPerField = factorsPerField.updated(f.id, MutableRef(Mutable.emptyDTF))
          val liveValidFieldTags = p.config.liveValidFieldTags(f.id)
          val naReqTypes =
            p.config.reqTypes.all
              .iterator
              .filter(rt => rt.live.is(Dead) || f.fieldReqTypeRules(rt.reqTypeId).isNA)
              .map(_.reqTypeId)
              .toSet
          val derivativeTags = Memo { (reqTypeId: ReqTypeId) =>
            f.derivativeTags.filterRulesByResult(liveValidFieldTags(reqTypeId).contains)
          }
          DerivativeTagField(f.id, derivativeTags, naReqTypes, liveValidFieldTags)
        }
        .to(ArraySeq)

      val graph = imps.forwards

      def getFieldManuals(f: DerivativeTagField,
                          reqTypeId: ReqTypeId,
                          values: IterableOnce[(ApplicableTagId, List[LocationOf.Tag.InReq])]): Map[ApplicableTagId, Set[Provenance.Manual]] =
        values
          .iterator
          .filter(e => f.liveValidTags(reqTypeId).contains(e._1))
          .map(e => (e._1, e._2.iterator.map(Provenance.fromTagLoc).toSet))
          .toMap

      // ---------------------------------------------------------------------------------------------------------------
      // Add parents' influences to children
      def pass1(): Unit = {
        type ParentsInfluence = Set[DerivativeTagFactor]
        type ParentsInfluencesByField = ListMap[CustomField.Tag.Id, ParentsInfluence]
        val emptyInfluence: ParentsInfluence = Set.empty
        val emptyInfluencesByField: ParentsInfluencesByField = ListMap.empty

        def scan(nodeId: ReqId, parentsInfluence: ParentsInfluencesByField): Unit = {
          val node = data(nodeId)
          val reqTypeId = node.req.reqTypeId

          var newInfluences = emptyInfluencesByField

          for (f <- fields) {
            val parentsManuals = parentsInfluence.getOrElse(f.fieldId, emptyInfluence)

            val newParentsInfluenceF: ParentsInfluence =
              if (node.req.live(p.config.reqTypes) is Dead) {
                Set.empty

              } else if (f.naReqTypes.contains(reqTypeId)) {
                parentsManuals

              } else {
                val dt        = f.derivativeTags(reqTypeId)
                val manuals   = getFieldManuals(f, reqTypeId, node.manualLive.m)
                val hasManual = manuals.nonEmpty
                val factors   = factorsPerField(f.fieldId)

                val relevantManuals: Set[DerivativeTagFactor] =
                  if (hasManual) {
                    // Discard parents' manuals and override with our own
                    manuals.iterator.flatMap { case (tag, provenance) =>
                      provenance.iterator.map { p =>
                        DerivativeTagFactor.Relation(nodeId, Backwards, tag, p)
                      }
                    }.toSet

                  } else if (node.liveDefaults.contains(f.fieldId)) {

                    // Combine parents' values with our default and see if parents' values are relevant
                    val d = node.liveDefaults(f.fieldId)
                    val ourDefault = Set1(d)
                    var results = ourDefault
                    parentsManuals.foreach {
                      case DerivativeTagFactor.Relation(_, _, tag, _) if tag !=* d => results += tag
                      case _                                                       =>
                    }
                    val tagFilter = f.liveValidTags(reqTypeId).contains _
                    results = dt.derive(results, tagOrderingByPos, tagFilter, tagFilter)

                    if (debugDerivativeTags) {
                      println(s"(${nodeId.value})               parentsManual: $parentsManuals")
                      println(s"(${nodeId.value})                 ourDefaults: $ourDefault")
                      println(s"(${nodeId.value}) ourDefaultAndParentsManuals: $results")
                    }

                    // Parent's values changed nothing - ignore them
                    if (results ==* ourDefault)
                      Set1(DerivativeTagFactor.Relation(nodeId, Backwards, d, Provenance.Default))
                    else
                      parentsManuals

                  } else
                      parentsManuals

                // Add data from parents
                if (relevantManuals eq parentsManuals)
                  factors.mod(_.addvs(nodeId, parentsManuals))

                relevantManuals
              }

            newInfluences = newInfluences.updated(f.fieldId, newParentsInfluenceF)

          } // each field

          for (child <- graph(nodeId))
            scan(child, newInfluences)

        } // scan

        for (n <- imps.roots)
          scan(n, emptyInfluencesByField)
      }

      // ---------------------------------------------------------------------------------------------------------------
      // Derive tags, and inform parents
      def pass2(): Unit =
        for (f <- fields) {
          val seen    = mutable.Map.empty[ReqId, Set[DerivativeTagFactor]]
          val factors = factorsPerField(f.fieldId)

          def scan(nodeId: ReqId): Set[DerivativeTagFactor] =
            seen.get(nodeId) match {
              case None =>

                seen.update(nodeId, Set.empty)

                val node      = data(nodeId)
                val reqTypeId = node.req.reqTypeId
                val dt        = f.derivativeTags(reqTypeId)

                @inline def descReq = PlainText.pubid(node.req.pubid, p)

                def processChildren(): Set[DerivativeTagFactor] = {
                  var s = Set.empty[DerivativeTagFactor]
                  for (child <- graph(nodeId)) {
                    val results = scan(child)
                    s = Util.mergeSets(s, results)
                  }
                  s
                }

                val finalResultsForThisNode: Set[DerivativeTagFactor] =
                  if (node.req.live(p.config.reqTypes) is Dead) {
                    for (child <- graph(nodeId))
                      scan(child)
                    Set.empty

                  } else if (f.naReqTypes.contains(node.req.reqTypeId)) {
                    val x = processChildren()
                    x

                  } else {
                    val manuals       = getFieldManuals(f, reqTypeId, node.manualLive.m)
                    val badManuals    = getFieldManuals(f, reqTypeId, node.deadTagsInLiveText.iterator ++ node.naTagsInLiveText.iterator)
                    val conflicts     = getFieldManuals(f, reqTypeId, node.conflictingTags.m)
                    val hasManual     = manuals.nonEmpty || badManuals.nonEmpty || conflicts.nonEmpty
                    val default       = node.liveDefaults.get(f.fieldId)
                    val hasDefault    = default.isDefined
                    val liveValidTags = f.liveValidTags(reqTypeId)

                    // Complete all children
                    val addedFromChildren = processChildren()

                    if (debugDerivativeTags) {
                      println("=================================================================")
                      println(s"= ${p.config.tags.needTagGroup(p.config.fields.custom(f.fieldId).tagId).name} -> $nodeId")
                    }

                    var addToParents = addedFromChildren
                    var defaultAddable = false

                    // Add data from children
                    if (addedFromChildren.nonEmpty) {
                      factors.mod(_.addvs(nodeId, addedFromChildren))
                    }

                    // Add data from ourself
                    if (hasManual) {
                      def addManuals(m: Map[ApplicableTagId, Set[Provenance.Manual]])
                                    (f: (ApplicableTagId, Provenance.Manual) => Unit): Unit =
                        for {
                          (t, ps) <- m
                          p <- ps
                        } f(t, p)

                      addManuals(manuals) { (t, p) =>
                        factors.mod(_.add(nodeId, DerivativeTagFactor.Self(t, p)))
                        addToParents += DerivativeTagFactor.Relation(nodeId, Forwards, t, p)
                      }
                      addManuals(badManuals) { (t, p) =>
                        factors.mod(_.add(nodeId, DerivativeTagFactor.Self(t, p)))
                      }
                      addManuals(conflicts) { (t, p) =>
                        factors.mod(_.add(nodeId, DerivativeTagFactor.Self(t, p)))
                      }

                    } else if (hasDefault) {
                      defaultAddable = true
                    } else {
                      factors.mod(_.add(nodeId, DerivativeTagFactor.EmptySelf))
                    }

                    // Derive tags from collected factors
                    var deadDerived = Set.empty[ApplicableTagId]
                    val derivationFilter: ApplicableTagId => Boolean =
                      tagId =>
                        liveValidTags.contains(tagId) || {
                          if (tagLive(tagId) is Dead)
                            deadDerived += tagId
                          false
                        }
                    var liveDerived = Set.empty[ApplicableTagId]
                    def addToLiveDerived(id: ApplicableTagId): Unit =
                      if (liveValidTags.contains(id)) {
                        liveDerived += id
                        if (defaultAddable && !default.contains(id))
                          defaultAddable = false
                      }
                    factors.value(nodeId).foreach {
                      case f: DerivativeTagFactor.Self          => addToLiveDerived(f.tag)
                      case f: DerivativeTagFactor.Relation      => addToLiveDerived(f.tag)
                      case DerivativeTagFactor.EmptySelf
                         | _: DerivativeTagFactor.EmptyRelation =>
                    }
                    liveDerived = dt.derive(liveDerived, tagOrderingByPos, liveValidTags.contains, derivationFilter)

                    // Don't say we've derived manual results
                    liveDerived = liveDerived &~ manuals.keySet
                    liveDerived = liveDerived &~ badManuals.keySet
                    liveDerived = liveDerived &~ conflicts.keySet

                    // Don't say we've derived the default; just use the default
                    if (defaultAddable)
                      for (d <- default)
                        liveDerived -= d

                    // Clean up
                    if (liveDerived.isEmpty) {
                      if (defaultAddable) {
                        for (d <- default) {
                          factors.mod(_.add(nodeId, DerivativeTagFactor.Self(d, Provenance.Default)))
                          addToParents += DerivativeTagFactor.Relation(nodeId, Forwards, d, Provenance.Default)
                        }
                      } else if (manuals.isEmpty) {
                        // Note: Only checking `manuals` here instead of using hasManual because badManuals aren't
                        // propagated to parents.
                        addToParents += DerivativeTagFactor.EmptyRelation(nodeId, Forwards)
                      }
                    } else {
                      for (t <- liveDerived)
                        addToParents += DerivativeTagFactor.Relation(nodeId, Forwards, t, Provenance.Derived)
                      if (hasDefault && !defaultAddable)
                        node.liveDefaults = node.liveDefaults.removed(f.fieldId)
                    }

                    // Add this field's derivation to this req's other derived tags
                    node.liveDerived = addDerivedTags(node.liveDerived, f.fieldId, liveDerived)
                    node.deadDerived = addDerivedTags(node.deadDerived, f.fieldId, deadDerived)

                    if (debugDerivativeTags) {
                      println(s"defaultAddable = $defaultAddable, hasManual = $hasManual, hasDefault = $hasDefault")
                      println(s"addedFromChildren: $addedFromChildren")
                      println(s"newlyDerived: ${liveDerived.map(_.value)}")
                      println(s"node.derived: ${node.liveDerived.keySet.map(_.value)}")
                      println(s"fieldFactors: ${factors.value(nodeId)}")
                      println(s"addToParents: ${addToParents}")
                      println(s"node.liveDefaults: ${node.liveDefaults}")
                      println(s"node.deadDefaults: ${node.deadDefaults}")
                      println(s"node.manualLive: ${node.manualLive.keys.map(_.value).toVector.sorted}")
                      println(s"node.manualDead: ${node.manualDead.keys.map(_.value).toVector.sorted}")
                      println(s"node.manualHiddenNA: ${node.manualHiddenNA.keys.map(_.value).toVector.sorted}")
                      println(s"badManuals: $badManuals")
                    }

                    assert(addToParents.nonEmpty, {
                      val sep = "=" * 100
                      println(sep)
                      println(p.prettyPrintImplicationGraph)
                      println(sep)
                      println(p.config.tags.prettyPrint)
                      println(sep)
                      println(s"req id = ${nodeId}")
                      println(s"defaultAddable = $defaultAddable, hasManual = $hasManual, hasDefault = $hasDefault")
                      println(s"node.liveDefaults: ${node.liveDefaults}")
                      println(s"node.deadDefaults: ${node.deadDefaults}")
                      println(s"node.manualLive: ${node.manualLive.keys.map(_.value).toVector.sorted}")
                      println(s"badManuals: ${badManuals}")
                      println(s"fieldFactors: ${factors.value(nodeId)}")
                      println(s"addedFromChildren: $addedFromChildren")
                      println(sep)
                      s"There need to be factors for $descReq"
                    })

                    addToParents
                  }

                // Done. Save results.
                seen.update(nodeId, finalResultsForThisNode)
                finalResultsForThisNode

              case Some(previousResults) =>
                previousResults
            }

          for (n <- imps.roots)
            scan(n)
        }

      // ---------------------------------------------------------------------------------------------------------------
      if (fields.nonEmpty) {
        pass1()
        pass2()
      }

      (fields, factorsPerField)
    }
  } // class Mutable

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████
  // Finally, calculate results

  private final class MutableVirtualTag(val id: ApplicableTagId,
                                        _derivationDesc: () => Option[DerivationDesc]) extends VirtualTag {

    override def derivationDesc = _derivationDesc()
    override def provenances    = _provenances
    override def live           = _live
    override def validity       = _validity
    override def isManualTag    = _isManualTag
    override def isManualInText = _isManualInText
    override def isDefault      = _isDefault
    override def isDerived      = _isDerived

    var _provenances    = Set.empty[Provenance]
    var _live           = Live: Live
    var _validity       = Valid: Validity
    var _isManualTag    = false
    var _isManualInText = false
    var _isDefault      = false
    var _isDerived      = false

    def markAsDead(): this.type = {
      _live = Dead
      this
    }

    def markAsInvalid(): this.type = {
      _validity = Invalid
      this
    }

    def markAsDefault(): Unit = {
      _provenances += Provenance.Default
      _isDefault = true
    }

    def markAsDerived(): Unit = {
      _provenances += Provenance.Derived
      _isDerived = true
    }

    val markAsManual: Provenance.Manual => Unit = {
      case Provenance.ManualTag    => markAsManualTag()
      case Provenance.ManualInText => markAsManualInText()
    }

    def markAsManualTag(): Unit = {
      _provenances += Provenance.ManualTag
      _isManualTag = true
    }

    def markAsManualInText(): Unit = {
      _provenances += Provenance.ManualInText
      _isManualInText = true
    }
  }

  private type MutableVirtualTagsByField = TagFieldId.Mutable[MutableVirtualTag]

  private class MutableVirtualTags(derivationDesc: ApplicableTagId => TagFieldId => Option[DerivationDesc]) {
    type TagState = MutableVirtualTagsByField
    var state = Map.empty[ApplicableTagId, TagState]

    def tagState(id: ApplicableTagId): TagState =
      state.getOrElse(id, {
        val d = derivationDesc(id)
        val s = new TagFieldId.Mutable(f => new MutableVirtualTag(id, () => d(f)))
        state = state.updated(id, s)
        s
      })

    def modTags(tags: IterableOnce[ApplicableTagId],
                dist: TagFieldDistribution.TagIds,
                m   : MutableVirtualTag => Unit): Unit =
      for (tag <- tags.iterator) {
        val s = tagState(tag)
        val fields = dist.fieldsFor(tag)
        s.modFieldsOrOther(fields, m)
      }

    def addManuals(manuals: Map[ApplicableTagId, List[LocationOf.Tag.InReq]],
                   dist: TagFieldDistribution.TagIds,
                   m   : Provenance.Manual => MutableVirtualTag => Unit): Unit =
      for ((tag, locs) <- manuals) {
        val fields = dist.fieldsFor(tag)
        val s = tagState(tag)
        for (loc <- locs) {
          val p = Provenance.fromTagLoc(loc)
          s.modFieldsOrOther(fields, m(p))
        }
      }
  }

  private def calculateVirtualTagsShared(data: Mutable.ForReq,
                                         dist: TagFieldDistribution.TagIds,
                                         init: () => MutableVirtualTags): MutableVirtualTags = {
    val s = init()
    import s._

    for ((tag, fields) <- data.liveDerived.m)
      tagState(tag).modFields(fields, _.markAsDerived())

    for ((field, tag) <- data.liveDefaults)
      tagState(tag).modField(field, _.markAsDefault())

    addManuals(data.manualLive.m, dist, p => _.markAsManual(p))

    addManuals(data.conflictingTags.m, dist, p => _.markAsInvalid().markAsManual(p))

    modTags(data.deadTagsInLiveText.keyIterator, dist, _.markAsDead().markAsManualInText())

    modTags(data.naTagsInLiveText.keyIterator, dist, _.markAsInvalid().markAsManualInText())

    s
  }

  private def calculateVirtualTagsLive(m: Mutable, data: Mutable.ForReq, init: () => MutableVirtualTags): MutableVirtualTags = {
    val dist = m.liveTagDist
    val s = calculateVirtualTagsShared(data, dist, init)
    s
  }

  private def calculateVirtualTagsDead(m: Mutable, data: Mutable.ForReq, init: () => MutableVirtualTags): MutableVirtualTags = {
    val dist = m.p.config.deadTagFieldDistribution
    val s = calculateVirtualTagsShared(data, dist, init)
    import s._

    addManuals(data.manualDead.m, dist, p => _.markAsDead().markAsManual(p))

    addManuals(data.manualHiddenNA.m, dist, p => _.markAsInvalid().markAsManual(p))

    for ((tag, fields) <- data.deadDerived.m)
      tagState(tag).modFields(fields, _.markAsDead().markAsDerived())

    for ((field, tag) <- data.deadDefaults)
      tagState(tag).modField(field, _.markAsDead().markAsDefault())

    s
  }

  private def describeDerivation(allFactors: Set[DerivativeTagFactor],
                                 self      : Req,
                                 fieldIds  : List[CustomField.Tag.Id],
                                 p         : Project): Option[DerivationDesc] = {
    Option.when(allFactors.nonEmpty) {
      import DerivationDesc._

      val tagCfg        = p.config.tags
      val orderingByPos = tagCfg.orderingByPos

      // Group factors by tag
      var byTag = Multimap.empty[ApplicableTagId, Set, ReqId]
      var noTag = Set.empty[ReqId]
      allFactors.foreach {
        case DerivativeTagFactor.Relation(req, _, tag, _) => byTag = byTag.add(tag, req)
        case DerivativeTagFactor.Self(tag, _)             => byTag = byTag.add(tag, self.id)
        case DerivativeTagFactor.EmptyRelation(req, _)    => noTag += req
        case DerivativeTagFactor.EmptySelf                => noTag += self.id
      }
      val allTags = MutableArray(byTag.keyIterator).sort(orderingByPos).arraySeq

      // Factors
      val factors = ArraySeq.newBuilder[Factor]
      for (reqs <- NonEmptySet.option(noTag)) {
        factors += Factor(None, PlainText.concisePubidSet(reqs, p))
      }
      for (tagId <- allTags.iterator) {
        val reqs = NonEmptySet.force(byTag(tagId))
        factors += Factor(tagId.some, PlainText.concisePubidSet(reqs, p))
      }

      // Derivation steps
      val steps = ArraySeq.newBuilder[DerivStep]
      for (fieldId <- fieldIds) {
        val field     = p.config.fields.custom(fieldId)
        val dt        = field.derivativeTags
        val tagFilter = p.config.liveValidFieldTags(fieldId)(self.reqTypeId).contains _

        @tailrec
        def addDerivationStep(tags: Set[ApplicableTagId]): Unit =
          if (tags.nonEmpty) {
            steps += DerivStep(NonEmptyVector.force(MutableArray(tags).sort(orderingByPos).to(Vector)))
            val next = dt.derive(tags, orderingByPos, tagFilter, tagFilter, recursively = false)
            if (next ne tags)
              addDerivationStep(next)
          }

        addDerivationStep(allTags.iterator.filter(tagFilter).toSet)
      }

      DerivationDesc(factors.result(), steps.result())
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  def apply(p: Project): VirtualProjectTags = {

    val m = new Mutable(p)
    import m.{p => _, _}

    val descDerivMemo: ReqId => ApplicableTagId => TagFieldId => Option[DerivationDesc] =
      if (dtFields.isEmpty)
        _ => _ => _ => None
      else
        Memo { reqId =>
          val req = p.content.reqs.need(reqId)
          Memo { tagId =>

            val relevantFields = dtFields.iterator.filter(_.liveValidTags(req.reqTypeId).contains(tagId)).map(_.fieldId).toList

            var self: TagFieldId => Option[DerivationDesc] = null
            self = Memo {

              case fieldId: CustomField.Tag.Id =>
                if (dtFactors.contains(fieldId)) {
                  val factors = dtFactors(fieldId).value(reqId)
                  describeDerivation(factors, req, fieldId :: Nil, p)
                } else
                  None

              case TagFieldId.Other =>
                None

              case TagFieldId.All =>
                relevantFields match {
                  case Nil =>
                    None

                  case fieldId :: Nil =>
                    self(fieldId)

                  case _ =>
                    val factors = relevantFields.iterator.map(dtFactors(_).value(reqId)).reduce(Util.mergeSets(_, _))
                    describeDerivation(factors, req, relevantFields, p)
                }
            }

            self
          }
        }

    val virtualTagsMemo: FilterDead => ReqId => LazyVal[MutableVirtualTags] =
      FilterDead.memo {
        case HideDead =>
          Memo { reqId =>
            LazyVal {
              val d = data(reqId)
              val descDeriv = descDerivMemo(reqId)
              val init = () => new MutableVirtualTags(descDeriv)
              calculateVirtualTagsLive(m, d, init)
            }
          }
        case ShowDead =>
          Memo { reqId =>
            LazyVal {
              val d = data(reqId)
              val descDeriv = descDerivMemo(reqId)
              val init = () => new MutableVirtualTags(descDeriv)
              calculateVirtualTagsDead(m, d, init)
            }
          }
      }

    val allLiveDeadResults: AllLiveDeadResults =
      distFn =>
        FilterDead.memoLazy { fd =>
          val virtualTagsForReq = virtualTagsMemo(fd)
          val defaultDist       = distFn(fd)

          val defaultsFn: Mutable.ForReq => Map[CustomField.Tag.Id, ApplicableTagId] =
            fd match {
              case HideDead => _.liveDefaults
              case ShowDead => b => Util.mergeDisjointMaps(b.liveDefaults, b.deadDefaults)
            }

          Memo { reqId =>
            def liveDeadResults(dist: TagFieldDistribution.TagIds): ResultsLiveDead =
              new LiveDeadImpl(
                reqId           = reqId,
                dist            = dist,
                virtualTags     = virtualTagsForReq(reqId),
                defaultsFn      = defaultsFn,
                liveDeadResults = liveDeadResults,
                mutableResults  = m,
              )
            liveDeadResults(defaultDist)
          }
        }

    new Impl(p.dataLogic.tagFieldDist, allLiveDeadResults, m)
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private type AllLiveDeadResults = (FilterDead => TagFieldDistribution.TagIds) => FilterDead => ReqId => ResultsLiveDead

  private final class Impl(distFn            : FilterDead => TagFieldDistribution.TagIds,
                           allLiveDeadResults: AllLiveDeadResults,
                           mutableResults    : Mutable) extends VirtualProjectTags {
    import mutableResults._

    private val fdResults = allLiveDeadResults(distFn)

    private val monoResults: ReqId => ResultsMono =
      Memo(reqId => new MonoImpl(
        reqId,
        fdResults(HideDead)(reqId),
        mutableResults))

    override def apply(reqId: ReqId): ResultsMono =
      monoResults(reqId)

    override def apply(reqId: ReqId, filterDead: FilterDead): ResultsLiveDead =
      fdResults(filterDead)(reqId)

    override def underTagGroup(tagGroupId: TagGroupId, filterDead: FilterDead): ReqId => Vector[ApplicableTagId] = {
      val tagLookup = fdResults(filterDead)
      val tagScope  = p.config.tagFieldDistribution(filterDead).inTagGroup(tagGroupId)
      val tagOrder  = p.config.tags.applicableTagOrdering(tagGroupId, filterDead)

      reqId =>
        MutableArray(tagLookup(reqId).set(TagFieldId.All).iterator.filter(tagScope.contains))
          .sort(tagOrder)
          .iterator()
          .toVector
    }

    override def withTagFieldDistFn(distFn2: FilterDead => TagFieldDistribution.TagIds) =
      new Impl(distFn2, allLiveDeadResults, mutableResults)

    @elidable(elidable.INFO)
    override def prettyPrint = {
      val pt = PlainText.ForProject.noCtx(p)
      val sep = "=" * 80

      def show(id: ApplicableTagId): String = {
        val tag = p.config.tags.needApplicableTag(id)
        tag.live match {
          case Live => tag.name
          case Dead => s"${tag.name} (DEAD)"
        }
      }

      def showS(i: Set[ApplicableTagId]): Iterator[String] =
        i.iterator.map(show)

      def showM[A](i: Multimap[ApplicableTagId, List, A]): Iterator[String] =
        i.iterator.map { case (tagId, as) =>
          s"${show(tagId)} found in ${MutableArray(as.map(_.toString)).sort.mkString("[", ", ", "]")}"
        }

      def showD(i: Map[CustomField.Tag.Id, ApplicableTagId]): Iterator[String] =
        i.iterator.map { case (fieldId, tagId) =>
          val field = p.config.fields.custom(fieldId)
          val group = p.config.tags.needTagGroup(field.tagId).name
          val s = s"${show(tagId)} from $group field (#${fieldId.value})"
          field.live(p.config) match {
            case Live => s
            case Dead => s + " (DEAD)"
          }
        }

      p.content.reqs.reqIterator().map { req =>
        val b = data(req.id)
        var title = s"${PlainText.pubid(req.pubid, p)}: ${pt.reqTitle(req)}"
        if (req.live(p.config.reqTypes) is Dead)
          title += " (DEAD)"
        var items = Vector.empty[String]
        items ++= showM(b.manualLive        ).map("manualLive         : " + _)
        items ++= showM(b.manualDead        ).map("manualDead         : " + _)
        items ++= showM(b.deadTagsInLiveText).map("deadTagsInLiveText : " + _)
        items ++= showM(b.naTagsInLiveText  ).map("naTagsInLiveText   : " + _)
        items ++= showD(b.liveDefaults      ).map("liveDefaults       : " + _)
        items ++= showD(b.deadDefaults      ).map("deadDefaults       : " + _)
        items ++= showS(b.liveDerived.keySet).map("liveDerived        : " + _)
        items ++= showS(b.deadDerived.keySet).map("deadDerived        : " + _)
        val detail = items.sorted.map("  - " + _).mkString("\n")
        if (detail.isEmpty)
          title
        else
          s"$title\n$detail"
      }
        .toVector
        .sorted
        .mkString(sep + "\n", "\n", "\n" + sep)
    }
  }

  // ===================================================================================================================

  private final class MonoImpl(reqId         : ReqId,
                               liveResults   : ResultsLiveDead,
                               mutableResults: Mutable) extends ResultsMono {
    import mutableResults._

    private val b = data(reqId)

    override def conflictingTagGroups =
      b.conflictingGroups

    override def conflictingTags =
      b.conflictingTags

    override def naTagsInLiveText =
      b.naTagsInLiveText

    override def derivativeTagFactors(f: CustomField.Tag.Id) =
      if (dtFactors.contains(f))
        dtFactors(f).value(reqId)
      else
        Set.empty

    private val childrenSummaries: CustomField.Tag.Id => ChildrenSummary =
      Memo(new ChildrenSummaryImpl(reqId, _, liveResults, mutableResults))

    override def childrenSummary(field: CustomField.Tag.Id) =
      childrenSummaries(field)
  }

  // ===================================================================================================================

  private final val debugProgressBar = false

  private final class ChildrenSummaryImpl(selfId        : ReqId,
                                          fieldId       : CustomField.Tag.Id,
                                          liveResults   : ResultsLiveDead,
                                          mutableResults: Mutable) extends ChildrenSummary {

    import mutableResults._

    private lazy val results: (Int, ProgressBar) =
      if (dtFactors.contains(fieldId)) {
        val allFactors = dtFactors(fieldId)
        val tags       = p.config.tags
        def selfLive   = p.content.reqs.need(selfId).live(p.config.reqTypes)

        // Group by reqId
        val _byReq = mutable.HashMap.empty[ReqId, MutableRef[List[ApplicableTagId]]]

        {
          def add(reqId: ReqId, tag: ApplicableTagId): Unit =
            _byReq.getOrElseUpdate(reqId, MutableRef(Nil)).mod(tag :: _)

          // Scan DT factors
          allFactors.value(selfId).foreach {
            case DerivativeTagFactor.EmptyRelation(req, Forwards)    => add(req, null)
            case DerivativeTagFactor.Relation(req, Forwards, tag, _) => add(req, tag)
            case DerivativeTagFactor.Self(tag, _: Provenance.Manual) => add(selfId, tag)
            case _                                                   =>
          }

          // If no manual tags added for self, add virtual results
          if (!_byReq.contains(selfId) && selfLive.is(Live)) {
            val selfTags = liveResults.set(fieldId)
            if (selfTags.isEmpty)
              add(selfId, null)
            else
              selfTags.foreach(add(selfId, _))
          }
        }

        // Debugging
        if (debugProgressBar) {
          val sep = "=" * 80
          println(sep)
          println(s"Progress bar components for ${PlainText.pubidByReqId(selfId, p)}")
          def descTag(id: ApplicableTagId) = Option(id).fold("∅")(tags.needApplicableTag(_).name)
          val rows = _byReq.keysIterator.toList.map { k =>
            val v = _byReq(k)
            val pubid = PlainText.pubidByReqId(k, p)
            s"  - $pubid: ${v.value.map(descTag).sorted.mkString(",")}"
          }
          rows.sorted.foreach(println)
          println(sep)
        }

        // Aggregate
        val _byTag = mutable.HashMap.empty[ApplicableTagId, MutableRef[Double]]
        var noTag = 0d
        var total = 0
        for (ref <- _byReq.valuesIterator) {
          total += 1
          val tags = ref.value
          val length = tags.length
          val weight = if (length > 1) 1d / length.toDouble else 1d
          tags foreach {
            case null => noTag += weight
            case t => _byTag.getOrElseUpdate(t, MutableRef.double()).mod(_ + weight)
          }
        }

        // Reorganise results
        val _progressBar = Array.newBuilder[ProgressBarPortion]
        for ((k, v) <- _byTag) {
          val tagId = k.some
          val tag = Some(tags.needApplicableTag(k))
          _progressBar += ProgressBarPortion(tagId, tag, v.value, total)
        }
        if (noTag > 0d) {
          _progressBar += ProgressBarPortion(None, None, noTag, total)
        }

        // Sort progress bar
        val progressBarArray = _progressBar.result()
        val progressBarOrder = Ordering.by((_: ProgressBarPortion).tagId)(tags.orderingByPosEmptyFirst)
        scala.util.Sorting.quickSort(progressBarArray)(progressBarOrder)
        val progressBar = ArraySeq.unsafeWrapArray(progressBarArray)

        (total, progressBar)

      } else
        (0, ArraySeq.empty)

    override def total       = results._1
    override def progressBar = results._2
  }

  // ===================================================================================================================

  private final class LiveDeadImpl(reqId          : ReqId,
                                   dist           : TagFieldDistribution.TagIds,
                                   virtualTags    : LazyVal[MutableVirtualTags],
                                   defaultsFn     : Mutable.ForReq => Map[CustomField.Tag.Id, ApplicableTagId],
                                   liveDeadResults: TagFieldDistribution.TagIds => ResultsLiveDead,
                                   mutableResults : Mutable) extends ResultsLiveDead {
    import mutableResults._

    private val b = data(reqId)

    override def isEmpty =
      virtualTags.value.state.isEmpty

    override def apply(id: ApplicableTagId, f: TagFieldId) = {
      val ts = virtualTags.value.state.getOrElse(id, null)
      if (ts eq null)
        new VirtualTag.Empty(id)
      else
        ts.get(f)
    }

    @inline private def allSet =
      virtualTags.value.state.keySet

    private lazy val otherSet =
      allSet &~ dist.usedInFields

    private val fieldSet = Memo { (fid: CustomField.Tag.Id) =>
      allSet & dist.inField(fid)
    }

    override val set = {
      case f: CustomField.Tag.Id => fieldSet(f)
      case TagFieldId.All        => allSet
      case TagFieldId.Other      => otherSet
    }

    private lazy val allOrdered =
      MutableArray(allSet).sortBy(tagOrderByName.apply).to(Vector)

    private def otherOrdered =
      MutableArray(otherSet).sortBy(tagOrderByName.apply).to(Vector)

    private val fieldOrdered = Memo { (fid: CustomField.Tag.Id) =>
      MutableArray(fieldSet(fid)).sortBy(tagOrderByPos.apply).to(Vector)
    }

    override val ordered = {
      case f: CustomField.Tag.Id => fieldOrdered(f)
      case TagFieldId.All        => allOrdered
      case TagFieldId.Other      => otherOrdered
    }

    override def defaults =
      defaultsFn(b)

    override def withTagFieldDist(dist2: TagFieldDistribution.TagIds) =
      if (dist eq dist2)
        this
      else
        liveDeadResults(dist2)
  }
}
