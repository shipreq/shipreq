package shipreq.webapp.base.data.derivation

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.Memo
import nyaya.util.Multimap
import scala.collection.immutable.ListMap
import scala.collection.mutable
import shipreq.base.util._
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText

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
    def manualLiveValues: Multimap[ApplicableTagId, List, LocationOf.Tag.InReq]
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
  }

  sealed trait VirtualTag {
    def provenances   : Set[Provenance]
    def live          : Live
    def validity      : Validity
    def isManualTag   : Boolean
    def isManualInText: Boolean
    def isDefault     : Boolean
    def isDerived     : Boolean

    @inline final def isDead = live is Dead
  }

  object VirtualTag {
    val empty: VirtualTag =
      new VirtualTag {
        override def provenances    = Set.empty
        override def live           = Live
        override def validity       = Valid
        override def isManualTag    = false
        override def isManualInText = false
        override def isDefault      = false
        override def isDerived      = false
      }
  }

  sealed trait ChildrenSummary {
    def aggregated: Map[Option[ApplicableTagId], Double]

    /** The number of live, relevant, unique, transitive children.
      * Also the sum of all aggregated values.
      */
    def total: Int
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private object Mutable {

    type DerivedTags = Multimap[ApplicableTagId, Set, CustomField.Tag.Id]
    val emptyDerivedTags: DerivedTags = Multimap.empty

    def addDerivedTags(prev: DerivedTags, field: CustomField.Tag.Id, tags: Set[ApplicableTagId]): DerivedTags =
      tags.foldLeft(prev)(_.add(_, field))

    val emptyDTF = Multimap.empty[ReqId, Set, DerivativeTagFactor]

    object ForReq {
      val emptyInReq  = Multimap.empty[ApplicableTagId, List, LocationOf.Tag.InReq]
      val emptyInText = Multimap.empty[ApplicableTagId, List, Location.Text]
    }

    final class ForReq(val req: Req,
                       val reqLive: Live,
                       nonApplicableTags: Set[ApplicableTagId],
                       tagLive: ApplicableTagId => Live) {
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
                                        derivativeTags: DerivativeTags,
                                        naReqTypes    : Set[ReqTypeId],
                                        tags          : Set[ApplicableTagId])
  }

  private final class Mutable(val p: Project) {
    import Mutable._

    val deadTags       = p.config.tags.deadApplicableTagIds
    val tagsInText     = p.atomScan.tagRefs
    val tagLive        = (id: ApplicableTagId) => Dead when deadTags.contains(id)
    val reqTags        = p.content.reqTags
    val reqTypes       = p.config.reqTypes
    val fieldRules     = p.config.fieldRules(ShowDead) // ShowDead so that it doesn't replace dead defaults with Optional
    val liveTagDist    = p.config.liveTagFieldDistribution
    val imps           = p.content.implications
    val tagOrderByName = p.config.tags.orderByName
    val tagOrderByPos  = p.config.tags.orderByPos

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
    }

    // Step 2: Derivative tags
    val dtFactors: DerivativeTagFactors =
      applyDerivativeTags(data)

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

    private def applyDerivativeTags(data: Data): DerivativeTagFactors = {

      var factorsPerField: DerivativeTagFactors = Map.empty

      val fields =
        p.config.liveCustomTagFields
        .iterator
        .filter(_.derivativeTags.enabled is Enabled)
        .map { f =>
          factorsPerField = factorsPerField.updated(f.id, MutableRef(Mutable.emptyDTF))
          val scope = liveTagDist.inField(f.id)
          val naReqTypes =
            p.config.reqTypes.all
              .iterator
              .filter(rt => rt.live.is(Dead) || f.fieldReqTypeRules(rt.reqTypeId).isNA)
              .map(_.reqTypeId)
              .toSet
          val derivativeTags = f.derivativeTags.filterRulesByResult(scope.contains)
          DerivativeTagField(f.id, derivativeTags, naReqTypes, scope)
        }
        .to(ArraySeq)

      val graph = imps.forwards

      def getFieldManuals(node: ForReq, f: DerivativeTagField): Map[ApplicableTagId, Set[Provenance.Manual]] =
        node.manualLive
          .iterator
          .filter(e => f.tags.contains(e._1))
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

          var newInfluences = emptyInfluencesByField

          for (f <- fields) {
            val parentsManuals = parentsInfluence.getOrElse(f.fieldId, emptyInfluence)

            val newParentsInfluenceF: ParentsInfluence =
              if (node.req.live(p.config.reqTypes) is Dead) {
                Set.empty

              } else if (f.naReqTypes.contains(node.req.reqTypeId)) {
                parentsManuals

              } else {
                val dt        = f.derivativeTags
                val manuals   = getFieldManuals(node, f)
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
                      case DerivativeTagFactor.Relation(_, _, tag, _) if tag !=* d =>
                        results = dt.add(results, tag)
                      case _ =>
                    }

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
          val dt      = f.derivativeTags

          def scan(nodeId: ReqId): Set[DerivativeTagFactor] =
            seen.get(nodeId) match {
              case None =>

                seen.update(nodeId, Set.empty)
                val node = data(nodeId)
                @inline def descReq = PlainText.pubid(node.req.pubid, p)

                def processChildren(): Set[DerivativeTagFactor] = {
                  var s = Set.empty[DerivativeTagFactor]
                  for (child <- graph(nodeId)) {
                    val results = scan(child)
                    s = Util.mergeSets(s, results)
                  }
                  s
                }

                if (node.req.live(p.config.reqTypes) is Dead) {
                  for (child <- graph(nodeId))
                    scan(child)
                  Set.empty

                } else if (f.naReqTypes.contains(node.req.reqTypeId)) {
                  processChildren()

                } else {
                  val manuals    = getFieldManuals(node, f)
                  val badManuals = (node.deadTagsInLiveText.keyIterator ++ node.naTagsInLiveText.keyIterator).filter(f.tags.contains).toSet
                  val hasManual  = manuals.nonEmpty || badManuals.nonEmpty
                  val default    = node.liveDefaults.get(f.fieldId)
                  val hasDefault = default.isDefined

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
                    for {
                      (t, provenance) <- manuals
                      p               <- provenance
                    } {
                      factors.mod(_.add(nodeId, DerivativeTagFactor.Self(t, p)))
                      addToParents += DerivativeTagFactor.Relation(nodeId, Forwards, t, p)
                    }
                    for (tag <- badManuals)
                      factors.mod(_.add(nodeId, DerivativeTagFactor.Self(tag, Provenance.ManualInText)))
                  } else if (hasDefault) {
                    defaultAddable = true
                  } else {
                    factors.mod(_.add(nodeId, DerivativeTagFactor.EmptySelf))
                  }

                  // Derive tags from collected factors
                  var deadDerived = Set.empty[ApplicableTagId]
                  val derivationFilter: ApplicableTagId => Boolean =
                    tagId => tagLive(tagId) match {
                      case Live => true
                      case Dead => deadDerived += tagId; false
                    }
                  var liveDerived = {
                    def addDerive(tags: Set[ApplicableTagId], id: ApplicableTagId): Set[ApplicableTagId] = {
                      if (defaultAddable && !default.contains(id))
                        defaultAddable = false
                      dt.add(tags, id, derivationFilter)
                    }
                    factors.value(nodeId).foldLeft(Set.empty[ApplicableTagId]) { (tags, fac) =>
                      fac match {
                        case f: DerivativeTagFactor.Self          => addDerive(tags, f.tag)
                        case f: DerivativeTagFactor.Relation      => addDerive(tags, f.tag)
                        case DerivativeTagFactor.EmptySelf
                           | _: DerivativeTagFactor.EmptyRelation => tags
                      }
                    }
                  }

                  // Don't say we've derived manual results
                  liveDerived = liveDerived &~ manuals.keySet
                  liveDerived = liveDerived &~ badManuals

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
                  seen.update(nodeId, addToParents)
                  addToParents
                }

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

      factorsPerField
    }
  } // class Mutable

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████
  // Finally, calculate results

  private class MutableVirtualTag extends VirtualTag {
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

  private class MutableVirtualTags {
    type TagState = MutableVirtualTagsByField
    var state = Map.empty[ApplicableTagId, TagState]

    def tagState(id: ApplicableTagId): TagState =
      state.getOrElse(id, {
        val s = new TagFieldId.Mutable(new MutableVirtualTag)
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

  private def calculateVirtualTagsShared(data: Mutable.ForReq, dist: TagFieldDistribution.TagIds): MutableVirtualTags = {
    val s = new MutableVirtualTags
    import s._

    for ((tag, fields) <- data.liveDerived.m)
      tagState(tag).modFields(fields, _.markAsDerived())

    for ((field, tag) <- data.liveDefaults)
      tagState(tag).modField(field, _.markAsDefault())

    addManuals(data.manualLive.m, dist, p => _.markAsManual(p))

    modTags(data.deadTagsInLiveText.keyIterator, dist, _.markAsDead().markAsManualInText())

    modTags(data.naTagsInLiveText.keyIterator, dist, _.markAsInvalid().markAsManualInText())

    s
  }

  private def calculateVirtualTagsLive(m: Mutable, data: Mutable.ForReq): MutableVirtualTags = {
    val dist = m.liveTagDist
    val s = calculateVirtualTagsShared(data, dist)
    s
  }

  private def calculateVirtualTagsDead(m: Mutable, data: Mutable.ForReq): MutableVirtualTags = {
    val dist = m.p.config.deadTagFieldDistribution
    val s = calculateVirtualTagsShared(data, dist)
    import s._

    addManuals(data.manualDead.m, dist, p => _.markAsDead().markAsManual(p))

    addManuals(data.manualHiddenNA.m, dist, p => _.markAsInvalid().markAsManual(p))

    for ((tag, fields) <- data.deadDerived.m)
      tagState(tag).modFields(fields, _.markAsDead().markAsDerived())

    for ((field, tag) <- data.deadDefaults)
      tagState(tag).modField(field, _.markAsDead().markAsDefault())

    s
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  def apply(p: Project): VirtualProjectTags = {

    val m = new Mutable(p)
    import m.data

    val virtualTagsMemo: FilterDead => ReqId => LazyVal[MutableVirtualTags] =
      FilterDead.memo {
        case HideDead =>
          Memo { reqId =>
            LazyVal {
              val d = data(reqId)
              calculateVirtualTagsLive(m, d)
            }
          }
        case ShowDead =>
          Memo { reqId =>
            LazyVal {
              val d = data(reqId)
              calculateVirtualTagsDead(m, d)
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
              case ShowDead => b => Util.mergeMaps(b.liveDefaults, b.deadDefaults)
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
      Memo(new MonoImpl(_, mutableResults))

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

  private final class MonoImpl(reqId: ReqId, mutableResults: Mutable) extends ResultsMono {
    import mutableResults._

    private val b = data(reqId)

    override def manualLiveValues =
      b.manualLive

    override def naTagsInLiveText =
      b.naTagsInLiveText

    override def derivativeTagFactors(f: CustomField.Tag.Id) =
      if (dtFactors.contains(f))
        dtFactors(f).value(reqId)
      else
        Set.empty

    private val childrenSummaries: CustomField.Tag.Id => ChildrenSummary =
      Memo(new ChildrenSummaryImpl(reqId, _, mutableResults))

    override def childrenSummary(field: CustomField.Tag.Id) =
      childrenSummaries(field)
  }

  // ===================================================================================================================

  private final class ChildrenSummaryImpl(reqId: ReqId,
                                          fieldId: CustomField.Tag.Id,
                                          mutableResults: Mutable) extends ChildrenSummary {

    import mutableResults._

    private lazy val _aggregated: (Map[Option[ApplicableTagId], Double], Int) =
      if (dtFactors.contains(fieldId)) {
        val allFactors = dtFactors(fieldId)

        // Group by reqId
        val byReq = mutable.HashMap.empty[ReqId, MutableRef[List[ApplicableTagId]]]

        def add(reqId: ReqId, tag: ApplicableTagId): Unit =
          byReq.getOrElseUpdate(reqId, MutableRef(Nil)).mod(tag :: _)

        allFactors.value(reqId).foreach {
          case DerivativeTagFactor.EmptyRelation(req, Forwards) => add(req, null)
          case DerivativeTagFactor.Relation(req, Forwards, tag, _) => add(req, tag)
          case _ => ()
        }

        // Aggregate by tag
        val agg = mutable.HashMap.empty[ApplicableTagId, MutableRef[Double]]
        var noTag = 0d
        var total = 0
        for (ref <- byReq.valuesIterator) {
          total += 1
          val tags = ref.value
          val length = tags.length
          val weight = if (length > 1) 1d / length.toDouble else 1d
          tags foreach {
            case null => noTag += weight
            case t => agg.getOrElseUpdate(t, MutableRef.double()).mod(_ + weight)
          }
        }

        // Reorganise results
        var results = Map.empty[Option[ApplicableTagId], Double]
        results = agg.foldLeft(results) { case (q, (k, v)) => q.updated(Some(k), v.value) }
        if (noTag > 0d)
          results = results.updated(None, noTag)

        (results, total)

      } else
        (Map.empty, 0)

    override def aggregated = _aggregated._1
    override def total      = _aggregated._2
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
        VirtualTag.empty
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
      case TagFieldId.Custom(f) => fieldSet(f)
      case TagFieldId.All       => allSet
      case TagFieldId.Other     => otherSet
    }

    private lazy val allOrdered =
      MutableArray(allSet).sortBy(tagOrderByName.apply).to(Vector)

    private def otherOrdered =
      MutableArray(otherSet).sortBy(tagOrderByName.apply).to(Vector)

    private val fieldOrdered = Memo { (fid: CustomField.Tag.Id) =>
      MutableArray(fieldSet(fid)).sortBy(tagOrderByPos.apply).to(Vector)
    }

    override val ordered = {
      case TagFieldId.Custom(f) => fieldOrdered(f)
      case TagFieldId.All       => allOrdered
      case TagFieldId.Other     => otherOrdered
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
