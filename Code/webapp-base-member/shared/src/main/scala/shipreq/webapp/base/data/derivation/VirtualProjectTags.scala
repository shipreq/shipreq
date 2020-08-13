package shipreq.webapp.base.data.derivation

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.Memo
import nyaya.util.Multimap
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

  def apply(p: Project): VirtualProjectTags =
    new Mutable(p).result

  /** Results that aren't indexed by anything. */
  sealed trait ResultsMono {
    def manualLiveValues: Multimap[ApplicableTagId, List, LocationOf.Tag.InReq]
    def naTagsInLiveText: Multimap[ApplicableTagId, List, Location.Text]
    def derivativeTagFactors(field: CustomField.Tag.Id): Set[DerivativeTagFactor]
  }

  /** Results indexed by FilterDead. */
  sealed trait ResultsLiveDead {
    val fieldSet: CustomField.Tag.Id => Set[ApplicableTagId]
    def allSet  : Set[ApplicableTagId]
    def otherSet: Set[ApplicableTagId]

    val fieldOrdered: CustomField.Tag.Id => Vector[ApplicableTagId]
    def allOrdered  : Vector[ApplicableTagId]
    def otherOrdered: Vector[ApplicableTagId]

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
                          sourceType: SourceType.NonDerived) extends DerivativeTagFactor

    final case class Relation(sourceReq : ReqId,
                              dir       : Direction,
                              tag       : ApplicableTagId,
                              sourceType: SourceType) extends DerivativeTagFactor

    sealed trait SourceType
    object SourceType {
      sealed trait NonDerived extends SourceType
      case object Manual  extends NonDerived
      case object Default extends NonDerived
      case object Derived extends SourceType

      implicit def univEqN: UnivEq[NonDerived] = UnivEq.derive
      implicit def univEq: UnivEq[SourceType] = UnivEq.derive
    }

    implicit def univEq: UnivEq[DerivativeTagFactor] = UnivEq.derive
  }

  // ===================================================================================================================

  private object Mutable {

    val emptyDTF = Multimap.empty[ReqId, Set, DerivativeTagFactor]

    object ForReq {
      val emptyInReq  = Multimap.empty[ApplicableTagId, List, LocationOf.Tag.InReq]
      val emptyInText = Multimap.empty[ApplicableTagId, List, Location.Text]
    }

    final class ForReq(nonApplicableTags: Set[ApplicableTagId], tagLive: ApplicableTagId => Live) {
      var manualLive          = ForReq.emptyInReq
      var manualDead          = ForReq.emptyInReq
      var deadTagsInLiveText  = ForReq.emptyInText
      var naTagsInLiveText    = ForReq.emptyInText
      var liveDefaults        = Map.empty[CustomField.Tag.Id, ApplicableTagId]
      var deadDefaults        = Map.empty[CustomField.Tag.Id, ApplicableTagId]
      var derived             = Set.empty[ApplicableTagId]

      def addManual(id     : ApplicableTagId,
                    loc    : LocationOf.Tag.InReq,
                    ctx    : Live,
                    allowNA: Boolean = false): Unit =
        if (allowNA || !nonApplicableTags.contains(id))
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

      def addDefault(f: CustomField.Tag.Id, id: ApplicableTagId): Unit =
        tagLive(id) match {
          case Live => liveDefaults = liveDefaults.updated(f, id)
          case Dead => deadDefaults = deadDefaults.updated(f, id)
        }
    }

    final case class DerivativeTagField(fieldId       : CustomField.Tag.Id,
                                        derivativeTags: DerivativeTags,
                                        liveTags      : Set[ApplicableTagId])
  }

  private final class Mutable(p: Project) {
    import Mutable._

    val deadTags    = p.config.tags.deadApplicableTagIds
    val tagsInText  = p.atomScan.tagRefs
    val tagLive     = (id: ApplicableTagId) => Dead when deadTags.contains(id)
    val reqTags     = p.content.reqTags
    val reqTypes    = p.config.reqTypes
    val fieldRules  = p.config.fieldRules(ShowDead) // ShowDead so that it doesn't replace dead defaults with Optional
    val liveTagDist = p.config.liveTagFieldDistribution
    val imps        = p.content.implications

    // -----------------------------------------------------------------------------------------------------------------
    def collectManualTags(req: Req, b: ForReq): Unit = {

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
        b.addManual(t.value, t.loc, ctx = Dead, allowNA = true)

      // Scan tag fields
      for (t <- reqTags(req.id))
        b.addManual(t, Location.Tags, ctx = Live)
    }

    // -----------------------------------------------------------------------------------------------------------------
    def addDefaults(req: Req, b: ForReq): Unit = {
      import FieldReqTypeRules.Resolution

      lazy val effectiveTags: Set[ApplicableTagId] =
        b.manualLive.keySet ++ b.deadTagsInLiveText.keySet ++ b.naTagsInLiveText.keySet

      for (f <- p.config.liveCustomTagFields) {
        fieldRules(req.reqTypeId).tag(f.id) match {

          case Resolution.DefaultTo(default) =>
            val relevant = liveTagDist.inField(f.id)
            if (!effectiveTags.exists(relevant.contains))
              b.addDefault(f.id, default)

          case Resolution.Mandatory
             | Resolution.Optional
             | Resolution.NotApplicable =>
        }
      }
    }

    // -----------------------------------------------------------------------------------------------------------------
    type DerivativeTagFactors = Map[CustomField.Tag.Id, MutableRef[Multimap[ReqId, Set, DerivativeTagFactor]]]

    private final val debugDerivativeTags = false

    def applyDerivativeTags(data: Data): DerivativeTagFactors = {
      import DerivativeTagFactor.SourceType

      var factors: DerivativeTagFactors = Map.empty

      val fields =
        p.config.liveCustomTagFields
        .iterator
        .filter(_.derivativeTags.enabled is Enabled)
        .map(f => DerivativeTagField(f.id, f.derivativeTags, liveTagDist.inField(f.id)))
        .tapEach(f => factors = factors.updated(f.fieldId, MutableRef(Mutable.emptyDTF)))
        .to(ArraySeq)

        val graph = imps.forwards
        @inline def relDir = Forwards

        for (f <- fields) {
          val seen = mutable.Map.empty[ReqId, Set[DerivativeTagFactor]]
          val fieldFactors = factors(f.fieldId)
          val dt = f.derivativeTags

          def scan(nodeId: ReqId,
                   parentsManual: Set[DerivativeTagFactor],
//                   parentsDefaults: Set[DerivativeTagFactor.Relation], // TODO pass down parent defaults later
                  ): Set[DerivativeTagFactor] =

            seen.get(nodeId) match {
              case None =>

                seen.update(nodeId, null)
                val node = data(nodeId)

                val theseManual = node.manualLive.keyIterator.filter(f.liveTags.contains).toSet
                val hasManual = theseManual.nonEmpty
                val hasDefault = node.liveDefaults.contains(f.fieldId)

                val newParentsManual: Set[DerivativeTagFactor] =
                  if (hasManual)
                    theseManual.map(DerivativeTagFactor.Relation(nodeId, Backwards, _, SourceType.Manual))
                  else if (hasDefault) {
                    val default = node.liveDefaults(f.fieldId)
                    val ourDefaults: Set[ApplicableTagId] =
                      Set1(default)
                    var ourDefaultAndParentsManuals: Set[ApplicableTagId] =
                      ourDefaults

                    parentsManual.foreach {
                      case DerivativeTagFactor.Relation(_, _, tag, _) if tag !=* default =>
                        ourDefaultAndParentsManuals = dt.combineOne(ourDefaultAndParentsManuals, tag)
                      case _ =>
                    }

                    if (debugDerivativeTags) {
                      println(s"(${nodeId.value})               parentsManual: $parentsManual")
                      println(s"(${nodeId.value})                 ourDefaults: $ourDefaults")
                      println(s"(${nodeId.value}) ourDefaultAndParentsManuals: $ourDefaultAndParentsManuals")
                    }
                    if (ourDefaultAndParentsManuals ==* ourDefaults)
                      Set.empty // TODO LAter Set1(DerivativeTagFactor.Relation(nodeId, Backwards, default, SourceType.Default))
                    else
                      parentsManual

                  } else
                    parentsManual

                // Complete all children
                val addedFromChildren = {
                  var s = Set.empty[DerivativeTagFactor]
                  for (child <- graph(nodeId)) {
                    val results = scan(child, newParentsManual)
                    s = Util.mergeSets(s, results)
                  }
                  s
                }

                if (debugDerivativeTags) {
                  println("=================================================================")
                  println(s"= ${p.config.tags.needTagGroup(p.config.fields.custom(f.fieldId).tagId).name} -> $nodeId")
                }

                // TODO AFTER THIS IS WORKING, NEED TO FILTER manualLive BY TAG GROUP
                var addToParents = addedFromChildren
                var defaultAddable = false

                if (newParentsManual eq parentsManual)
                  fieldFactors.mod(_.addvs(nodeId, parentsManual))

                if (addedFromChildren.nonEmpty) {
                   fieldFactors.mod(_.addvs(nodeId, addedFromChildren))
                }

                (hasManual, hasDefault) match {
                  case (true, _) =>
                    for (t <- theseManual) {
                      fieldFactors.mod(_.add(nodeId, DerivativeTagFactor.Self(t, SourceType.Manual)))
                      addToParents += DerivativeTagFactor.Relation(nodeId, relDir, t, SourceType.Manual)
                    }

                  case (false, true) =>
                    for (default <- node.liveDefaults.get(f.fieldId)) {

                      defaultAddable = true
                    }

                  case (false, false) =>
                    fieldFactors.mod(_.add(nodeId, DerivativeTagFactor.EmptySelf))
//                    addToParents += DerivativeTagFactor.EmptyRelation(nodeId, relDir)
                }

                // Derive final results for this node
                // TODO AFTER THIS IS WORKING, SCOPE DERIVED BY FIELD

                var nonDefaultEncountered = false

                def addDerive(tags: Set[ApplicableTagId], id: ApplicableTagId): Set[ApplicableTagId] = {
                  for (default <- node.liveDefaults.get(f.fieldId)) {
                    if (id != default)
                      nonDefaultEncountered = true
                  }
                  dt.combineOne(tags, id)
                }

                var newlyDerived =
                  fieldFactors.value(nodeId).foldLeft(Set.empty[ApplicableTagId]) { (tags, fac) =>
                    fac match {
                      case DerivativeTagFactor.EmptySelf
                         | _: DerivativeTagFactor.EmptyRelation => tags
                      case f: DerivativeTagFactor.Self          => addDerive(tags, f.tag)
                      case f: DerivativeTagFactor.Relation      => addDerive(tags, f.tag)
                    }
                  }
                val derived1 = newlyDerived
                newlyDerived --= node.manualLive.keys
                if (defaultAddable & nonDefaultEncountered)
                  defaultAddable = false

                if (defaultAddable)
                  newlyDerived --= node.liveDefaults.get(f.fieldId)

                if (newlyDerived.isEmpty) {
                  if (defaultAddable)
                    for (default <- node.liveDefaults.get(f.fieldId)) {
                      fieldFactors.mod(_.add(nodeId, DerivativeTagFactor.Self(default, SourceType.Default)))
                      addToParents += DerivativeTagFactor.Relation(nodeId, relDir, default, SourceType.Default)
                    }
                  if (!hasManual && !defaultAddable)
                    addToParents += DerivativeTagFactor.EmptyRelation(nodeId, relDir)
                } else {
                  for (t <- newlyDerived)
                    addToParents += DerivativeTagFactor.Relation(nodeId, relDir, t, SourceType.Derived)
                  if (hasDefault && !defaultAddable)
                    node.liveDefaults = node.liveDefaults.removed(f.fieldId)
                }

                node.derived =
                  Util.mergeSets(node.derived, newlyDerived)

                if (debugDerivativeTags) {
                  println(s"nonDefaultEncountered: $nonDefaultEncountered, defaultAddable = $defaultAddable, hasManual = $hasManual, hasDefault = $hasDefault")
                  println(s"addedFromChildren: $addedFromChildren")
                  println(s"derived1: ${derived1 .map(_.value)}")
                  println(s"derivedN: ${newlyDerived.map(_.value)}")
                  println(s"derived: ${node.derived.map(_.value)}")
                  println(s"fieldFactors: ${fieldFactors.value(nodeId)}")
                  println(s"addToParents: ${addToParents}")
                  println(s"node.liveDefaults: ${node.liveDefaults}")
                  println(s"node.manualLive: ${node.manualLive.keys.map(_.value).toVector.sorted}")
                }

                seen.update(nodeId, addToParents)
                addToParents

              case Some(results) => results
            }

          for (n <- imps.roots)
            scan(n, Set.empty)
        }

      factors
    }

    // -----------------------------------------------------------------------------------------------------------------

    type Data = mutable.HashMap[ReqId, ForReq]

    val result: VirtualProjectTags = {
      val data: Data = mutable.HashMap.empty

      // Step 1: Scan all requirements
      for (req <- p.content.reqs.reqIterator()) {
        val nonApplicableTags = p.config.naTags(req.reqTypeId).set
        val b = new ForReq(nonApplicableTags, tagLive)
        data.update(req.id, b)

        // Process current req
        collectManualTags(req, b)
        addDefaults(req, b)
      }

      // Step 2: Derivative tags
      val dtFactors = applyDerivativeTags(data)

      val monoResults: ReqId => ResultsMono =
        Memo { reqId =>
          val b = data(reqId)
          new ResultsMono {
            override def manualLiveValues =
              b.manualLive

            override def naTagsInLiveText =
              b.naTagsInLiveText

            override def derivativeTagFactors(f: CustomField.Tag.Id) =
              dtFactors.get(f) match {
                case Some(m) => m.value(reqId)
                case None    => Set.empty
              }
          }
        }

      val tagOrderByName = p.config.tags.orderByName
      val tagOrderByPos  = p.config.tags.orderByPos

      val allSetMemo: FilterDead => ReqId => Set[ApplicableTagId] = {
        var live: ReqId => Set[ApplicableTagId] = null
        val x: FilterDead => ReqId => Set[ApplicableTagId] =
          FilterDead.memo {
            case HideDead =>
              Memo { reqId =>
                val b = data(reqId)
                var results = b.derived
                results ++= b.manualLive.keys
                results ++= b.deadTagsInLiveText.keys
                results ++= b.naTagsInLiveText.keys
                results ++= b.liveDefaults.values
                results
              }
            case ShowDead =>
              Memo { reqId =>
                val b = data(reqId)
                var results = live(reqId)
                results ++= b.deadDefaults.values
                results ++= b.manualDead.keys
                results
              }
          }
        live = x(HideDead)
        x
      }

      val defaultsFdFn: FilterDead => ForReq => Map[CustomField.Tag.Id, ApplicableTagId] =
        FilterDead.memo {
          case HideDead => _.liveDefaults
          case ShowDead => b => Util.mergeMaps(b.liveDefaults, b.deadDefaults)
        }

      def allLiveDeadResults(distFn: FilterDead => TagFieldDistribution.TagIds): FilterDead => ReqId => ResultsLiveDead = {
        FilterDead.memoLazy { fd =>
          val allSetFD = allSetMemo(fd)
          val defaultDist = distFn(fd)
          val defaultsFn = defaultsFdFn(fd)
          Memo { reqId =>
            @inline def b = data(reqId)
            def liveDeadResults(dist: TagFieldDistribution.TagIds): ResultsLiveDead =
              new ResultsLiveDead {

                override def isEmpty =
                  allSet.isEmpty

                override def allSet =
                  allSetFD(reqId)

                override lazy val otherSet = {
                  val tagsUsedInFields = dist.usedInFields
                  allSet &~ tagsUsedInFields
                }

                override val fieldSet = Memo { fid =>
                  val legal = dist inField fid
                  allSet & legal
                }

                override lazy val allOrdered =
                  MutableArray(allSet).sortBy(tagOrderByName.apply).to(Vector)

                override def otherOrdered =
                  MutableArray(otherSet).sortBy(tagOrderByName.apply).to(Vector)

                override val fieldOrdered = Memo { fid =>
                  MutableArray(fieldSet(fid)).sortBy(tagOrderByPos.apply).to(Vector)
                }

                override def defaults =
                  defaultsFn(b)

                override def withTagFieldDist(dist2: TagFieldDistribution.TagIds) =
                  if (dist eq dist2)
                    this
                  else
                    liveDeadResults(dist2)
              }

            liveDeadResults(defaultDist)
          }
        }
      }

      def allResults(distFn: FilterDead => TagFieldDistribution.TagIds): VirtualProjectTags = {
        val fdResults = allLiveDeadResults(distFn)

        new VirtualProjectTags {
          override def apply(reqId: ReqId): ResultsMono =
            monoResults(reqId)

          override def apply(reqId: ReqId, filterDead: FilterDead): ResultsLiveDead =
            fdResults(filterDead)(reqId)

          override def underTagGroup(tagGroupId: TagGroupId, filterDead: FilterDead): ReqId => Vector[ApplicableTagId] = {
            val tagLookup = fdResults(filterDead)
            val tagScope  = p.config.tagFieldDistribution(filterDead).inTagGroup(tagGroupId)
            val tagOrder  = p.config.tags.applicableTagOrdering(tagGroupId, filterDead)

            reqId =>
              MutableArray(tagLookup(reqId).allSet.iterator.filter(tagScope.contains))
                .sort(tagOrder)
                .iterator()
                .toVector
          }

          override def withTagFieldDistFn(distFn2: FilterDead => TagFieldDistribution.TagIds) =
            allResults(distFn2)

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
              val title = s"${PlainText.pubid(req.pubid, p)}: ${pt.reqTitle(req)}"
              var items = Vector.empty[String]
              items ++= showM(b.manualLive        ).map("manualLive         : " + _)
              items ++= showM(b.manualDead        ).map("manualDead         : " + _)
              items ++= showM(b.deadTagsInLiveText).map("deadTagsInLiveText : " + _)
              items ++= showM(b.naTagsInLiveText  ).map("naTagsInLiveText   : " + _)
              items ++= showD(b.liveDefaults      ).map("liveDefaults       : " + _)
              items ++= showD(b.deadDefaults      ).map("deadDefaults       : " + _)
              items ++= showS(b.derived           ).map("derived            : " + _)
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
      }

      allResults(p.dataLogic.tagFieldDist)
    }
  }
}
