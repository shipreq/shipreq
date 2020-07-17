package shipreq.webapp.base.event

import shipreq.webapp.base.data.{DataValidators => V, _}
import shipreq.webapp.base.event.ApplyEventLib._
import shipreq.webapp.base.event.Event._

trait ApplyOtherEvent {
  this: ApplyEvent =>

  object OtherEvents {
    val validateProjectName = validateA(V.projectName)

    def applyProjectNameSet(e: ProjectNameSet): Eval[Unit] =
      validateProjectName(e.name).flatMap(name =>
        Eval.mod(Project.name.set(name)))
  }

  // ===================================================================================================================

  object ManualIssueEvents {

    def applyCreate(e: ManualIssueCreate): Eval[Unit] = {

      def validateDoesntExist: Eval[Unit] =
        Eval.tests(
          !_.manualIssues.imap.containsK(e.id),
          s"${show(e.id)} already exists.")

      def add(mi: ManualIssues): ManualIssues = {
        val nextId = ManualIssueId(1 + math.max(e.id.value, mi.nextId.value))
        val newMap = mi.imap + ManualIssue(e.id, e.text)
        ManualIssues(newMap, nextId)
      }

      for {
        _ <- whenUntrusted(validateDoesntExist)
        _ <- Project.manualIssues.modify(add)
      } yield ()
    }

    private def validateExists(id: ManualIssueId): Eval[Unit] =
      Eval.tests(
        _.manualIssues.imap.containsK(id),
        s"${show(id)} not found.")

    def applyUpdate(e: ManualIssueUpdate): Eval[Unit] =
      for {
        _ <- whenUntrusted(validateExists(e.id))
        _ <- Project.manualIssues.modify(_.modIMap(_ + ManualIssue(e.id, e.text)))
      } yield ()

    def applyDelete(e: ManualIssueDelete): Eval[Unit] =
      for {
        _ <- whenUntrusted(validateExists(e.id))
        _ <- Project.manualIssues.modify(_.modIMap(_ - e.id))
      } yield ()
  }

  // ===================================================================================================================

  object SavedViewEvents {
    import shipreq.webapp.base.data.savedview._

    private val ^ = SavedViewGD
    private val GD = GenericDataApp[SavedView](^)

    private val v1 = RetiredGenericData.SavedViewGDv1
    private val GDv1 = GenericDataApp[SavedView](v1)

    private val updateColumns        = fieldUpdateFn(SavedView.columns)
    private val updateFilter         = fieldUpdateFn(SavedView.filter)
    private val updateFilterDead     = fieldUpdateFn(SavedView.filterDead)
    private val updateOrder          = fieldUpdateFn(SavedView.order)
    private val updateImpGraphConfig = fieldUpdateFn(SavedView.impGraphConfig)

    private val updateValuesV1 = GDv1.updateEachValue {
      case v: v1.ValueForName       => sv => validateName(Some(sv.id), v.value).map(SavedView.name.set(_)(sv))
      case v: v1.ValueForColumns    => updateColumns   (v.value)
      case v: v1.ValueForFilter     => updateFilter    (v.value)
      case v: v1.ValueForFilterDead => updateFilterDead(v.value)
      case v: v1.ValueForOrder      => updateOrder     (v.value)
    }

    private val updateValues = GD.updateEachValue {
      case v: ^.ValueForName           => sv => validateName(Some(sv.id), v.value).map(SavedView.name.set(_)(sv))
      case v: ^.ValueForColumns        => updateColumns       (v.value)
      case v: ^.ValueForFilter         => updateFilter        (v.value)
      case v: ^.ValueForFilterDead     => updateFilterDead    (v.value)
      case v: ^.ValueForOrder          => updateOrder         (v.value)
      case v: ^.ValueForImpGraphConfig => updateImpGraphConfig(v.value)
    }

    private val updateIdCeiling = updateIdCeilingFn(IdCeilings.savedView)

    private def validateName(subject: Option[SavedView.Id], newName: SavedView.Name)(implicit trust: Trust): Eval[SavedView.Name] =
      if (trust is Trusted)
        Eval.pure(newName)
      else
        Eval.get.flatMap { p =>
          val state = SavedView.Name.State(subject, p.savedViews)
          val validate = validateI(SavedView.Name.validator(state))(_.value)
          validate(newName)
        }

    def applyCreate(e: SavedViewCreateV1): Eval[Unit] =
      applyCreate(SavedViewCreate(
        id             = e.id,
        name           = e.name,
        columns        = e.columns,
        order          = e.order,
        filterDead     = e.filterDead,
        filter         = e.filter,
        impGraphConfig = None,
      ))

    def applyCreate(e: SavedViewCreate): Eval[Unit] = {

      def validateId: Eval[Unit] =
        Eval.tests(
          _.savedViewIterator.forall(_.id !=* e.id),
          s"${show(e.id)} already exists.")

      def add(sv: SavedView): SavedViews.Optional => SavedViews.Optional = {
        case None      => Some(SavedViews(sv))
        case Some(svs) => Some(svs + sv)
      }

      def createSV(name: SavedView.Name) =
        SavedView(e.id, name, View(
          filterDead     = e.filterDead,
          columns        = e.columns,
          order          = e.order,
          filter         = e.filter,
          impGraphConfig = e.impGraphConfig,
        ))

      for {
        _    <- whenUntrusted(validateId)
        name <- validateName(None, e.name)
        _    <- Project.savedViews.modify(add(createSV(name)))
        _    <- updateIdCeiling(e.id)
      } yield ()
    }

    private def notFound(id: SavedView.Id) = s"${show(id)} not found."

    def applyUpdate(e: SavedViewUpdateV1): Eval[Unit] =
      optionalModEval(Project.savedView(e.id), notFound(e.id))(
        updateValuesV1(e.vs))

    def applyUpdate(e: SavedViewUpdate): Eval[Unit] =
      optionalModEval(Project.savedView(e.id), notFound(e.id))(
        updateValues(e.vs))

    def applyDefaultSet(e: SavedViewDefaultSet): Eval[Unit] =
      optionalModEval(Project.savedViewsNE, notFound(e.id)) { ne =>
        val get: Eval[SavedView] =
          if (trust is Untrusted)
            Eval.some(ne.nonDefault.get(e.id), notFound(e.id))
          else
            Eval.pure(ne.nonDefault.need(e.id))
        get.map(SavedViews.NonEmpty(_, ne.nonDefault - e.id + ne.default))
      }

    def applyDelete(e: SavedViewDelete): Eval[Unit] = {

      def delDefault(ne: SavedViews.NonEmpty): SavedViews.Optional =
        if (ne.nonDefault.isEmpty)
          None
        else {
          val min = ne.nonDefault.valuesIterator.minBy(_.name.value.toUpperCase)
          val svs = new SavedViews.NonEmpty(min, ne.nonDefault - min.id)
          Some(svs)
        }

      def delNonDefault(ne: SavedViews.NonEmpty): SavedViews.Optional =
        Some(ne.copy(nonDefault = ne.nonDefault - e.id))

      for {
        p      <- Eval.get
        svs    <- Eval.some(p.savedViews, notFound(e.id))
        _      <- whenUntrusted(Eval.some(svs.get(e.id), notFound(e.id)).void)
        result = if (svs.default.id ==* e.id) delDefault(svs) else delNonDefault(svs)
        _      <- Project.savedViews.set(result)
      } yield ()
    }

  }
}