package shipreq.webapp.member.project.protocol.binary.v1

import shipreq.webapp.base.util.On
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.savedview.{ImpGraphConfig, SavedView}
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.project.filter.Filter

/** v1.7
  *
  * Changes:
  *
  *   - add FilterAst.Scoped
  */
object Rev7 {
  import boopickle.DefaultBasic._
  import shipreq.webapp.base.protocol.binary.v1.BaseData._
  import BaseMemberData1._
  import BaseMemberData1.SavedViewPicklers._
  import BaseMemberData2._
  import Rev1._
  import Rev4._

  implicit lazy val pickleValidFilter: Pickler[Filter.Valid] = {
    import shipreq.webapp.member.project.filter.{IntensionalReqSet, FilterAst}
    import Filter._
    import Filter.Implicits._
    import Filter.Valid.FieldCriteriaF

    implicit val picklerNonEmptyVectorUnit: Pickler[NonEmptyVector[Unit]] =
      implicitly[Pickler[Int]].xmap(NonEmptyVector force Vector.fill(_)(()))(_.length)

    implicit val picklerNonEmptySetInt: Pickler[NonEmptySet[Int]] =
      pickleNES

    implicit def picklerIRSetS[RT: Pickler]: Pickler[IntensionalReqSet.SomeOfType[RT]] =
      new Pickler[IntensionalReqSet.SomeOfType[RT]] {
        override def pickle(a: IntensionalReqSet.SomeOfType[RT])(implicit state: PickleState): Unit = {
          state.pickle(a.reqType)
          state.pickle(a.numbers)
        }
        override def unpickle(implicit state: UnpickleState): IntensionalReqSet.SomeOfType[RT] = {
          val reqType = state.unpickle[RT]
          val numbers = state.unpickle[NonEmptySet[Int]]
          IntensionalReqSet.SomeOfType(reqType, numbers)
        }
      }

    implicit def picklerIRSetW[RT: Pickler]: Pickler[IntensionalReqSet.WholeType[RT]] =
      transformPickler(IntensionalReqSet.WholeType.apply[RT])(_.reqType)

    def picklerIRSet[RT: Pickler]: Pickler[IntensionalReqSet[RT]] =
      new Pickler[IntensionalReqSet[RT]] {
        import IntensionalReqSet._
        private[this] final val KeySomeOfType = 0
        private[this] final val KeyWholeType  = 1
        override def pickle(a: IntensionalReqSet[RT])(implicit state: PickleState): Unit =
          a match {
            case b: SomeOfType[RT] => state.enc.writeByte(KeySomeOfType); state.pickle(b)
            case b: WholeType[RT]  => state.enc.writeByte(KeyWholeType ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): IntensionalReqSet[RT] =
          state.dec.readByte match {
            case KeySomeOfType => state.unpickle[SomeOfType[RT]]
            case KeyWholeType  => state.unpickle[WholeType[RT]]
          }
      }

    implicit val picklerSpecialBuiltInFieldFilterOk: Pickler[SpecialBuiltInField.FilterOk] =
      new Pickler[SpecialBuiltInField.FilterOk] {
        private[this] final val KeyTitle = 't'
        override def pickle(a: SpecialBuiltInField.FilterOk)(implicit state: PickleState): Unit =
          a match {
            case SpecialBuiltInField.Title => state.enc.writeByte(KeyTitle)
          }
        override def unpickle(implicit state: UnpickleState): SpecialBuiltInField.FilterOk =
          state.dec.readByte match {
            case KeyTitle => SpecialBuiltInField.Title
          }
      }

    implicit val picklerValidHashTag: Pickler[Valid.HashTag] =
      pickleDisj

    implicit lazy val picklerValidFilter: Pickler[Valid.Field] =
      pickleDisj

    implicit val picklerValidIssueCatNEV: Pickler[NonEmptyVector[Valid.IssueCat]] =
      pickleNEV

    implicit val picklerValidReqSubset: Pickler[Valid.ReqSubset] =
      picklerIRSet

    implicit val picklerValidReqSet: Pickler[Valid.ReqSet] =
      pickleNEV

    implicit val picklerFilterAstAttr: Pickler[FilterAst.Attr] =
      new Pickler[FilterAst.Attr] {
        private[this] final val KeyAnyIssue = 'i'
        private[this] final val KeyAnyTag   = 't'
        override def pickle(a: FilterAst.Attr)(implicit state: PickleState): Unit =
          a match {
            case FilterAst.Attr.AnyIssue => state.enc.writeByte(KeyAnyIssue)
            case FilterAst.Attr.AnyTag   => state.enc.writeByte(KeyAnyTag  )
          }
        override def unpickle(implicit state: UnpickleState): FilterAst.Attr =
          state.dec.readByte match {
            case KeyAnyIssue => FilterAst.Attr.AnyIssue
            case KeyAnyTag   => FilterAst.Attr.AnyTag
          }
      }

    implicit val picklerFilterAstText: Pickler[FilterAst.Text] =
      new Pickler[FilterAst.Text] {
        override def pickle(a: FilterAst.Text)(implicit state: PickleState): Unit = {
          state.pickle(a.text)
          state.pickle(a.quoteChar)
        }
        override def unpickle(implicit state: UnpickleState): FilterAst.Text = {
          val text      = state.unpickle[String]
          val quoteChar = state.unpickle[Option[Char]]
          FilterAst.Text(text, quoteChar)
        }
      }

    implicit val picklerFilterAstRegex: Pickler[FilterAst.Regex] =
      transformPickler(FilterAst.Regex.apply)(_.text)

    implicit val picklerFilterAstPresence: Pickler[FilterAst.Presence[Valid.Attr]] =
      transformPickler(FilterAst.Presence.apply[Valid.Attr])(_.attr)

    implicit val picklerFilterAstHasIssue: Pickler[FilterAst.HasIssue[Valid.IssueCat]] =
      new Pickler[FilterAst.HasIssue[Valid.IssueCat]] {
        override def pickle(a: FilterAst.HasIssue[Valid.IssueCat])(implicit state: PickleState): Unit = {
          state.pickle(a.on)
          state.pickle(a.criteria)
        }
        override def unpickle(implicit state: UnpickleState): FilterAst.HasIssue[Valid.IssueCat] = {
          val on       = state.unpickle[On]
          val criteria = state.unpickle[NonEmptyVector[Valid.IssueCat]]
          FilterAst.HasIssue(on, criteria)
        }
      }

    implicit val picklerFilterAstHashRef: Pickler[FilterAst.HashRef[Valid.HashTag]] =
      transformPickler(FilterAst.HashRef.apply[Valid.HashTag])(_.value)

    implicit def picklerFilterAstImpCriteriaReqs[R: Pickler]: Pickler[FilterAst.ImpCriteria.Reqs[R]] =
      transformPickler(FilterAst.ImpCriteria.Reqs.apply[R])(_.value)

    implicit def picklerFilterAstImpCriteriaQuery[Q: Pickler]: Pickler[FilterAst.ImpCriteria.Query[Q]] =
      transformPickler(FilterAst.ImpCriteria.Query.apply[Q])(_.value)

    implicit def picklerFilterAstImpCriteria[R, Q](implicit p1: Pickler[FilterAst.ImpCriteria.Query[Q]], p2: Pickler[FilterAst.ImpCriteria.Reqs[R]]): Pickler[FilterAst.ImpCriteria[R, Q]] =
      new Pickler[FilterAst.ImpCriteria[R, Q]] {
        private[this] final val KeyQuery = 'q'
        private[this] final val KeyReqs  = 'r'
        override def pickle(a: FilterAst.ImpCriteria[R, Q])(implicit state: PickleState): Unit =
          a match {
            case b: FilterAst.ImpCriteria.Query[Q] => state.enc.writeByte(KeyQuery); state.pickle(b)
            case b: FilterAst.ImpCriteria.Reqs[R]  => state.enc.writeByte(KeyReqs ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): FilterAst.ImpCriteria[R, Q] =
          state.dec.readByte match {
            case KeyQuery => state.unpickle[FilterAst.ImpCriteria.Query[Q]]
            case KeyReqs  => state.unpickle[FilterAst.ImpCriteria.Reqs[R]]
          }
      }

    implicit val picklerFilterAstImpliesAnyOf: Pickler[FilterAst.ImpliesAnyOf[Valid.ImpCriteriaF, Unit]] = {
      type F[A] = Valid.ImpCriteriaF[A]
      type T = FilterAst.ImpliesAnyOf[Valid.ImpCriteriaF, Unit]
      new Pickler[T] {
        override def pickle(a: T)(implicit state: PickleState): Unit = {
          writeVersion(1)
          state.pickle(a.criteria)
        }
        override def unpickle(implicit state: UnpickleState): T =
          readByVersion(1) {

            // v1.0
            case 0 =>
              val reqs = state.unpickle[Valid.ReqSet]
              val criteria = FilterAst.ImpCriteria.Reqs(reqs): F[Unit]
              FilterAst.ImpliesAnyOf(criteria)

            // v1.1
            case 1 =>
              val criteria = state.unpickle[F[Unit]]
              FilterAst.ImpliesAnyOf(criteria)
          }
      }
    }

    implicit val picklerFilterAstImpliedByAnyOf: Pickler[FilterAst.ImpliedByAnyOf[Valid.ImpCriteriaF, Unit]] =
      picklerFilterAstImpliesAnyOf.xmap(x => FilterAst.ImpliedByAnyOf(x.criteria))(x => FilterAst.ImpliesAnyOf(x.criteria))

    implicit val picklerFilterAstReqs: Pickler[FilterAst.Reqs[Valid.ReqSet]] =
      transformPickler(FilterAst.Reqs.apply[Valid.ReqSet])(_.reqs)

    implicit val picklerFilterAstReqType: Pickler[FilterAst.ReqType[Valid.ReqType]] =
      transformPickler(FilterAst.ReqType.apply[Valid.ReqType])(_.reqType)

    implicit val picklerFilterAstNot: Pickler[FilterAst.Not[Unit]] =
      transformPickler(FilterAst.Not.apply[Unit])(_.clause)

    implicit val picklerFilterAstAllOf: Pickler[FilterAst.AllOf[Unit]] =
      transformPickler(FilterAst.AllOf.apply[Unit])(_.clauses)

    implicit val picklerFilterAstAnyOf: Pickler[FilterAst.AnyOf[Unit]] =
      new Pickler[FilterAst.AnyOf[Unit]] {
        override def pickle(a: FilterAst.AnyOf[Unit])(implicit state: PickleState): Unit = {
          state.pickle(a.tail)
        }
        override def unpickle(implicit state: UnpickleState): FilterAst.AnyOf[Unit] = {
          val tail = state.unpickle[NonEmptyVector[Unit]]
          FilterAst.AnyOf((), tail)
        }
      }

    implicit val picklerFilterAstFieldAttr: Pickler[FilterAst.FieldAttr] =
      new Pickler[FilterAst.FieldAttr] {
        private[this] final val KeyBlank         = 0
        private[this] final val KeyDefaultInUse  = 1
        private[this] final val KeyNotApplicable = 2
        private[this] final val KeyNotBlank      = 3
        override def pickle(a: FilterAst.FieldAttr)(implicit state: PickleState): Unit =
          a match {
            case FilterAst.FieldAttr.Blank         => state.enc.writeByte(KeyBlank        )
            case FilterAst.FieldAttr.DefaultInUse  => state.enc.writeByte(KeyDefaultInUse )
            case FilterAst.FieldAttr.NotApplicable => state.enc.writeByte(KeyNotApplicable)
            case FilterAst.FieldAttr.NotBlank      => state.enc.writeByte(KeyNotBlank)
          }
        override def unpickle(implicit state: UnpickleState): FilterAst.FieldAttr =
          state.dec.readByte match {
            case KeyBlank         => FilterAst.FieldAttr.Blank
            case KeyDefaultInUse  => FilterAst.FieldAttr.DefaultInUse
            case KeyNotApplicable => FilterAst.FieldAttr.NotApplicable
            case KeyNotBlank      => FilterAst.FieldAttr.NotBlank
          }
      }

    implicit val picklerFieldCriteriaAttr: Pickler[FilterAst.FieldCriteria.Attr[FilterAst.FieldAttr]] =
      transformPickler(FilterAst.FieldCriteria.Attr.apply[FilterAst.FieldAttr])(_.value)

    implicit val picklerFieldCriteriaReqTypePosSet: Pickler[FilterAst.FieldCriteria.ReqTypePosSet] =
      transformPickler(FilterAst.FieldCriteria.ReqTypePosSet.apply)(_.value)

    implicit val picklerFieldCriteriaQuery: Pickler[FilterAst.FieldCriteria.Query[Unit]] =
      transformPickler(FilterAst.FieldCriteria.Query.apply[Unit])(_.value)

    implicit val picklerFieldCriteria: Pickler[FieldCriteriaF[Unit]] =
      new Pickler[FieldCriteriaF[Unit]] {
        private[this] final val KeyAttr          = 'a'
        private[this] final val KeyReqTypePosSet = 'p'
        private[this] final val KeyQuery         = 'q'
        override def pickle(a: FieldCriteriaF[Unit])(implicit state: PickleState): Unit =
          a match {
            case b: FilterAst.FieldCriteria.Attr[FilterAst.FieldAttr] => state.enc.writeByte(KeyAttr         ); state.pickle(b)
            case b: FilterAst.FieldCriteria.ReqTypePosSet             => state.enc.writeByte(KeyReqTypePosSet); state.pickle(b)
            case b: FilterAst.FieldCriteria.Query[Unit]               => state.enc.writeByte(KeyQuery        ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): FieldCriteriaF[Unit] =
          state.dec.readByte match {
            case KeyAttr          => state.unpickle[FilterAst.FieldCriteria.Attr[FilterAst.FieldAttr]]
            case KeyReqTypePosSet => state.unpickle[FilterAst.FieldCriteria.ReqTypePosSet]
            case KeyQuery         => state.unpickle[FilterAst.FieldCriteria.Query[Unit]]
          }
      }

    implicit val picklerFilterAstFieldProp: Pickler[Valid.FieldPropF[Unit]] =
      new Pickler[Valid.FieldPropF[Unit]] {
        override def pickle(a: Valid.FieldPropF[Unit])(implicit state: PickleState): Unit = {
          state.pickle(a.field)
          state.pickle(a.criteria)
        }
        override def unpickle(implicit state: UnpickleState) = {
          val field = state.unpickle[Valid.Field]
          val attr  = state.unpickle[FieldCriteriaF[Unit]]
          FilterAst.FieldProp(field, attr)
        }
      }

    implicit def picklerFilterAstScopeDerivation[A: Pickler]: Pickler[FilterAst.Scope.Derivation[A]] =
      new Pickler[FilterAst.Scope.Derivation[A]] {
        override def pickle(a: FilterAst.Scope.Derivation[A])(implicit state: PickleState): Unit = {
          state.pickle(a.field)
        }
        override def unpickle(implicit state: UnpickleState): FilterAst.Scope.Derivation[A] = {
          val field = state.unpickle[Option[A]]
          FilterAst.Scope.Derivation(field)
        }
      }

    implicit def picklerFilterAstScope[A](implicit p1: Pickler[FilterAst.Scope.Derivation[A]]): Pickler[FilterAst.Scope[A]] =
      new Pickler[FilterAst.Scope[A]] {
        private[this] final val KeyDerivation = 0
        override def pickle(a: FilterAst.Scope[A])(implicit state: PickleState): Unit =
          a match {
            case b: FilterAst.Scope.Derivation[A] => state.enc.writeByte(KeyDerivation); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): FilterAst.Scope[A] =
          state.dec.readByte match {
            case KeyDerivation => state.unpickle[FilterAst.Scope.Derivation[A]]
          }
      }

    implicit val picklerValidScope: Pickler[Valid.Scope] =
      pickleNES

    implicit val picklerFilterAstScoped1: Pickler[FilterAst.Scoped1[Valid.Scope, Unit]] =
      new Pickler[FilterAst.Scoped1[Valid.Scope, Unit]] {
        override def pickle(a: FilterAst.Scoped1[Valid.Scope, Unit])(implicit state: PickleState): Unit = {
          state.pickle(a.main)
          state.pickle(a.scope)
        }
        override def unpickle(implicit state: UnpickleState): FilterAst.Scoped1[Valid.Scope, Unit] = {
          val main  = state.unpickle[Boolean]
          val scope = state.unpickle[Valid.Scope]
          FilterAst.Scoped1(main, scope, ())
        }
      }

    implicit val picklerFilterAstScoped2: Pickler[FilterAst.Scoped2[Valid.Scope, Unit]] =
      new Pickler[FilterAst.Scoped2[Valid.Scope, Unit]] {
        override def pickle(a: FilterAst.Scoped2[Valid.Scope, Unit])(implicit state: PickleState): Unit = {
          state.pickle(a.scope)
        }
        override def unpickle(implicit state: UnpickleState): FilterAst.Scoped2[Valid.Scope, Unit] = {
          val scope = state.unpickle[Valid.Scope]
          FilterAst.Scoped2(scope, (), ())
        }
      }

    implicit val picklerFilterAstOrderOp: Pickler[FilterAst.OrderOp] =
      new Pickler[FilterAst.OrderOp] {
        private[this] final val Key_<  = 0
        private[this] final val Key_>  = 1
        private[this] final val Key_<= = 2
        private[this] final val Key_>= = 3
        override def pickle(a: FilterAst.OrderOp)(implicit state: PickleState): Unit =
          a match {
            case FilterAst.OrderOp.<  => state.enc.writeByte(Key_< )
            case FilterAst.OrderOp.>  => state.enc.writeByte(Key_> )
            case FilterAst.OrderOp.<= => state.enc.writeByte(Key_<=)
            case FilterAst.OrderOp.>= => state.enc.writeByte(Key_>=)
          }
        override def unpickle(implicit state: UnpickleState): FilterAst.OrderOp =
          state.dec.readByte match {
            case Key_<  => FilterAst.OrderOp.<
            case Key_>  => FilterAst.OrderOp.>
            case Key_<= => FilterAst.OrderOp.<=
            case Key_>= => FilterAst.OrderOp.>=
          }
      }

    implicit val picklerFilterAstRelativeTags: Pickler[FilterAst.RelativeTags[Valid.ApTag]] =
      new Pickler[FilterAst.RelativeTags[Valid.ApTag]] {
        override def pickle(a: FilterAst.RelativeTags[Valid.ApTag])(implicit state: PickleState): Unit = {
          state.pickle(a.op)
          state.pickle(a.subject)
        }
        override def unpickle(implicit state: UnpickleState): FilterAst.RelativeTags[Valid.ApTag] = {
          val op      = state.unpickle[FilterAst.OrderOp]
          val subject = state.unpickle[Valid.ApTag]
          FilterAst.RelativeTags(op, subject)
        }
      }

    implicit val picklerValidF: Pickler[ValidF[Unit]] =
      new Pickler[ValidF[Unit]] {
        private[this] final val KeyAllOf          = 0
        private[this] final val KeyAnyOf          = 1
        private[this] final val KeyHasIssue       = 2
        private[this] final val KeyHashRef        = 3
        private[this] final val KeyImpliedByAnyOf = 4
        private[this] final val KeyImpliesAnyOf   = 5
        private[this] final val KeyNot            = 6
        private[this] final val KeyPresence       = 7
        private[this] final val KeyRegex          = 8
        private[this] final val KeyReqType        = 9
        private[this] final val KeyReqs           = 10
        private[this] final val KeyText           = 11
        private[this] final val KeyFieldProp      = 12
        private[this] final val KeyScoped1        = 13
        private[this] final val KeyScoped2        = 14
        private[this] final val KeyRelativeTags   = 15
        override def pickle(a: ValidF[Unit])(implicit state: PickleState): Unit =
          a match {
            case b: FilterAst.AllOf         [Unit]                     => state.enc.writeByte(KeyAllOf         ); state.pickle(b)
            case b: FilterAst.AnyOf         [Unit]                     => state.enc.writeByte(KeyAnyOf         ); state.pickle(b)
            case b: FilterAst.HasIssue      [Valid.IssueCat]           => state.enc.writeByte(KeyHasIssue      ); state.pickle(b)
            case b: FilterAst.HashRef       [Valid.HashTag]            => state.enc.writeByte(KeyHashRef       ); state.pickle(b)
            case b: FilterAst.ImpliedByAnyOf[Valid.ImpCriteriaF, Unit] => state.enc.writeByte(KeyImpliedByAnyOf); state.pickle(b)
            case b: FilterAst.ImpliesAnyOf  [Valid.ImpCriteriaF, Unit] => state.enc.writeByte(KeyImpliesAnyOf  ); state.pickle(b)
            case b: FilterAst.Not           [Unit]                     => state.enc.writeByte(KeyNot           ); state.pickle(b)
            case b: FilterAst.Presence      [Valid.Attr]               => state.enc.writeByte(KeyPresence      ); state.pickle(b)
            case b: FilterAst.Regex                                    => state.enc.writeByte(KeyRegex         ); state.pickle(b)
            case b: FilterAst.ReqType       [Valid.ReqType]            => state.enc.writeByte(KeyReqType       ); state.pickle(b)
            case b: FilterAst.Reqs          [Valid.ReqSet]             => state.enc.writeByte(KeyReqs          ); state.pickle(b)
            case b: FilterAst.Text                                     => state.enc.writeByte(KeyText          ); state.pickle(b)
            case b: Valid.FieldPropF        [Unit]                     => state.enc.writeByte(KeyFieldProp     ); state.pickle(b)
            case b: FilterAst.Scoped1       [Valid.Scope, Unit       ] => state.enc.writeByte(KeyScoped1       ); state.pickle(b)
            case b: FilterAst.Scoped2       [Valid.Scope, Unit       ] => state.enc.writeByte(KeyScoped2       ); state.pickle(b)
            case b: FilterAst.RelativeTags  [Valid.ApTag             ] => state.enc.writeByte(KeyRelativeTags  ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): ValidF[Unit] =
          state.dec.readByte match {
            case KeyAllOf          => state.unpickle[FilterAst.AllOf         [Unit                    ]]
            case KeyAnyOf          => state.unpickle[FilterAst.AnyOf         [Unit                    ]]
            case KeyHasIssue       => state.unpickle[FilterAst.HasIssue      [Valid.IssueCat          ]]
            case KeyHashRef        => state.unpickle[FilterAst.HashRef       [Valid.HashTag           ]]
            case KeyImpliedByAnyOf => state.unpickle[FilterAst.ImpliedByAnyOf[Valid.ImpCriteriaF, Unit]]
            case KeyImpliesAnyOf   => state.unpickle[FilterAst.ImpliesAnyOf  [Valid.ImpCriteriaF, Unit]]
            case KeyNot            => state.unpickle[FilterAst.Not           [Unit                    ]]
            case KeyPresence       => state.unpickle[FilterAst.Presence      [Valid.Attr              ]]
            case KeyRegex          => state.unpickle[FilterAst.Regex                                   ]
            case KeyReqType        => state.unpickle[FilterAst.ReqType       [Valid.ReqType           ]]
            case KeyReqs           => state.unpickle[FilterAst.Reqs          [Valid.ReqSet            ]]
            case KeyText           => state.unpickle[FilterAst.Text                                    ]
            case KeyFieldProp      => state.unpickle[Valid.FieldPropF        [Unit                    ]]
            case KeyScoped1        => state.unpickle[FilterAst.Scoped1       [Valid.Scope, Unit       ]]
            case KeyScoped2        => state.unpickle[FilterAst.Scoped2       [Valid.Scope, Unit       ]]
            case KeyRelativeTags   => state.unpickle[FilterAst.RelativeTags  [Valid.ApTag             ]]
          }
      }

    pickleFix[ValidF]
  }

  // ===================================================================================================================

  import Rev1.SavedViewPicklers._

  object SavedViewPicklers {
    import shipreq.webapp.member.project.data.savedview._

    implicit val picklerView: Pickler[View] =
      new Pickler[View] {
        override def pickle(a: View)(implicit state: PickleState): Unit = {
          writeVersion(1) // v1.1
          state.pickle(a.columns)
          state.pickle(a.order)
          state.pickle(a.filterDead)
          state.pickle(a.filter)
          state.pickle(a.impGraphConfig)
        }
        override def unpickle(implicit state: UnpickleState): View =
          readByVersion(1) {

            // v1.0
            case 0 =>
              val columns    = state.unpickle[NonEmptyVector[Column]]
              val order      = state.unpickle[SortCriteria]
              val filterDead = state.unpickle[FilterDead]
              val filter     = state.unpickle[Option[Filter.Valid]]
              View(columns, order, filterDead, filter, None)

            // v1.1
            case 1 =>
              val columns        = state.unpickle[NonEmptyVector[Column]]
              val order          = state.unpickle[SortCriteria]
              val filterDead     = state.unpickle[FilterDead]
              val filter         = state.unpickle[Option[Filter.Valid]]
              val impGraphConfig = state.unpickle[Option[ImpGraphConfig]]
              View(columns, order, filterDead, filter, impGraphConfig)
          }
      }

    implicit val picklerSavedView: Pickler[SavedView] =
      new Pickler[SavedView] {
        override def pickle(a: SavedView)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.name)
          state.pickle(a.view)
        }
        override def unpickle(implicit state: UnpickleState): SavedView = {
          val id   = state.unpickle[SavedView.Id]
          val name = state.unpickle[SavedView.Name]
          val view = state.unpickle[View]
          SavedView(id, name, view)
        }
      }

    implicit val pickleSavedViewsND: Pickler[SavedViews.NonDefault] =
      pickleIMap(SavedViews.emptyNonDefault)

    implicit val pickleSavedViews: Pickler[SavedViews.NonEmpty] =
      new Pickler[SavedViews.NonEmpty] {
        override def pickle(a: SavedViews.NonEmpty)(implicit state: PickleState): Unit = {
          state.pickle(a.default)
          state.pickle(a.nonDefault)
        }
        override def unpickle(implicit state: UnpickleState): SavedViews.NonEmpty = {
          val default    = state.unpickle[SavedView]
          val nonDefault = state.unpickle[SavedViews.NonDefault]
          SavedViews.NonEmpty(default, nonDefault)
        }
      }
  }

  // ===================================================================================================================

  implicit lazy val pickleSavedViewGDv1: Pickler[RetiredGenericData.SavedViewGDv1.NonEmptyValues] = {
    import RetiredGenericData.SavedViewGDv1._

    implicit val picklerValueForColumns    = transformPickler(ValueForColumns   .apply)(_.value)
    implicit val picklerValueForFilter     = transformPickler(ValueForFilter    .apply)(_.value)
    implicit val picklerValueForFilterDead = transformPickler(ValueForFilterDead.apply)(_.value)
    implicit val picklerValueForName       = transformPickler(ValueForName      .apply)(_.value)
    implicit val picklerValueForOrder      = transformPickler(ValueForOrder     .apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyColumns    = 'C'
        private[this] final val KeyFilter     = 'F'
        private[this] final val KeyFilterDead = 'D'
        private[this] final val KeyName       = 'N'
        private[this] final val KeyOrder      = 'O'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForColumns    => state.enc.writeByte(KeyColumns   ); state.pickle(b)
            case b: ValueForFilter     => state.enc.writeByte(KeyFilter    ); state.pickle(b)
            case b: ValueForFilterDead => state.enc.writeByte(KeyFilterDead); state.pickle(b)
            case b: ValueForName       => state.enc.writeByte(KeyName      ); state.pickle(b)
            case b: ValueForOrder      => state.enc.writeByte(KeyOrder     ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyColumns    => state.unpickle[ValueForColumns]
            case KeyFilter     => state.unpickle[ValueForFilter]
            case KeyFilterDead => state.unpickle[ValueForFilterDead]
            case KeyName       => state.unpickle[ValueForName]
            case KeyOrder      => state.unpickle[ValueForOrder]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  implicit lazy val pickleSavedViewGD: Pickler[SavedViewGD.NonEmptyValues] = {
    import SavedViewGD._

    implicit val picklerValueForColumns        = transformPickler(ValueForColumns       .apply)(_.value)
    implicit val picklerValueForFilter         = transformPickler(ValueForFilter        .apply)(_.value)
    implicit val picklerValueForFilterDead     = transformPickler(ValueForFilterDead    .apply)(_.value)
    implicit val picklerValueForName           = transformPickler(ValueForName          .apply)(_.value)
    implicit val picklerValueForOrder          = transformPickler(ValueForOrder         .apply)(_.value)
    implicit val picklerValueForImpGraphConfig = transformPickler(ValueForImpGraphConfig.apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyColumns        = 'C'
        private[this] final val KeyFilter         = 'F'
        private[this] final val KeyFilterDead     = 'D'
        private[this] final val KeyName           = 'N'
        private[this] final val KeyOrder          = 'O'
        private[this] final val KeyImpGraphConfig = 'I'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForColumns        => state.enc.writeByte(KeyColumns       ); state.pickle(b)
            case b: ValueForFilter         => state.enc.writeByte(KeyFilter        ); state.pickle(b)
            case b: ValueForFilterDead     => state.enc.writeByte(KeyFilterDead    ); state.pickle(b)
            case b: ValueForName           => state.enc.writeByte(KeyName          ); state.pickle(b)
            case b: ValueForOrder          => state.enc.writeByte(KeyOrder         ); state.pickle(b)
            case b: ValueForImpGraphConfig => state.enc.writeByte(KeyImpGraphConfig); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyColumns        => state.unpickle[ValueForColumns]
            case KeyFilter         => state.unpickle[ValueForFilter]
            case KeyFilterDead     => state.unpickle[ValueForFilterDead]
            case KeyName           => state.unpickle[ValueForName]
            case KeyOrder          => state.unpickle[ValueForOrder]
            case KeyImpGraphConfig => state.unpickle[ValueForImpGraphConfig]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  private[binary] implicit lazy val picklerEventSavedViewCreateV1: Pickler[Event.SavedViewCreateV1] =
    new Pickler[Event.SavedViewCreateV1] {
      override def pickle(a: Event.SavedViewCreateV1)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.name)
        state.pickle(a.columns)
        state.pickle(a.order)
        state.pickle(a.filterDead)
        state.pickle(a.filter)
      }
      override def unpickle(implicit state: UnpickleState): Event.SavedViewCreateV1 = {
        val id         = state.unpickle[SavedView.Id]
        val name       = state.unpickle[SavedView.Name]
        val columns    = state.unpickle[NonEmptyVector[savedview.Column]]
        val order      = state.unpickle[savedview.SortCriteria]
        val filterDead = state.unpickle[FilterDead]
        val filter     = state.unpickle[Option[Filter.Valid]]
        Event.SavedViewCreateV1(id, name, columns, order, filterDead, filter)
      }
    }

  private[binary] implicit lazy val picklerEventSavedViewUpdateV1: Pickler[Event.SavedViewUpdateV1] =
    new Pickler[Event.SavedViewUpdateV1] {
      override def pickle(a: Event.SavedViewUpdateV1)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.SavedViewUpdateV1 = {
        val id = state.unpickle[SavedView.Id]
        val vs = state.unpickle[RetiredGenericData.SavedViewGDv1.NonEmptyValues]
        Event.SavedViewUpdateV1(id, vs)
      }
    }

  private[binary] implicit lazy val picklerEventSavedViewCreate: Pickler[Event.SavedViewCreate] =
    new Pickler[Event.SavedViewCreate] {
      override def pickle(a: Event.SavedViewCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.name)
        state.pickle(a.columns)
        state.pickle(a.order)
        state.pickle(a.filterDead)
        state.pickle(a.filter)
        state.pickle(a.impGraphConfig)
      }
      override def unpickle(implicit state: UnpickleState): Event.SavedViewCreate = {
        val id             = state.unpickle[SavedView.Id]
        val name           = state.unpickle[SavedView.Name]
        val columns        = state.unpickle[NonEmptyVector[savedview.Column]]
        val order          = state.unpickle[savedview.SortCriteria]
        val filterDead     = state.unpickle[FilterDead]
        val filter         = state.unpickle[Option[Filter.Valid]]
        val impGraphConfig = state.unpickle[Option[ImpGraphConfig]]
        Event.SavedViewCreate(id, name, columns, order, filterDead, filter, impGraphConfig)
      }
    }

  private[binary] implicit lazy val picklerEventSavedViewUpdate: Pickler[Event.SavedViewUpdate] =
    new Pickler[Event.SavedViewUpdate] {
      override def pickle(a: Event.SavedViewUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.SavedViewUpdate = {
        val id = state.unpickle[SavedView.Id]
        val vs = state.unpickle[SavedViewGD.NonEmptyValues]
        Event.SavedViewUpdate(id, vs)
      }
    }

}
