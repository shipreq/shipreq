package shipreq.webapp.client.project.app.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra._
import monocle.macros.Lenses
import shipreq.base.util.{MutableArray, NonEmptyVector}
import shipreq.base.util.univeq._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.CreateContentCmd
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.client.base.data.{Enabled, TCB}
import shipreq.webapp.client.project.protocol.ServerCall
import shipreq.webapp.client.project.widgets._
import shipreq.webapp.client.project.widgets.high._
import SelectOne.{Choice, Choices}
import UnivEq.univEqOption

object CreationInterface {
  sealed trait Type
  case class GenericReqType(rt: CustomReqTypeId) extends Type
  case object UseCaseType                        extends Type
  case object ReqCodeGroupType                   extends Type

  implicit def typeEquality: UnivEq[Type] = UnivEq.force

  type SelType = Option[Type]
  val selectComponent = SelectOne.Component[SelType]

  case class Props(createIO    : ServerCall[CreateContentCmd],
                   state       : State,
                   previewState: Preview.State)

  @Lenses
  case class State(selectedType: SelType,
                   rcg         : CreateReqCodeGroupState,
                   req         : CreateReqState)

  sealed trait Status
  case object Editing extends Status
  case object Locked extends Status
  case class Failed(reason: String) extends Status
  implicit def statusEquality: UnivEq[Status] = UnivEq.derive

  @Lenses
  case class CreateReqCodeGroupState(status: Status, reqCode: String, title: String)

  @Lenses
  case class CreateReqState(status: Status, reqCodes: String, title: String, tags: String, imp: String)

  implicit val reusabilityState: Reusability[State] = Reusability.byRef

  def initState: State =
    State(None,
      CreateReqCodeGroupState(Editing, "", ""),
      CreateReqState(Editing, "", "", "", ""))
}

// =====================================================================================================================
import CreationInterface._

class CreationInterface($               : CompState.Access[State],
                        previewFeature  : Preview.ForChildren,
                        pxProject       : Px[Project],
                        pxProjectText   : Px[PlainText.ForProject],
                        pxProjectWidgets: Px[ProjectWidgets],
                        pxTextSearch    : Px[TextSearch]) {

  val types: Px[Choices[SelType]] =
    pxProject.map { p =>
      val blank = Choice[SelType](None, "", Enabled)
      val rcg   = Choice[SelType](Some(ReqCodeGroupType), UiText.reqCodeGroup, Enabled)
      val uc    = Choice[SelType](Some(UseCaseType), UiText.useCase, Enabled)
      val grs   = MutableArray(p.config.reqTypes.liveCustomReqTypes)
                    .map(rt => Choice[SelType](Some(GenericReqType(rt.id)), rt.fullName, Enabled))
                    .sortBy(_.label)
      NonEmptyVector(blank) ++ grs.array :+ uc :+ rcg
    }

  val Component = ReactComponentB[Props]("Creation")
    .render($ => render($.props))
    //    .configure(shouldComponentUpdate) TODO
    .build

  private def render(p: Props) = {
    val s = p.state

    val select: SelType => Callback =
      $ _setStateL State.selectedType

    val selProps = SelectOne.Props[SelType](
      s.selectedType, types.value(), Some(select))

    val detail: Type => TagMod = {
      case GenericReqType(rt) => CreateGenericReq.Component((p, rt))
      case UseCaseType        => CreateUseCase.Component(p)
      case ReqCodeGroupType   => CreateReqCodeGroup.Component(p)
    }

    <.div(
      "Create ",
      selectComponent(selProps),
      s.selectedType map detail)
  }

  def ctrls(create: Option[Callback], status: Status, setStatus: Status => Callback): TagMod = {
    val c: TagMod = createButton(create)
    failureNotice(status, setStatus).fold(c)(c + _)
  }

  def createButton(create: Option[Callback]) =
    <.button(
      ^.disabled := create.isEmpty,
      ^.onClick -->? create,
      "Create") // english

  def failureNotice(status: Status, setStatus: Status => Callback): Option[TagMod] =
    status match {
      case Editing | Locked => None
      case Failed(err) => Some(
        <.div(err, <.button("Got it", ^.onClick --> setStatus(Editing)))) // English
    }

  def ajax(p: Props, setStatus: Status => Callback, cmd: CreateContentCmd): Callback = Callback.lazily {
    val remoteCall = p.createIO(cmd,
      TCB.Success(setStatus(Editing)),
      f => TCB.Failure(setStatus(Failed(f))))
    remoteCall >> setStatus(Locked)
  }

  private val _noExtra: Any ~=> TagMod =
    ReusableFn(_ => EmptyTag)

  // ===================================================================================================================

  object CreateReqCodeGroup {

    val $$ = $ zoomL State.rcg
    val setStatus  = $$ zoomL CreateReqCodeGroupState.status  setState (_: Status)
    val setReqCode = ReusableFn($$ zoomL CreateReqCodeGroupState.reqCode setState (_: String))
    val setTitle   = ReusableFn($$ zoomL CreateReqCodeGroupState.title   setState (_: String))

    val titleFocus = FocusId.InCI(ReqCodeGroupType, Column.Title)

    def render(p: Props) = {
      import Px.AutoValue._

      val state = p.state.rcg

      val propsReqCode =
        ReqCodeEditor.Single.Props(
          ReusableVar(state.reqCode)(setReqCode),
          None,
          pxProject.reqCodes.trie,
          _noExtra)

      val propsTitle =
        RichTextEditor.ReqCodeGroupTitle.Props(
          pxProject,
          pxProjectText,
          pxTextSearch,
          pxProjectWidgets,
          ReusableVar(state.title)(setTitle),
          None,
          None,
          previewFeature.forChild(titleFocus, p.previewState),
          None)

      val create: Option[Callback] =
        for {
          code  <- propsReqCode.parseResult.toOption
          title <- propsTitle.parseResult.toOption
          if state.status !=* Locked
        } yield
          ajax(p, setStatus, CreateContentCmd.CreateReqCodeGroup(code, title))

      <.table(
        <.thead(
          <.tr(
            <.th(UiText.ColumnNames.code),
            <.th(UiText.ColumnNames.title),
            <.th())),
        <.tbody(
          <.tr(
            <.td(propsReqCode.render),
            <.td(propsTitle.render),
            <.td(ctrls(create, state.status, setStatus)))))
    }

    val Component = ReactComponentB[Props]("CreateRCG").render_P(render).build
  }

  // ===================================================================================================================

  object CreateReqShared {
    val $$ = $ zoomL State.req
    val setStatus   = $$ zoomL CreateReqState.status   setState (_: Status)
    val setImp      = ReusableFn($$ zoomL CreateReqState.imp      setState (_: String))
    val setReqCodes = ReusableFn($$ zoomL CreateReqState.reqCodes setState (_: String))
    val setTitle    = ReusableFn($$ zoomL CreateReqState.title    setState (_: String))
    val setTags     = ReusableFn($$ zoomL CreateReqState.tags     setState (_: String))

    val pxImpLookup = Px.apply2(pxProject, pxProjectText)(ImplicationEditor.Lookup.all)

    val pxImpValidationFn =
      pxProject.map(p =>
        ImplicationEditor.validationFn(p, None, Set.empty, Column implicationDirection Column.ImplicationSrc))

    val pxTagLookup = pxProject map TagEditor.Lookup.all

    private val reqFormHeader =
      <.thead(
        <.tr(
          <.th(UiText.ColumnNames.title),
          <.th(UiText.ColumnNames.code),
          <.th(UiText.ColumnNames.tags),
          <.th(UiText.ColumnNames.implicationSrc),
          <.th))

    def renderReqForm(reqCodes: ReactElement,
                      title   : ReactElement,
                      tags    : ReactElement,
                      imps    : ReactElement,
                      ctrls   : TagMod) = {
      <.table(
        reqFormHeader,
        <.tbody(
          <.tr(
            <.td(title),
            <.td(reqCodes),
            <.td(tags),
            <.td(imps),
            <.td(ctrls))))
    }

    import Px.AutoValue._

    def getPropsReqCodes(state: CreateReqState) =
      ReqCodeEditor.Multiple.Props(
        ReusableVar(state.reqCodes)(setReqCodes),
        None,
        pxProject.reqCodes.trie,
        _noExtra)

    def getPropsTags(state: CreateReqState) =
      TagEditor.Props(
        None,
        ReusableVar(state.tags)(setTags),
        pxTagLookup,
        None,
        None)

    def getPropsImps(state: CreateReqState) =
      ImplicationEditor.Props(
        ReusableVar(state.imp)(setImp),
        pxImpLookup,
        pxImpValidationFn,
        None,
        None,
        pxTextSearch)
  }

  // ===================================================================================================================

  object CreateGenericReq {
    import CreateReqShared._

    type Props = (CreationInterface.Props, CustomReqTypeId)

    val titleFocus = FocusId.InCI(GenericReqType(CustomReqTypeId(-1)), Column.Title)

    def render(pp: Props) = {
      import Px.AutoValue._

      val p = pp._1
      val state = p.state.req

      val propsReqCodes = getPropsReqCodes(state)
      val propsTags     = getPropsTags(state)
      val propsImp      = getPropsImps(state)

      val propsTitle =
        RichTextEditor.GenericReqTitle.Props(
          pxProject,
          pxProjectText,
          pxTextSearch,
          pxProjectWidgets,
          ReusableVar(state.title)(setTitle),
          None,
          None,
          previewFeature.forChild(titleFocus, p.previewState),
          None)

      val create: Option[Callback] =
        for {
          codes   <- propsReqCodes.parseResult   .toOption
          title   <- propsTitle   .parseResult   .toOption
          tagIds  <- propsTags    .parseResultSet.toOption
          impSrcs <- propsImp     .parseResult   .toOption
          if state.status !=* Locked
        } yield {
          val cmd = CreateContentCmd.CreateGenericReq(pp._2, title, codes, tagIds, impSrcs.added)
          ajax(p, setStatus, cmd)
        }

      renderReqForm(
        propsReqCodes.render,
        propsTitle   .render,
        propsTags    .render,
        propsImp     .render,
        ctrls(create, state.status, setStatus))
    }

    val Component = ReactComponentB[Props]("CreateGR").render_P(render).build
  }

  // ===================================================================================================================

  object CreateUseCase {
    import CreateReqShared._

    type Props = CreationInterface.Props

    val titleFocus = FocusId.InCI(UseCaseType, Column.Title)

    def render(p: Props) = {
      import Px.AutoValue._

      val state = p.state.req

      val propsReqCodes = getPropsReqCodes(state)
      val propsTags     = getPropsTags(state)
      val propsImp      = getPropsImps(state)

      val propsTitle =
        RichTextEditor.UseCaseTitle.Props(
          pxProject,
          pxProjectText,
          pxTextSearch,
          pxProjectWidgets,
          ReusableVar(state.title)(setTitle),
          None,
          None,
          previewFeature.forChild(titleFocus, p.previewState),
          None)

      val create: Option[Callback] =
        for {
          codes   <- propsReqCodes.parseResult   .toOption
          title   <- propsTitle   .parseResult   .toOption
          tagIds  <- propsTags    .parseResultSet.toOption
          impSrcs <- propsImp     .parseResult   .toOption
          if state.status !=* Locked
        } yield {
          val cmd = CreateContentCmd.CreateUseCase(title, codes, tagIds, impSrcs.added)
          ajax(p, setStatus, cmd)
        }

      renderReqForm(
        propsReqCodes.render,
        propsTitle   .render,
        propsTags    .render,
        propsImp     .render,
        ctrls(create, state.status, setStatus))
    }

    val Component = ReactComponentB[Props]("CreateUC").render_P(render).build
  }
}
