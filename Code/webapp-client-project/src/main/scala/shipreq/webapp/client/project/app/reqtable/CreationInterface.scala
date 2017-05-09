/*
package shipreq.webapp.client.project.app.reqtable

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._, vdom.html_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra._
import monocle.macros.Lenses
import shipreq.base.util.Backwards
import shipreq.base.util.univeq._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.CreateContentCmd
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.client.base.data.{Enabled, TCB}
import shipreq.webapp.client.project.feature.PreviewFeature
import shipreq.webapp.client.project.protocol.ServerCall
import shipreq.webapp.client.project.widgets._
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

  case class Props(createIO: ServerCall[CreateContentCmd],
                   state   : State,
                   preview : PreviewFeature.ReadWrite.Composite[PreviewId])

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

class CreationInterface($               : StateAccessPure[State],
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

  val Component = ScalaComponent.builder[Props]("Creation")
    .render($ => render($.props))
    //    .configure(shouldComponentUpdate) TODO
    .build

  private def render(p: Props) = {
    val s = p.state

    val select: SelType => Callback =
      $ setStateFnL State.selectedType

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
      s.selectedType.whenDefined(detail))
  }

  def ctrls(create: Option[Callback], status: Status, setStatus: Status => Callback): TagMod =
    TagMod(
      createButton(create),
      failureNotice(status, setStatus).whenDefined)

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
    Reusable.fn(_ => EmptyVdom)

  // ===================================================================================================================

  object CreateReqCodeGroup {

    val $$: StateAccessPure[CreateReqCodeGroupState] = $ zoomStateL State.rcg
    val setStatus  = Reusable.fn.state($$ zoomStateL CreateReqCodeGroupState.status ).set
    val setReqCode = Reusable.fn.state($$ zoomStateL CreateReqCodeGroupState.reqCode).set
    val setTitle   = Reusable.fn.state($$ zoomStateL CreateReqCodeGroupState.title  ).set

    val titleFocus = PreviewId.InCI(ReqCodeGroupType, Column.Title)

    def render(p: Props) = {
      import Px.AutoValue._

      val state = p.state.rcg

      val propsReqCode =
        ReqCodeEditor.Single.Props(
          StateSnapshot.withReuse(state.reqCode)(setReqCode),
          None,
          pxProject.reqCodes.trie,
          None,
          None)

      val propsTitle =
        RichTextEditor.ReqCodeGroupTitle.Props(
          pxProject,
          pxProjectText,
          pxTextSearch,
          pxProjectWidgets,
          StateSnapshot.withReuse(state.title)(setTitle),
          None,
          None,
          p.preview(titleFocus),
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

    val Component = ScalaComponent.builder[Props]("CreateRCG").render_P(render).build
  }

  // ===================================================================================================================

  object CreateReqShared {
    val $$: StateAccessPure[CreateReqState] = $ zoomStateL State.req
    val setStatus   = Reusable.fn.state($$ zoomStateL CreateReqState.status  ).set
    val setImp      = Reusable.fn.state($$ zoomStateL CreateReqState.imp     ).set
    val setReqCodes = Reusable.fn.state($$ zoomStateL CreateReqState.reqCodes).set
    val setTitle    = Reusable.fn.state($$ zoomStateL CreateReqState.title   ).set
    val setTags     = Reusable.fn.state($$ zoomStateL CreateReqState.tags    ).set

    val pxImpLookup = Px.apply2(pxProject, pxProjectText)(ImplicationEditor.Lookup.all)

    private def impDir = Backwards
    val pxImpValidationFn = pxProject.map(p => ImplicationEditor.validationFn(p, None, Set.empty, impDir))

    val pxTagLookup = pxProject map TagEditor.Lookup.all

    private val reqFormHeader =
      <.thead(
        <.tr(
          <.th(UiText.ColumnNames.title),
          <.th(UiText.ColumnNames.code),
          <.th(UiText.ColumnNames.tags),
          <.th(UiText.ColumnNames.implications(impDir)),
          <.th))

    def renderReqForm(reqCodes: VdomElement,
                      title   : VdomElement,
                      tags    : VdomElement,
                      imps    : VdomElement,
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
        StateSnapshot.withReuse(state.reqCodes)(setReqCodes),
        None,
        pxProject.reqCodes.trie,
        None,
        None)

    def getPropsTags(state: CreateReqState) =
      TagEditor.Props(
        None,
        StateSnapshot.withReuse(state.tags)(setTags),
        pxTagLookup,
        None,
        None)

    def getPropsImps(state: CreateReqState) =
      ImplicationEditor.Props(
        StateSnapshot.withReuse(state.imp)(setImp),
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

    val titleFocus = PreviewId.InCI(GenericReqType(CustomReqTypeId(-1)), Column.Title)

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
          StateSnapshot.withReuse(state.title)(setTitle),
          None,
          None,
          p.preview(titleFocus),
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

    val Component = ScalaComponent.builder[Props]("CreateGR").render_P(render).build
  }

  // ===================================================================================================================

  object CreateUseCase {
    import CreateReqShared._

    type Props = CreationInterface.Props

    val titleFocus = PreviewId.InCI(UseCaseType, Column.Title)

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
          StateSnapshot.withReuse(state.title)(setTitle),
          None,
          None,
          p.preview(titleFocus),
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

    val Component = ScalaComponent.builder[Props]("CreateUC").render_P(render).build
  }
}
*/
