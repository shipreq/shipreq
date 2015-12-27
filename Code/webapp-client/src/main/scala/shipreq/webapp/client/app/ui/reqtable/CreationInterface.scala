package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra._
import monocle.macros.Lenses
import shipreq.base.util.{NonEmptyVector, UnivEq, univEqOps}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.CreateContentCmd
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.client.app.ui.SelectOne.{Choice, Choices}
import shipreq.webapp.client.app.ui.newui.{ImplicationEditor, ReqCodeEditor, RichTextEditor, TagEditor}
import shipreq.webapp.client.app.ui.{ProjectWidgets, SelectOne}
import shipreq.webapp.client.lib.TCB
import shipreq.webapp.client.lib.ui.feature.PreviewFeature
import shipreq.webapp.client.util.Enabled
import UnivEq.univEqOption

object CreationInterface {
  sealed trait Type
  case object ReqCodeGroupType extends Type
  case class GenericReqType(rt: CustomReqTypeId) extends Type
  implicit def typeEquality: UnivEq[Type] = UnivEq.force

  type SelType = Option[Type]
  val selectComponent = SelectOne.Component[SelType]

  case class Props(createIO    : CallServer[CreateContentCmd],
                   state       : State,
                   previewState: Preview.State)

  @Lenses
  case class State(selectedType: SelType,
                   rcg         : CreateReqCodeGroupState,
                   greq        : CreateGenericReqState)

  sealed trait Status
  case object Editing extends Status
  case object Locked extends Status
  case class Failed(reason: String) extends Status
  implicit def statusEquality: UnivEq[Status] = UnivEq.deriveAuto

  @Lenses
  case class CreateReqCodeGroupState(status: Status, reqCode: String, title: String)

  @Lenses
  case class CreateGenericReqState(status: Status, reqCodes: String, title: String, tags: String, imp: String)

  implicit val reusabilityState: Reusability[State] = Reusability.byRef

  def initState: State =
    State(None,
      CreateReqCodeGroupState(Editing, "", ""),
      CreateGenericReqState(Editing, "", "", "", ""))
}

// =====================================================================================================================
import shipreq.webapp.client.app.ui.reqtable.CreationInterface._

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
      val rts   = p.config.liveCustomReqTypes
                    .map(rt => Choice[SelType](Some(GenericReqType(rt.id)), rt.fullName, Enabled))
                    .sortBy(_.label)
      (NonEmptyVector(blank) :+ rcg) ++ rts
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

  private val _emptyTag: Any => TagMod =
    _ => EmptyTag

  object CreateReqCodeGroup { // ---------------------------------------------------------------------------------------

    val $$ = $ zoomL State.rcg
    val setStatus  = $$ zoomL CreateReqCodeGroupState.status  setState (_: Status)
    val setReqCode = $$ zoomL CreateReqCodeGroupState.reqCode setState (_: String)
    val setTitle   = $$ zoomL CreateReqCodeGroupState.title   setState (_: String)

    val titleFocus = FocusId.InCI(ReqCodeGroupType, Column.Title)

    def render(p: Props) = {
      import Px.AutoValue._

      val state = p.state.rcg

      val propsReqCode =
        ReqCodeEditor.Single.Props(
          ExternalVar(state.reqCode)(setReqCode),
          None,
          pxProject.reqCodes.trie,
          _emptyTag)

      val propsTitle =
        RichTextEditor.ReqCodeGroupTitle.Props(
          pxProject,
          pxProjectText,
          pxTextSearch,
          pxProjectWidgets,
          ExternalVar(state.title)(setTitle),
          previewFeature.forChild(titleFocus, p.previewState),
          None,
          _emptyTag)

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

  object CreateGenericReq { // ---------------------------------------------------------------------------------------

    type Props = (CreationInterface.Props, CustomReqTypeId)

    val $$ = $ zoomL State.greq
    val setStatus   = $$ zoomL CreateGenericReqState.status   setState (_: Status)
    val setReqCodes = $$ zoomL CreateGenericReqState.reqCodes setState (_: String)
    val setTitle    = $$ zoomL CreateGenericReqState.title    setState (_: String)
    val setTags     = $$ zoomL CreateGenericReqState.tags     setState (_: String)
    val setImp      = $$ zoomL CreateGenericReqState.imp      setState (_: String)

    val titleFocus = FocusId.InCI(GenericReqType(CustomReqTypeId(-1)), Column.Title)

    val pxImpLookup = Px.apply2(pxProject, pxProjectText)(ImplicationEditor.Lookup.all)

    val pxImpValidationFn =
      pxProject.map(p =>
        ImplicationEditor.validationFn(p, None, Set.empty, ImplicationEditor isDeclFwd Column.ImplicationSrc))

    val pxTagLookup = pxProject map TagEditor.Lookup.all

    def render(pp: Props) = {
      import Px.AutoValue._

      val p = pp._1
      val state = p.state.greq

      val propsReqCodes =
        ReqCodeEditor.Multiple.Props(
          ExternalVar(state.reqCodes)(setReqCodes),
          None,
          pxProject.reqCodes.trie,
          _emptyTag)

      val propsTags =
        TagEditor.Props(
          ExternalVar(state.tags)(setTags),
          pxTagLookup,
          _emptyTag)

      val propsTitle =
        RichTextEditor.GenericReqTitle.Props(
          pxProject,
          pxProjectText,
          pxTextSearch,
          pxProjectWidgets,
          ExternalVar(state.title)(setTitle),
          previewFeature.forChild(titleFocus, p.previewState),
          None,
          _emptyTag)

      val propsImp =
        ImplicationEditor.Props(
          ExternalVar(state.imp)(setImp),
          pxImpLookup,
          pxImpValidationFn,
          pxTextSearch,
          _emptyTag)

      val create: Option[Callback] =
        for {
          codes   <- propsReqCodes.parseResult.toOption
          title   <- propsTitle   .parseResult.toOption
          tags    <- propsTags    .parseResult.toOption
          impSrcs <- propsImp     .parseResult.toOption
          if state.status !=* Locked
        } yield {
          val tagIds = tags.map(_.id).toSet
          val cmd = CreateContentCmd.CreateGenericReq(pp._2, title, codes, tagIds, impSrcs.added)
          ajax(p, setStatus, cmd)
        }

      <.table(
        <.thead(
          <.tr(
            <.th(UiText.ColumnNames.code),
            <.th(UiText.ColumnNames.title),
            <.th(UiText.ColumnNames.tags),
            <.th(UiText.ColumnNames.implicationSrc),
            <.th())),
        <.tbody(
          <.tr(
            <.td(propsReqCodes.render),
            <.td(propsTitle   .render),
            <.td(propsTags    .render),
            <.td(propsImp     .render),
            <.td(ctrls(create, state.status, setStatus)))))
    }

    val Component = ReactComponentB[Props]("CreateGR").render_P(render).build
  }
}
