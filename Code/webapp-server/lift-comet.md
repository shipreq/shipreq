Let's make sense of Lift's confusing doc and API.

# Actor-land

* SimpleActor[-T]
  * Abstract: `! : T => Unit`

* TypedActor[-T, +R] <: SimpleActor[-T]
  * Abstract: `!? : T => R`

* ForwardableActor[T, R] <: TypedActor[T, R]
  * Abstract: proxy between external actors

* GenericActor[+R] = TypedActor[Any, R]

* SimplestGenericActor = GenericActor[Any]

* SpecializedLiftActor[T] <: SimpleActor[T]
  * Defaults
    * `def !(msg: T): Unit`
    * `protected def around[R](f: => R): R`
    * `protected def aroundLoans: List[CommonLoanWrapper] = Nil`
    * `protected def exceptionHandler: PartialFunction[Throwable, Unit]`
    * `protected def execTranslate(f: T => Unit)(v: T): Unit = f(v)`
    * `protected def highPriorityReceive: Box[PartialFunction[T, Unit]] = Empty`
    * `protected def insertMsgAtHeadOfQueue_!(msg: T): Unit`
    * `protected def messageHandler: PartialFunction[T, Unit]`
    * `protected def testTranslate(f: T => Boolean)(v: T): Boolean = f(v)`

* LiftActor
  * <: SpecializedLiftActor[Any] & GenericActor[Any] & ForwardableActor[Any, Any]
  * Defaults
    * `def !!(msg: Any, timeout: Long): Box[Any]`
    * `def !!(msg: Any): Box[Any]`
    * `def !?(msg: Any): Any`
    * `def !?(timeout: Long, message: Any): Box[Any]`
    * `def !<(msg: Any): LAFuture[Any]`
    * `protected def execTranslate(f: Any => Unit)(v: Any)`
    * `protected def reply(v: Any)`
    * `protected def testTranslate(f: Any => Boolean)(v: Any)`
  * Final
    * `protected def forwardMessageTo(msg: Any, forwardTo: TypedActor[Any, Any])`

# Comet-land

* LiftCometActor
  * <: TypedActor[Any, Any] & ForwardableActor[Any, Any] & Dependent
  * Abstract
    * `def buildSpan(xml: NodeSeq): NodeSeq`
    * `def hasOuter: Boolean`
    * `def lastListenerTime: Long`
    * `def lastRenderTime: Long`
    * `def name: Box[String]`
    * `def parentTag: Elem`
    * `def theType: Box[String]`
    * `def uniqueId: String`
    * `protected def initCometActor(creationInfo: CometCreationInfo): Unit`
  * Defaults
    * `def cometActorLocale: Locale = _myLocale`
    * `def cometProcessingTimeout = LiftRules.cometProcessingTimeout`
    * `def cometProcessingTimeoutHandler(): JsCmd = Noop`
    * `def cometRenderTimeout = LiftRules.cometRenderTimeout`
    * `def cometRenderTimeoutHandler(): Box[NodeSeq] = Empty`
    * `def poke(): Unit = {}`
    * `def predicateChanged(which: Cell[_]): Unit = poke()`
    * `def sendInitialReq_? : Boolean = false`

* BaseCometActor
  * <: LiftActor & LiftCometActor & CssBindImplicits
  * Abstract
    * `def render: RenderOut`
  * Defaults
    * `def appendJsonHandler(h: PartialFunction[Any, JsCmd]): Unit = …`
    * `def attributes = _attributes`
    * `def autoIncludeJsonCode: Boolean = false`
    * `def buildSpan(xml: NodeSeq): Elem = …`
    * `def defaultHtml: NodeSeq = _defaultHtml`
    * `def defaultPrefix: Box[String] = Empty`
    * `def error(id: String, n: NodeSeq): Unit = …`
    * `def error(id: String, n: String): Unit = …`
    * `def error(n: NodeSeq): Unit = …`
    * `def error(n: String): Unit = …`
    * `def exceptionHandler : PartialFunction[Throwable, Unit] = …`
    * `def fixedRender: Box[NodeSeq] = Empty`
    * `def hasOuter = true`
    * `def highPriority: PartialFunction[Any, Unit] = Map.empty`
    * `def jsonSend: JsonCall = _sendJson`
    * `def jsonToIncludeInCode: JsCmd = _jsonToIncludeCode`
    * `def lastListenerTime: Long = _lastListenerTime`
    * `def lastRenderTime: Long =  _lastRenderTime`
    * `def lifespan: Box[TimeSpan] = Empty`
    * `def lowPriority: PartialFunction[Any, Unit] = Map.empty`
    * `def mediumPriority: PartialFunction[Any, Unit] = Map.empty`
    * `def name: Box[String] = _name`
    * `def notice(id: String, n: NodeSeq): Unit = …`
    * `def notice(id: String, n: String): Unit = …`
    * `def notice(n: NodeSeq): Unit = …`
    * `def notice(n: String): Unit = …`
    * `def onJsonError: Box[JsCmd] = Empty`
    * `def parentTag = <div style="display: inline"/>`
    * `def poke(): Unit = …`
    * `def receiveJson: PartialFunction[JsonAST.JValue, JsCmd] = Map()`
    * `def renderClock: Long = lastRenderTime`
    * `def reRender(): Unit = …`
    * `def reRender(sendAll: Boolean): Unit = …`
    * `def theSession = _theSession`
    * `def theType: Box[String] = _theType`
    * `def unWatch = partialUpdate(Call("lift.unlistWatch", uniqueId))`
    * `def warning(id: String, n: NodeSeq): Unit = …`
    * `def warning(id: String, n: String): Unit = …`
    * `def warning(n: NodeSeq): Unit = …`
    * `def warning(n: String): Unit = …`
    * `implicit def elemToFull(in: Elem): Box[NodeSeq] = Full(in)`
    * `implicit def nodeSeqToFull(in: NodeSeq): Box[NodeSeq] = Full(in)`
    * `implicit def pairToPair(in: (String, Any)): (String, NodeSeq) = …`
    * `protected def alwaysReRenderOnPageLoad = false`
    * `protected def answer(answer: Any): Unit = …`
    * `protected def ask(who: LiftCometActor, what: Any)(answerWith: Any => Unit): Unit = …`
    * `protected def cacheFixedRender = false`
    * `protected def calcFixedRender: Box[NodeSeq] =`
    * `protected def captureInitialReq(initialReq: Box[Req]): Unit = …`
    * `protected def clearWiringDependencies(): Unit = …`
    * `protected def cometListeners: List[ListenerId] = listeners.map(_._1)`
    * `protected def composeFunction: PartialFunction[Any, Unit] = composeFunction_i`
    * `protected def dontCacheRendering: Boolean = false`
    * `protected def initCometActor(creationInfo: CometCreationInfo): Unit = …`
    * `protected def listenerTransition(): Unit = {}`
    * `protected def localSetup(): Unit = …`
    * `protected def localShutdown(): Unit = …`
    * `protected def manualWiringDependencyManagement = false`
    * `protected def messageHandler = …`
    * `protected def partialUpdate(cmd: => JsCmd): Unit = this ! PartialUpdateMsg(() => cmd)`
    * `protected def reportError(msg: String, exception: Exception): Unit = …`
    * `protected def running = _running`
    * `protected def startQuestion(what: Any): Unit = …`
    * `protected implicit def arrayToRenderOut(in: Seq[Node]): RenderOut = …`
    * `protected implicit def jsToXmlOrJsCmd(in: JsCmd): RenderOut = …`
    * `protected implicit def nodeSeqFuncToBoxNodeSeq(f: NodeSeq => NodeSeq): Box[NodeSeq]`
    * `protected implicit def nsToNsFuncToRenderOut(f: NodeSeq => NodeSeq) = …`
    * `val uniqueId = Helpers.nextFuncName`

* CometActor <: BaseCometActor
  * `override final private[http] def partialUpdateStream_? = false`

* MessageCometActor <: BaseCometActor
  * Final
    * `override final private[http] def partialUpdateStream_? = true`
    * `override final def render = NodeSeq.Empty`
  * Defaults
    * `protected def pushMessage(cmd: => JsCmd) = partialUpdate(cmd)`

* CometListener <: BaseCometActor
  * Abstract: `protected def registerWith: SimpleActor[Any]`
  * Messages `{Add,Remove}AListener` of itself to actor at `registerWith`

* ListenerManager <: SimpleActor
  * Abstract
    * `protected def createUpdate: Any`
  * Defaults
    * `protected def highPriority: PartialFunction[Any, Unit] = Map.empty`
    * `protected def listenerService: PartialFunction[Any, Unit] = …`
    * `protected def lowPriority: PartialFunction[Any, Unit] = Map.empty`
    * `protected def mediumPriority: PartialFunction[Any, Unit] = Map.empty`
    * `protected def messageHandler: PartialFunction[Any, Unit] = …`
    * `protected def onListenersListEmptied(): Unit = ()`
    * `protected def sendListenersMessage(msg: Any): Unit = …`
    * `protected def updateListeners(listeners: List[ActorTest] = listeners): Unit = …`

* NamedCometActorTrait <: BaseCometActor
  * Defaults
    * `override def lifespan = Full(120.seconds)`
    * On local{Setup,Shutdown}, NamedCometListener.dispatcher(name) ! {,un}register(this)

========================================================================================================================

# Flow

* BaseCometActor
  * Setup
    * `messageHandler` receives `Listen(when, id, AnswerRender => Unit)`
  * Send
    * `.partialUpdate()`
    * `messageHandler` receives `PartialUpdateMsg(js)`
      * Eval `js`
      * `theSession.updateFunctionMap(S.functionMap, uniqueId, time)`
      * `S.clearFunctionMap`
      * Broadcast to listeners
        * val rendered = AnswerRender(…)`
        * `listeners.foreach(_._2(rendered))`
        * `listeners = Nil`
  * Shutdown
