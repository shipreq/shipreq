Processes
=========
* Difficulut algorithms/complex code design.
  * Model data by hand in Excel.
  * Apply use cases.
  * Write algorithm in psuedo-code.
  * Write skeleton in code. Start with interfaces, and high-level impl of logic.
    This will usually turn up problems with data flow (ie. method params, class
    fields).
  * Write code and tests. Start with low-level, work to high.

* App
  * Requirements & scope.
  * Write use cases for entire app.
  * Prototype app with pen & paper (or similar).
  * Turn prototype into real screens with real dimensions and nothing missing.
    (Inkscape for mobile, real HTML for web).
  * Test use cases with prototype.
  * Move to impl.

Lift
====
* <body data-lift-content-id="main">
  This means crop file contents to #main.
  Without a surround attribute in #main there is no <html>, just the #main elem

* Snippet variable scope
  Object - Global.
  Class  - Per-request (including subsequent Ajax requests).
  object x extends RequestVar[Int](0) - Per-request.
  object x extends SessionVar[Int](0) - Per-session.

* Bindings in net.liftweb.util
    ValueCell, DynamicCell, FuncCell
    val total = price.lift(_ * qty)
* Application:
    "#total" #> WiringUI.asText(total)


* Ajax
  * Singleton snippets with closures.
  * StatefulSnippets.
  * ajaxForm(bind( id->SHtml.tag

  * Make form data-lift="form.ajax"
  * Add <button type="submit">Submit</button>
  * Add hidden(onSubmitCallback)

* Surround JS + CSS in <lift:with-resource-id> to append ?XXX to the links and
  avoid client caching. Suffix changes on Lift boot.

* LiftRules.viewDispatch matches a request URL and returns Either[() =>
  Box[NodeSeq], LiftView]

* class Hello extends LiftView { override def dispatch = {case "cool" => cool _}}
  will automatically serve /Hello/cool

* RequestVar can share data between snippets on same page (ie. req/resp).
  object sample extends RequestVar[Box[String]](Empty) // Define
  sample.is.openOr("")      // Read
  sample(Box.!!(newValue))) // Write
  sample.is.choice(...)(...)

* SessionVar is used the same way as RequestVar.

* SiteMap
  * Simple : Menu("Home") / "index" = Menu(Loc("Home",List("index"),"Home"))
  * Menu(Loc(ID, TemplatePath, Title))
  * Loc == location == template (not URL?)
  * Loc >> xxx allows rules & attributes to be applied to a page.
  * >> Hidden = Don't display in rendered site map HTML
  * >> If(cond, resp) = Used to require certain conditions to access
  * >> Unless(cond, resp) = Used to bar access in certain conditions
  * >> Test(req => cond) = If a condition isn't met, render a 404
  * >> Title(x=>Text(title)) = Dynamic title
  * >> EarlyResponse(resp) = Repond with a given LiftResponse; forget rest of
    rendering pipeline.
  * >> TestAccess(action) = If Empty returned, page is renderable and appears in
    sitemap rendering. If a RedirectResponse is returned, page redirects on
    access and the link is hidden from the sitemap rendering.
  * Custom LocParams can be created by extending UserLocParam.

* URLs with params in path
  * Example to match /user/1/photo/7
  * Menu.param[(User,Photo)]
  * Title = Loc.LinkText(x => Text(s"User: ${x._1}, Photo: ${x._2}"))
  * Param extraction (Menu.param arg #3)
    1) String => Box[T]
    2) PartialFunction + extractors: { case User(u) :: Photo(p) :: Nil => Full((u,p)); case _=>Empty }
  * URL generation (Menu.param arg #4) = T => String/List[String]
    (x: (User,Photo)) => x._1.id.toString :: x._2.id.toString :: Nil
  * Path = "user" / * / "photo" / *
  * Template file will match Path with "star" being used in-place of "*".
    Eg. user/star/photo/star.html
  * Full example:
      lazy val menu = Menu.params[(User, UserPost)](
        "AUserPost",
        Loc.LinkText(tpl => Text("Post: "+tpl._2.title)),
        {
          case User(u) :: UserPost(up) :: Nil => Full(u -> up)
          case _ => Empty
        },
        (tpl: (User, UserPost)) => tpl._1.id.is.toString :: tpl._2.id.is.toString :: Nil
      ) / "users" / * / "posts" / *

* Authentication
  * LocParams If/Unless/TestAccess
  * LiftRules.authentication = Http{Basic,Digest}Authentication
  * LiftRules.authentication = HttpBasicAuthentication("yourRealm"){
      case (username, pwd, req) =>
        if (username & pwd are valid) {
        // HARDCODED EXAMPLE: if(un == "admin" && pwd == "password"){
          userRoles(AuthRole("admin"))
          true
        } else false
    }
    object userRoles extends SessionVar[Box[List[Role]]](List.Empty)
  * >> HttpAuthProtected(req => Full(AuthRole("admin")))
