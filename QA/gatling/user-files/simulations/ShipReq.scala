import io.gatling.core.Predef._
import io.gatling.core.session.Expression
import io.gatling.core.structure.ChainBuilder
import io.gatling.http.Predef._
import io.gatling.jdbc.Predef._
import io.gatling.http.Headers.Names._
import io.gatling.http.Headers.Values._
import scala.concurrent.duration._
import bootstrap._
import assertions._

import ShipReq._

object ShipReq {

  case class TestUser(username: String, password: String)

  val testUser1 = TestUser("test__xabcdefghijklmnopx__1000", """71^[:q-.At*'.^7Vh>(^rEQ>yxOJ(WK/p/Zo.H+KA(4PT07Kp.`FYEG^4YDO)rQl=N<g@WDNpPGeOr<Px26F6@GDA:wG3pwy50CRJk""")

  val httpProtocol = http
    .baseURL(System.getProperty("baseurl"))
    .acceptHeader("*/*")
    .acceptEncodingHeader("gzip, deflate")
    .acceptLanguageHeader("en-US,en;q=0.5")
    .connection("keep-alive")
    .userAgentHeader("Mozilla/5.0 (X11; Linux x86_64; rv:25.0) Gecko/20100101 Firefox/25.0")
    .disableFollowRedirect

  val userClicked = Map("Accept" -> """text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8""")

  val loginPostHeaders = Map(
    """Accept""" -> """text/javascript, application/javascript, application/ecmascript, application/x-ecmascript, */*; q=0.01""",
    """Cache-Control""" -> """no-cache""",
    """Content-Type""" -> """application/x-www-form-urlencoded; charset=UTF-8""",
    """Pragma""" -> """no-cache""",
    """X-Requested-With""" -> """XMLHttpRequest""")

  def getOnce(keyPrefix: String, run: String => ChainBuilder)(name: String) = {
    val key = s"loaded_${keyPrefix}_$name"
    doIf(s => !s.contains(key))(exec(run(name))).exec(_.set(key, true))
  }
  
  // ----------
  // --  JS  --
  // ----------

  val jsHeaders = Map("Accept" -> "*/*")

  val getJsOnce = getOnce("js", n => getJs(s"/assets/$n.js")) _

  def getJs(url: String) =
    exec(http("JS: " + url.replace("/assets/", "").replaceFirst("\\?.*$", "")).get(url).headers(jsHeaders).check(status is 200))
  
  val getLiftAjax = exec(http("JS: liftAjax").get("/ajax_request/liftAjax.js").headers(jsHeaders).check(status is 200))

  // -----------
  // --  CSS  --
  // -----------

  val cssHeaders = Map("Accept" -> "text/css,*/*;q=0.1")

  val getCssOnce = getOnce("css", n => getCss(s"/assets/$n.css")) _

  def getCss(url: String) =
    exec(http("CSS: " + url.replace("/assets/", "").replaceFirst("\\?.*$", "")).get(url).headers(cssHeaders).check(status is 200))

  // -------------------------------------------------------------------------------------------------------------------

  val getCommonDeps =
      exec(pause(30 millis))
      .exec(getCssOnce("app"))
      .exec(getJsOnce("app"))
      .exec(getLiftAjax)

  def home(loggedIn: Boolean) = {
    val r = regex("Test Project")
    val (name, respTest) = if (loggedIn)
      ("Home (logged in)", r.exists)
    else
      ("Home (anon)", r.notExists)
    exec(http("Home").get("/").headers(userClicked).check(status is 200, respTest))
      .exec(getCommonDeps)
  }

  val about =
    exec(http("About").get("/about").headers(userClicked).check(status is 200))
      .exec(getCommonDeps)

  val register =
    exec(http("Register").get("/register").headers(userClicked).check(status is 200))
      .exec(getCommonDeps)

  val getLogin =
    exec(http("Login GET").get("/login").headers(userClicked)
      .check(
        regex("""lift_page = "([A-Za-z0-9]+)"""").saveAs("login_post_url") //
        , regex("""hidden" name="([A-Za-z0-9]+)"""").saveAs("login_post_hidden") //
        , regex("""name="([A-Za-z0-9]+)"[^>]+?who""").saveAs("login_post_username") //
        , regex("""name="([A-Za-z0-9]+)"[^>]+?password""").saveAs("login_post_password") //
        , regex("""liftAjax.lift_uriSuffix='([A-Za-z0-9]+?)=_';return true""").saveAs("login_post_submit") //
        ))
      .exec(getCommonDeps)

  def postLogin(user: TestUser) =
    exec(http("Login POST").post("/ajax_request/${login_post_url}-00/").headers(loginPostHeaders)
      .param("${login_post_hidden}", "true") //
      .param("${login_post_username}", user.username) //
      .param("${login_post_password}", user.password) //
      .param("${login_post_submit}", "_") //
      .check(status is 200, regex("window.location").exists))

  def loginAs(user: TestUser) =
    group("Login")(
      getLogin
        .pause(120 millis).exec(postLogin(user))
        .pause(70 millis).exec(home(true)))

  def project(id: String) =
    group("Project Overview")(
      exec(http(s"Project: $id").get(s"/project/$id").headers(userClicked).check(status is 200))
        .exec(getCommonDeps)
        .exec(getJsOnce("zeroclipboard"))
        .exec(getJsOnce("project")))

  def usecaseEditor(id: String) =
    group("UseCase Editor")(
      exec(http(s"UCE: $id").get(s"/usecase/$id").headers(userClicked).check(status is 200))
        .exec(getCommonDeps)
        .exec(getJsOnce("uce")))

  def readUsecases(project: String) =
    group("Read UCs")(
      exec(http(s"Read UCs: $project").get(s"/project/$project/read").headers(userClicked).check(status is 200))
        .pause(20 millis)
        .exec(getCommonDeps)
        .exec(getJs("/assets/vendor/mathjax/MathJax.js?config=/assets/mathjax"))
        .pause(40 millis)
        .exec(getJs("/assets/mathjax.js")))

  val logout =
    exec(http("Logout").get("/logout").check(status is 302))

  val smokeTest = scenario("Smoke Test")
    .group("Home (anon)")(exec(home(false)))
                      .exec(about)
                      .exec(register)
    .pause( 50 millis).exec(loginAs(testUser1))
    .pause(100 millis).exec(project("j8NA940XXv9"))
    .pause(200 millis).exec(usecaseEditor("2PbB1awttl1"))
    .pause(200 millis).exec(usecaseEditor("2PbB10XLd8j"))
    .pause(200 millis).exec(project("j8NA940XXv9"))
    .pause(200 millis).exec(readUsecases("j8NA940XXv9"))
    .pause(200 millis).exec(logout)
}

class SmokeTest extends Simulation {
  setUp(smokeTest.inject(atOnce(1 users))).protocols(httpProtocol)
}

class StressTest extends Simulation {
  val users = 200
  val newUsersPerSec = 10
  setUp(smokeTest.inject(ramp(users users).over((users / newUsersPerSec) seconds))).protocols(httpProtocol)
}
