package shipreq.webapp.snippet

import org.joda.time.DateTime
import org.mockito.Mockito.{when, verify}
import org.scalatest.FunSpec
import shipreq.webapp.app.DI
import shipreq.webapp.db.{UserDescriptor, UseCaseIdent, UseCaseRev, Project, Share}
import shipreq.webapp.feature.uc.persist.UseCaseSaveCheckpoint
import shipreq.webapp.feature.{UcFilters, UcFilter}
import shipreq.webapp.lib.{LogShareView, StatLogger}
import shipreq.webapp.lib.Types._
import shipreq.webapp.security.PasswordAndSalt
import shipreq.webapp.test.{TestData, TestHelpers, MockDaoProvider}
import shipreq.webapp.util.NonEmptyTemplate
import ShareView._

class ShareViewTest extends FunSpec with TestHelpers with TestData {
  lazy val template = NonEmptyTemplate.load("share-view").get

  val URL = "12345678".tag[IsShareUrlToken]
  val userId = UD1.id
  val projectId = 123.tag[IsProjectId]
  val share = Share(1.tag, projectId, URL, "z", None, UcFilter.toJson(UcFilters.All))
  val project = Project(projectId, "z", userId)
  val PS = PasswordAndSalt.createWithRandomSalt("correct")
  val cp = {
    val uc = MockUc4.UC
    val ucr = UseCaseRev(UseCaseIdent(8.tag, (2: Short).tag, projectId), 3, 9.tag, uc.header, DateTime.now)
    UseCaseSaveCheckpoint(uc, ucr, null, null)
  }

  def subject = {
    val s = new ShareView(URL)
    s.loadUcs = (p: ProjectId, f: UcFilter) => List(cp)
    s
  }

  def setupValidShare[U](f: => U): U =
    inMockSession {
      DI.StatLogger.doWith(mock[StatLogger]) {
        MockDaoProvider(dao => {
          when(dao.findShareAndPassword(URL)).thenReturn(Some(share, PS))
          when(dao.findShareAndProject(URL)).thenReturn(Some(share, project))
        }).install {
          f
        }
      }
    }

  def setupInvalidShare[U](f: => U): U =
    inMockSession {
      MockDaoProvider(dao => {
        when(dao.findShareAndPassword(URL)).thenReturn(None)
        when(dao.findShareAndProject(URL)).thenReturn(None)
      }).install {
        f
      }
    }

  def setupValidShareAndUser(u: UserDescriptor = UD1)(f: ShareView => Unit): Unit =
    setupValidShare {
      withUserLoggedIn(Some(u)) {
        f(subject)
      }
    }

  def authGuest(token: ShareUrlToken = URL
    , when: DateTime = DateTime.now.minusSeconds(30)
    , password: String @@ Hashed = PS.hashedPassword): Unit =
    AuthMapVar.atomicUpdate(_ + (token -> (when, password)))

  describe("Rendering") {
    def assertFrag(html: String, pass: Boolean, ucs: Boolean, zeroUcs: Boolean = false): Unit = {
      val a = html contains "passwordRequired"
      val b = html contains "ucs-published"
      val c = html contains "There are no use cases"
      (a, b, c) shouldBe(pass, ucs, zeroUcs)
    }

    it("should render the PasswordRequired page") {
      inMockSession {
        val html = subject.renderPage(PasswordRequired)(template).toString
        assertFrag(html, true, false)
        html should include ("type=\"password\"")
      }
    }

    it("should render the ZeroUcs page") {
      val html = subject.renderPage(ZeroUcs("TITLE", <xxx/>))(template).toString
      assertFrag(html, false, true, true)
      html should include("xxx")
    }

    it("should render the ShowUcs page") {
      val html = subject.renderPage(ShowUcs("TITLE", <xxx/>))(template).toString
      assertFrag(html, false, true)
      html should include("xxx")
    }
  }

  describe("GET valid url") {

    describe("User is a guest") {
      it("should deny access and ask for password the first time") {
        subject.initialPage shouldBe PasswordRequired
      }

      it("should allow access when already authorised") {
        setupValidShare {
          authGuest()
          subject.initialPage shouldBe a[ShowUcs]
        }
      }

      it("should deny access when already authorised but expired") {
        setupValidShare {
          authGuest(when = DateTime.now minusWeeks 2)
          subject.initialPage shouldBe PasswordRequired
        }
      }

      it("should deny access when already authorised but password has changed") {
        setupValidShare {
          authGuest(password = "changed".tag)
          subject.initialPage shouldBe PasswordRequired
        }
      }

      it("should deny access when other page is authorised") {
        setupValidShare {
          authGuest(token = "different".tag)
          subject.initialPage shouldBe PasswordRequired
        }
      }
    }

    describe("User is the project owner") {

      it("should display the UCs (when there are none)") {
        setupValidShareAndUser()(s => {
          s.loadUcs = (p: ProjectId, f: UcFilter) => List.empty
          s.initialPage shouldBe a[ZeroUcs]
        })
      }

      it("should display the UCs (when there are some)") {
        setupValidShareAndUser()(_.initialPage shouldBe a[ShowUcs])
      }
    }

    describe("User is logged-in and unrelated") {
      it("should ask for password when user is a guest") {
        setupValidShareAndUser(UD2)(_.initialPage shouldBe PasswordRequired)
      }
    }
  }

  describe("GET invalid url") {
    it("should act as if valid (when user is guest)") {
      setupInvalidShare {
        subject.initialPage shouldBe PasswordRequired
      }
    }

    it("should act as if valid (when user logged in)") {
      setupInvalidShare {
        withUserLoggedIn(Some(UD1)) {
          subject.initialPage shouldBe PasswordRequired
        }
      }
    }
  }

  describe("Credential submission") {
    lazy val jsForIncorrectPasswordValidShare =
      setupValidShare {subject.onSubmitPassword("wrong").toJsCmd}

    it("should reject when password incorrect") {
      assertJsErrorNotice(jsForIncorrectPasswordValidShare, Some("denied"))
    }

    it("should act as if password incorrect when share doesnt exist") {
      setupInvalidShare {
        subject.onSubmitPassword("whatever").toJsCmd shouldBe jsForIncorrectPasswordValidShare
      }
    }

    describe("When password correct") {
      it("should reload the page") {
        setupValidShare {
          val js = subject.onSubmitPassword("correct").toJsCmd
          assertJsErrorNotice(js, None)
          js should include("reload")
        }
      }

      it("should update the authmap") {
        val oldAuthMap: AuthMap = Map("qwe".tag -> (DateTime.now minusHours 4, "roar".tag))
        val o = oldAuthMap.head
        setupValidShare {
          AuthMapVar.set(oldAuthMap)
          subject.onSubmitPassword("correct")

          val a = AuthMapVar.get
          a should have size 2
          a.get(o._1) shouldBe Some(o._2)
          a.get(URL) shouldBe defined
          a(URL)._1.isAfterNow shouldBe false
          a(URL)._1.isAfter(DateTime.now minusSeconds 1) shouldBe true
          a(URL)._2 shouldBe PS.hashedPassword
        }
      }

      it("should log the view") {
        setupValidShare {
          subject.onSubmitPassword("correct")
          verify(DI.StatLogger.vend).!(any[LogShareView])
        }
      }
    }
  }
}