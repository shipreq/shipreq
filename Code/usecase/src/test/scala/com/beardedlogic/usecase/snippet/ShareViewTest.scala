package com.beardedlogic.usecase.snippet

import org.mockito.Mockito.when
import org.scalatest.FunSpec
import com.beardedlogic.usecase.db.{UseCaseIdent, UseCaseRev, Project, Share}
import com.beardedlogic.usecase.feature.uc.persist.UseCaseSaveCheckpoint
import com.beardedlogic.usecase.feature.{UcFilters, UcFilter}
import com.beardedlogic.usecase.lib.Misc
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.test.{TestData, TestHelpers, MockDaoProvider}
import com.beardedlogic.usecase.util.NonEmptyTemplate

class ShareViewTest extends FunSpec with TestHelpers with TestData {
  lazy val template = NonEmptyTemplate.load("share-view").get

  val URL = "12345678".tag[IsShareUrlToken]
  val userId = UD1.id
  val projectId = 123.tag[IsProjectId]
  val share = Share(1.tag, projectId, URL, "z", None, UcFilter.toJson(UcFilters.All))
  val project = Project(projectId, "z", userId)
  val cp = {
    val uc = MockUc4.UC
    val ucr = UseCaseRev(UseCaseIdent(8.tag, (2: Short).tag, projectId), 3, 9.tag, uc.header, Misc.currentTimeAsIso8601Str)
    UseCaseSaveCheckpoint(uc, ucr, null, null)
  }

  describe("Rendering") {
    def assertFrag(html: String, pass: Boolean, ucs: Boolean, zeroUcs: Boolean = false): Unit = {
      val a = html contains "passwordRequired"
      val b = html contains "ucs-published"
      val c = html contains "There are no use cases"
      (a, b, c) shouldBe(pass, ucs, zeroUcs)
    }

    it("should render the PasswordRequired page") {
      val s = new ShareView(URL)
      val html = s.renderPage(s.PasswordRequired)(template).toString
      assertFrag(html, true, false)
    }

    it("should render the ZeroUcs page") {
      val s = new ShareView(URL)
      val html = s.renderPage(s.ZeroUcs(<xxx/>))(template).toString
      assertFrag(html, false, true, true)
      html should include("xxx")
    }

    it("should render the ShowUcs page") {
      val s = new ShareView(URL)
      val html = s.renderPage(s.ShowUcs(<xxx/>))(template).toString
      assertFrag(html, false, true)
      html should include("xxx")
    }
  }

  describe("GET valid url") {

    describe("User is a guest") {
      it("should ask for password when user is a guest") {
        val s = new ShareView(URL)
        s.initialPage shouldBe s.PasswordRequired
      }
    }

    describe("User is the project owner") {

      def test(f: ShareView => Unit): Unit =
        MockDaoProvider(dao => {
          when(dao.findShareAndProject(URL)).thenReturn(Some(share, project))
        }).install {
          withUserLoggedIn(Some(UD1)) {
            val s = new ShareView(URL)
            f(s)
          }
        }

      it("should display the UCs (when there are none)") {
        test(s => {
          s.loadUcs = (p: ProjectId, f: UcFilter) => List.empty
          s.initialPage shouldBe a[s.ZeroUcs]
        })
      }

      it("should display the UCs (when there are some)") {
        test(s => {
          s.loadUcs = (p: ProjectId, f: UcFilter) => List(cp)
          s.initialPage shouldBe a[s.ShowUcs]
        })
      }
    }

    describe("User is logged-in and unrelated") {
      it("should ask for password when user is a guest") {
        MockDaoProvider(dao => {
          when(dao.findShareAndProject(URL)).thenReturn(Some(share, project))
        }).install {
          withUserLoggedIn(Some(UD2)) {
            val s = new ShareView(URL)
            s.initialPage shouldBe s.PasswordRequired
          }
        }
      }
    }
  }

  describe("GET invalid url") {
    it("should act as if valid") {
      MockDaoProvider(dao => {
        when(dao.findShareAndProject(URL)).thenReturn(None)
      }).install {
        withUserLoggedIn(Some(UD1)) {
          val s = new ShareView(URL)
          s.initialPage shouldBe s.PasswordRequired
        }
      }
    }
  }
}
