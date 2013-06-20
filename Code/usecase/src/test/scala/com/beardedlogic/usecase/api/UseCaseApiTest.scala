package com.beardedlogic.usecase
package api

import org.scalatest.FunSpec
import org.scalatest.prop._
import net.liftweb.json.JsonDSL._
import net.liftweb.json._
import net.liftweb.http.testing.HttpResponse
import lib.{ExternalId, Defaults}
import model.{UseCase, UseCaseSummary, DAO}
import test.LiveTest
import test.LiveTestHelpers._

class UseCaseApiTest extends FunSpec with LiveTest with PropertyChecks {

  def goodRequestData(title: String): JValue = ("title" -> title)

  def urlFor(uc: UseCase) = "/api/usecase/" + ExternalId.toExternal(uc.valueId)

  def withUc[U](block: (DAO, UseCase) => U): U = {
    DAO.withSession(dao => {
      val uc = dao.createInitialUseCase(Defaults.Title, Defaults.FieldList.get)
      block(dao, uc)
    })
  }

  // TODO Test UCC.save corrects UC titles too
  describe("PUT /api/usecase/ID") {
    def testSuccess(title: String, expectedTitleAfterSave: String): Option[UseCaseSummary] =
      withUc((dao, uc1) => testSuccess2(dao, uc1, title, expectedTitleAfterSave))

    def testSuccess2(dao: DAO, uc1: UseCase, title: String, expectedTitleAfterSave: String): Option[UseCaseSummary] =
      for {
        r <- jsonPut(urlFor(uc1), goodRequestData(title)) ! 200
        uc2 <- r.expectJson[UseCaseSummary]
      } yield {
        uc2.number should equal(uc1.number)
        uc2.title should equal(expectedTitleAfterSave)
        dao.findAllUseCaseSummaries() should contain(uc2)
        uc2
      }

    it("should 200 with new UC on success") {
      testSuccess("great", "great")
    }

    it("should correct invalid titles") {
      val examples = Table(("INPUT", "OUTPUT")
        , ("   omg   ", "omg")
        , ("what     about", "what about")
        , ("what\tabout", "what about")
        , ("\tgreat  work\n", "great work")
        , ("", Defaults.Title) // NOP actually
        , ("    ", Defaults.Title) // NOP actually
      )
      forAll(examples)(testSuccess(_, _))
    }

    it("should 200 when no change") {
      DAO.withSession(dao => {
        val uc1 = dao.createInitialUseCase(Defaults.Title, Defaults.FieldList.get)
        val uc2s = testSuccess2(dao, uc1, "hello", "hello").get
        val uc2 = dao.findUseCase(uc2s.valueId).get
        testSuccess2(dao, uc2, uc2.title, uc2.title)
        // TODO timestamp should be old
      })
    }

    it("should 404 when UC not found") {
      jsonPut("/api/usecase/987654321", goodRequestData("blah")) ! 404
      jsonPut("/api/usecase/__", goodRequestData("blah")) ! 404
    }

    it("should 400 when input data invalid") {
      withUc((_, uc) => jsonPut(urlFor(uc), ("titlezzz" -> 123)) ! 400)
    }

    // TODO test 428
    ignore("should 428 when UC rev not latest") {
      withUc((dao, uc1) => {
      })
    }

    it("should withstand current updates") {
      withUc((dao, uc) => {
        (1 to 100).par.map{i=> {
          //          import sys.process._
          //          val cmd = s"""curl -X PUT -H Content-Type:application/json --data {"title":"asd$i"} -w %{http_code} http://localhost:8080${urlFor(uc)}""" #| "tail -1" #| "perl -pe s/\\D+//g"
          //          val code = cmd.!!.trim.toInt
          val code = jsonPut(urlFor(uc), goodRequestData("asdg"+i)).asInstanceOf[HttpResponse].code
          Seq(200,428) should contain(code)
        }}
      })
    }
  }
}
