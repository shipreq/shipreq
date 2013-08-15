package com.beardedlogic.usecase
package law

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._
import org.scalatest.FunSuite
import org.scalatest.prop._
import lib._
import field._
import model._
import test.TestDatabaseSupport
import test.DataGenerators._
import Types._
import UseCaseFns._

class UseCaseLaws extends FunSuite with TestDatabaseSupport with Checkers {

  def runs = 10

  test("load(save(uc)) = uc") {check(SaveAndLoad, MinSuccessful(runs))}

  test("save(save(uc)) = NOP") {check(SecondSaveIsNop, MinSuccessful(runs))}

  // -------------------------------------------------------------------------------------------------------------------

  lazy val SaveAndLoad = forAll((uc: UseCase) => timedDbProp("SaveAndLoad", uc) {
    uc <==> saveAndLoad(uc).uc
  })

  lazy val SecondSaveIsNop = forAll((uc: UseCase) => timedDbProp("SecondSaveIsNop", uc) {
    val cp1 = saveAndLoad(uc)
    val (save2, diffs) = collectTableDiffs {save(cp1.uc, Some(cp1))}
    val tableChanges = diffs.filterNot {case (_, diff) => diff == 0}
    (tableChanges.isEmpty && save2.isEmpty) :| s"Shouldn't save the second time. UC content =\n${uc.toPrettyString}"
  })

  // -------------------------------------------------------------------------------------------------------------------

  def save(uc: UseCase, prev: Option[UseCaseSaveCheckpoint]): Option[UseCaseSaveCheckpoint] =
    UseCasePersistence.save(uc, prev, db)

  def load(ucRev: UseCaseRev) =
    Locks.UseCase.withReadLockToken(ucRev)(UseCasePersistence.load(ucRev, db, _))

  def saveAndLoad(uc: UseCase, prev: Option[UseCaseSaveCheckpoint] = None) =
    load(save(uc, prev).getOrElse(prev.get).rec)

  // -------------------------------------------------------------------------------------------------------------------

  // Each check pass will run in its own transaction
  override val wrapTestsInTransaction = false

  implicit lazy val arbUseCase: Arbitrary[UseCase] = Arbitrary(useCaseGen(Defaults.FieldList.get))

  def all(ps: Seq[Prop]) = Prop.all(ps: _*)

  def failMsg(name: String)(a: Any, b: Any) = s"$name failure:\n A=$a\n B=$b"

  def equal[T](name: => String)(a: T, b: T): Prop = (a == b) :|| failMsg(name)(a, b)

  def timedProp(name: String, uc: UseCase)(f: => Prop): Prop =
    time(t => debug("%s: %6d steps in %.2f sec.".format(name, uc.stepsAndLabels.get.size, t)))(f)

  def timedDbProp(name: String, uc: UseCase)(f: => Prop): Prop =
    timedProp(name, uc) {rollbackAfter(f)}

  implicit class UseCaseExt(val x: UseCase) {
    def <==>(y: UseCase): Prop = equalUseCases(x, y)
    def =/=(y: UseCase): Prop = equalUseCases(x, y).flatMap(_.failure :| "Use cases are supposed to differ.")
    def textFields: List[TextField] = filter[TextField](x.fields)
    def stepFields: List[StepField] = filter[StepField](x.fields)
  }

  def makeSingleLine(t: String) = t.replace("\n", "\\n")

  def extractStepText(u: UseCase)(f: StepField): Map[LabelStr, String] =
    f(u.fieldValues).textByLabels(u.stepsAndLabels) mapValues makeSingleLine

  def equalHeader(a: UseCase, b: UseCase) = equal("Header")(a.header, b.header)

  def equalTextFields(a: UseCase, b: UseCase) = all(
    for ((fa, fb) <- a.textFields.zip(b.textFields))
    yield equal("Text field")(fa(a.fieldValues).text, fb(b.fieldValues).text)
  )

  def equalStepFields(a: UseCase, b: UseCase) = {
    val z = Map.empty[LabelStr, String]
    val aSteps = a.stepFields.map(extractStepText(a)).foldLeft(z)(_ ++ _)
    val bSteps = b.stepFields.map(extractStepText(b)).foldLeft(z)(_ ++ _)
    val sizeProp = equal("Step count")(aSteps.size, bSteps.size)
    val stepProp = equal("Steps")(aSteps, bSteps)
    //    val stepProps = for ((al, at) <- aSteps) yield equal("Step field")(x, y)
    //    all {sizeProp +: stepProps.toList}
    sizeProp && stepProp
  }

  def equalUseCases(a: UseCase, b: UseCase) = (
    equalHeader(a, b)
      && equalTextFields(a, b)
      && equalStepFields(a, b)
    )
}
