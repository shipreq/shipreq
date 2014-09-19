import utest._
// import utest.ExecutionContext.RunNow

object Test extends TestSuite {
  def tests = TestSuite {
    'test2{
      * - {1 == 1}
      * - {2 == 2}
      * - {3 == 3}
    }
  }
}
