package shipreq.webapp.base.hash

import utest._
import scala.collection.immutable.ListSet
import shipreq.webapp.base.event.ApplyEvent.LogicVer
import shipreq.webapp.base.test.BaseTestUtil._

object HashRecTest extends TestSuite {

  implicit def listHash(h: Int): Option[Int] = Some(h)

  def hr(scope   : HashScope,
         logicVer: LogicVer   = LogicVer.Current,
         scheme  : HashScheme = HashScheme.latest)
        (hash    : Option[Int]) =
    HashRec(scope, logicVer, scheme)(hash)

  val wp1 = hr(HashScope.WholeProject)(101)
  val wp2 = hr(HashScope.WholeProject)(102)

  val dr1 = hr(HashScope.DeletionReasons)(111)
  val dr2 = hr(HashScope.DeletionReasons)(112)

  val ct1 = hr(HashScope.CfgTags)(121)
  val ct2 = hr(HashScope.CfgTags)(122)

  val ci1 = hr(HashScope.CfgIssueTypes)(131)
  val ci2 = hr(HashScope.CfgIssueTypes)(132)

  val cfg1 = hr(HashScope.Config)(141)
  val cfg2 = hr(HashScope.Config)(142)

  val tf1 = hr(HashScope.TextFieldData)(151)
  val tf2 = hr(HashScope.TextFieldData)(152)

  override def tests = TestSuite {

    'merge {
      def test(earlier: HashRec*)(later: HashRec*)(expect: HashRec*): Unit = {
        val a = HashRec.merge(earlier.to[ListSet], later.to[ListSet])
        val e = expect.to[ListSet]
        assertSet(actual = a, expect = e)
      }

      'nop {
        test(        )()(        )
        test(wp1     )()(wp1     )
        test(wp1, dr1)()(wp1, dr1)
      }

      'repeat {
        test(wp1     )(wp1     )(wp1     )
        test(wp1, dr1)(wp1, dr1)(wp1, dr1)
      }

      'same {
        test(dr1)(dr2)(dr2)
      }

      'larger {
        test(dr1     )(wp1)(wp1)
        test(dr1, ct1)(wp1)(wp1)
      }

      'smaller {
        test(wp1)(dr1     )(dr1     )
        test(wp1)(dr1, ct1)(dr1, ct1)
      }

      'unrelated {
        test(dr1)(ct1)(dr1, ct1)
      }

      'unrelatedAndSame {
        test(dr1, ct1)(ci1, ct2)(dr1, ci1, ct2)
      }

      'unrelatedAndLarger {
        test(dr1, ct1)(tf1, cfg2)(dr1, tf1, cfg2)
      }

      'unrelatedAndSmaller {
        test(dr1, cfg1)(tf1, ct2)(dr1, tf1, ct2)
      }

    }
  }
}
