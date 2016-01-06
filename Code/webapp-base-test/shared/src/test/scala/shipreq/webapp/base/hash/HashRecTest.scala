package shipreq.webapp.base.hash

import utest._
import scala.collection.immutable.ListSet
import shipreq.base.util.{Valid, Invalid}
import shipreq.webapp.base.event.ApplyEvent.LogicVer
import shipreq.webapp.base.test.WebappTestUtil._

object HashRecTest extends TestSuite {

  implicit def listHash(h: Int): Option[Int] = Some(h)

  def hr(scope   : HashScope,
         logicVer: LogicVer   = LogicVer.Current,
         scheme  : HashScheme = HashScheme.latest)
        (hash    : Option[Int]) =
    HashRec(scope, logicVer, scheme)(hash)

  val oldScheme = HashTestUtil.fakeHashSchemeV(1, {case HashScope.ImplicationData => Invalid; case _ => Valid})

  val wp1 = hr(HashScope.WholeProject)(101)
  val wp2 = hr(HashScope.WholeProject)(102)
  val wp3 = hr(HashScope.WholeProject, scheme = oldScheme)(103)

  val dr1 = hr(HashScope.DeletionReasons)(111)
  val dr2 = hr(HashScope.DeletionReasons)(112)
  val dr3 = hr(HashScope.DeletionReasons, scheme = oldScheme)(113)

  val ct1 = hr(HashScope.CfgTags)(121)
  val ct2 = hr(HashScope.CfgTags)(122)
  val ct3 = hr(HashScope.CfgTags, scheme = oldScheme)(123)

  val ci1 = hr(HashScope.CfgIssueTypes)(131)
  val ci2 = hr(HashScope.CfgIssueTypes)(132)
  val ci3 = hr(HashScope.CfgIssueTypes, scheme = oldScheme)(133)

  val cfg1 = hr(HashScope.Config)(141)
  val cfg2 = hr(HashScope.Config)(142)
  val cfg3 = hr(HashScope.Config, scheme = oldScheme)(143)

  val tf1 = hr(HashScope.TextFieldData)(151)
  val tf2 = hr(HashScope.TextFieldData)(152)
  val tf3 = hr(HashScope.TextFieldData, scheme = oldScheme)(153)

  val id1 = hr(HashScope.ImplicationData)(161)
  val id2 = hr(HashScope.ImplicationData)(162)

  // TODO Handle & test changes in LogicVer

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

      'scope {
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

      'scheme {
        'sameScope {
          test(wp3     )(wp1     )(wp1     )
          test(wp3, tf3)(wp1, tf1)(wp1, tf1)
        }

        'larger {
          test(dr3     )(wp1)(wp1)
          test(dr3, ct3)(wp1)(wp1)
        }

        'smaller {
          test(wp3)(dr1     )(dr1     )
          test(wp3)(dr1, ct1)(dr1, ct1)
        }

        'unrelated {
          test(dr3)(ct1)(dr3, ct1)
        }

        'unrelatedAndSame {
          test(dr3, ct3)(ci1, ct2)(dr3, ci1, ct2)
        }

        'unrelatedAndLarger {
          test(dr3, ct3)(tf1, cfg2)(dr3, tf1, cfg2)
        }

        'unrelatedAndSmaller {
          test(dr3, cfg3)(tf1, ct2)(dr3, tf1, ct2)
        }
      }

      'newScopeAfterScheme {
        // Suppose the ImplicationData scope was added after `oldScheme`...
        test(id1, dr1, ct1)(ct2)(dr1, ct2, id1) // ct2 is latest scheme which doesn't invalidate id<n>
        test(id1, dr1, ct1)(ct3)(dr1, ct3)      // oldScheme invalidates id<n>
        test(id1, dr1     )(ct3)(dr1, ct3)      // oldScheme invalidates id<n>
      }

    }
  }
}
