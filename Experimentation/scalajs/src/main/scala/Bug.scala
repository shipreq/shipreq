//import scala.scalajs.js
//
//object Bug extends js.JSApp {
//
//  trait AAA[I, C, O]
//  trait BBB[D, V]
//  type VVV = Boolean
//  def StrAAA: AAA[String, String, String] = null
//  def StrBBB: BBB[String, VVV] = null
//
//  case class SSS[P, I, C, O](pc: P => C, v: AAA[I, C, O]) {
//    def edit[V](e: BBB[I, V]) = SSSE(this, e)
//  }
//
//  case class SSSE[P, V, I, C, O](s: SSS[P, I, C, O], b: BBB[I, V])
//
//  def SSI[P] = new {
//    def apply[C](pc: P => C) = new {
//      def apply[I, O](v: AAA[I, C, O])(e: BBB[I, VVV]) =
//        SSS(pc, v).edit(e)
//    }
//  }
//
//  def SSO[P] = new {
//    def apply[V, I1, C1, O1](s1: SSSE[P, V, I1, C1, O1]) = s1
//  }
//
//  case class P(a: String, b: Int)
//
//  val x = SSO[P](SSI[P](_.a)(StrAAA)(StrBBB))
//
//  override def main(): Unit = ()
//}
