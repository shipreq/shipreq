//package shipreq.webapp.base.hash2
//
//import japgolly.univeq.UnivEq
//import shipreq.base.util.IMap
//import shipreq.webapp.base.hash.Hash
//
//// Scope/Leaf/Subset/Space = objs
//
////object HashScope {
////
//////  case object WholeProject        extends HashScope
//////  case object   Config            extends HashScope
//////  case object     CfgIssueTypes   extends HashScope
//////  case object     CfgReqTypes     extends HashScope
//////  case object     CfgFields       extends HashScope
//////  case object     CfgTags         extends HashScope
//////  case object   Content           extends HashScope
//////  case object     Reqs            extends HashScope
//////  case object       GenericReqs   extends HashScope
//////  case object       UseCases      extends HashScope
//////  case object       PubidRegister extends HashScope
//////  case object     ReqCodes        extends HashScope
//////  case object     TextFieldData   extends HashScope
//////  case object     TagData         extends HashScope
//////  case object     ImplicationData extends HashScope
//////  case object     DeletionReasons extends HashScope
//////  case object   Other             extends HashScope
////
////  sealed trait Leaf
////
////  object Leaf {
////    case object Name      extends Leaf
////    case object SavedViews extends Leaf
////  }
////}
//
///*
//
//ScopeLeaf (eg. SavedViews)
//Hash[-A] (i.e. A => Int)
//
//ScopeLeafHasher[S, A] = ( S , Hash[-A] )
//
//Scheme  [S, A] = IMap[S, ScopeLeafHasher[S, A]]
//SchemeI [S, A] = ( SchemeId, Scheme[S, A] )
//
//
//Scheme[A] => Hash[A] --- aggregate all active scopes
//
//*/
//
//
////sealed trait HashScope
////
////object HashScope {
////  case object CfgIssueTypes   extends HashScope
////  case object CfgReqTypes     extends HashScope
////  case object CfgFields       extends HashScope
////  case object CfgTags         extends HashScope
////  case object GenericReqs     extends HashScope
////  case object UseCases        extends HashScope
////  case object PubidRegister   extends HashScope
////  case object ReqCodes        extends HashScope
////  case object TextFieldData   extends HashScope
////  case object TagData         extends HashScope
////  case object ImplicationData extends HashScope
////  case object DeletionReasons extends HashScope
////  case object ProjectName     extends HashScope
////  case object SavedViews      extends HashScope
////}
//
////HashRec(logicVer: LogicVer    : char -- The version of event application logic used before calculating the hash.
////        scope   : HashScope   : char -- Indicates the Project subset covered by the hash.
////        scheme  : HashScheme  : char -- The version of hash calculation logic used to generate the hash.
////        hash    : Option[Int] : int  -- Hash value, or NULL to disable integrity checking.
//
//object Generic {
//
////  final case class ScopeHash[S, A](scope: S, hash: Hash[A]) {
////    def contramap[B](f: B => A): ScopeHash[S, B] =
////      ScopeHash(scope, hash cmap f)
////  }
//
//  case class HashFailure(actual: Int, expected: Int) {
//    assert(actual != expected)
//  }
//
//  final case class Scheme[S, A](scopeHashers: Map[S, HashFn[A]]) {
//
//    def check(dataBefore: Option[A], dataNow: A, recs: Map[S, Option[Int]]): Map[S, HashFailure] =
//      // warn about irrelavent scopes with values
//      // compare hashes for each relevant scope
//      // Hash=None == force pass
//      // missing entry for scope means no change to scope from before
//      ???
//  }
//
//  sealed trait EvolutionOp[S, A]
//  object EvolutionOp {
//    final case class Add   [S, A](scope: S, value: HashFn[A]) extends EvolutionOp[S, A]
//    final case class Evolve[S, A](scope: S, value: HashFn[A]) extends EvolutionOp[S, A]
//    final case class Drop  [S, A](scope: S) extends EvolutionOp[S, A]
//  }
//
//  final case class Schemes[I, S, A](schemes: Map[I, Scheme[S, A]], latestId: I, nextId: I => I) {
//    assert(schemes.contains(latestId))
//
//    val latest = schemes(latestId)
//
//    def evolve(op: EvolutionOp[S, A]): Schemes[I, S, A] = {
//
//      val newId = nextId(latestId)
//      assert(!schemes.contains(newId))
//
//      val newScheme: Scheme[S, A] =
//        op match {
//          case EvolutionOp.Add(s, h) =>
//            assert(!latest.scopeHashers.contains(s), s"Latest scheme already contains scope: $s")
//            Scheme(latest.scopeHashers.updated(s, h))
//          case EvolutionOp.Evolve(s, h) =>
//            assert(latest.scopeHashers.contains(s), s"Latest scheme doesn't contain scope: $s")
//            Scheme(latest.scopeHashers.updated(s, h))
//          case EvolutionOp.Drop(s) =>
//            assert(latest.scopeHashers.contains(s), s"Latest scheme doesn't contain scope: $s")
//            Scheme(latest.scopeHashers - s)
//        }
//
//      val newSchemes = schemes.updated(newId, newScheme)
//
//      Schemes(newSchemes, newId, nextId)
//    }
//  }
//
//  object Schemes {
//    def init[I: UnivEq, S: UnivEq, A](initialId: I)(nextId: I => I)(initialValue: Scheme[S, A]): Schemes[I, S, A] =
//      apply(UnivEq.emptyMap[I, Scheme[S, A]].updated(initialId, initialValue), initialId, nextId)
//  }
//
//  // evolve hash schemes
//  // - bunch of hashes in DB all with previous hash scheme n-1
//  // - group all db hash recs by hash scheme, then use (hashScheme(id): Scheme[I, S, A]).check(...)
//
//  // - for new recs with new hash scheme
//  // - need to work out how to handle missing values, the schema should
//
//
//}