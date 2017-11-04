package shipreq.webapp.base

import shipreq.webapp.base.data.Project

package object hash {

  object ProjectHashModule extends EvoHashModule[HashScope, Project] {

    override val schemeRegistry =
      initSchemes(
        HashScope.ProjectName     --> ProjectHasher.hashProjectName,
        HashScope.CfgIssueTypes   --> ProjectHasher.hashCustomIssueTypes,
        HashScope.CfgReqTypes     --> ProjectHasher.hashReqTypes,
        HashScope.CfgFields       --> ProjectHasher.hashFieldSet,
        HashScope.CfgTags         --> ProjectHasher.hashTagTree,
        HashScope.GenericReqs     --> ProjectHasher.hashGenericReqs,
        HashScope.UseCases        --> ProjectHasher.hashUseCases,
        HashScope.PubidRegister   --> ProjectHasher.hashPubidRegister,
        HashScope.ReqCodes        --> ProjectHasher.hashReqCodes,
        HashScope.TextFieldData   --> ProjectHasher.hashReqDataText,
        HashScope.TagData         --> ProjectHasher.hashReqDataTags,
        HashScope.ImplicationData --> ProjectHasher.hashImplications,
        HashScope.DeletionReasons --> ProjectHasher.hashDeletionReasons,
        HashScope.SavedViews      --> ProjectHasher.hashSavedViews)
  }

  type HashRecsForScheme = EvoHashModule.ScopeMap[HashScope, Option[Int]]
  type HashRecs          = ProjectHashModule.HashRecs
  type HashScheme        = ProjectHashModule.Scheme
  val  HashSchemes       = ProjectHashModule.schemeRegistry
}
