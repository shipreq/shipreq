package shipreq.webapp.base.feature.autocomplete.strategies

import shipreq.webapp.base.data.derivation.NaTags
import shipreq.webapp.base.data.{Contextualise, Plain, _}
import shipreq.webapp.base.feature.AutoCompleteFeature._
import shipreq.webapp.base.feature.autocomplete.strategies.AutoCompleteTestUtil._
import shipreq.webapp.base.feature.autocomplete.strategies._
import shipreq.webapp.base.test.SampleProject3._
import shipreq.webapp.base.text.Text

object Issue extends AutoCompleteTestModules.Issue {
  override implicit val strategies =
    HashtagStrategies.forIssuesAndTags(
      issues     = project.config.customIssueTypes.values,
      tags       = Nil,
      filterDead = HideDead)(
      Contextualise)
}

object TagC extends AutoCompleteTestModules.TagC {
  override implicit val strategies =
    HashtagStrategies.forTags(project.config.tags.applicableTagIterator().toList, HideDead)(Contextualise)
}

object TagP extends AutoCompleteTestModules.TagP {
  override implicit val strategies =
    HashtagStrategies.forTags(project.config.tags.applicableTagIterator().toList, HideDead)(Plain)
}

object ReqC extends AutoCompleteTestModules.ReqC {
  override implicit val strategies = {
    val reqItems = ProjectStrategies.reqItems(project, plainText)
    ProjectStrategies.req(reqItems, textSearch)(Contextualise)
  }
}

object ReqCodePrefixes extends AutoCompleteTestModules.ReqCodePrefixes {
  override implicit val strategies =
    ReqCodeStrategies.prefixes(fakeTrie)
}

object ReqCodeRefs extends AutoCompleteTestModules.ReqCodeRefs {
  override implicit val strategies = {
    val candidates = ReqCodeStrategies.refCandidates(project2, plainText2)
    RefStrategies(candidates)(Contextualise)
  }
}

object Tex extends AutoCompleteTestModules.Tex {
  override implicit val strategies =
    SmallStrategies.tex
}

// =====================================================================================================================

trait CustomTextField {
  implicit val strategies =
    AutoComplete.Project.richText(Text.CustomTextField, project, NaTags.none, plainText, textSearch)
}

object CustomTextFieldIssue extends AutoCompleteTestModules.Issue with CustomTextField {
  override protected def `#D` =
    super.`#D` ++ Seq("defer", "pri=med", "prod")
}

object CustomTextFieldReq extends AutoCompleteTestModules.ReqC with CustomTextField

object CustomTextFieldTag extends AutoCompleteTestModules.TagC with CustomTextField

object CustomTextFieldTex extends AutoCompleteTestModules.Tex with CustomTextField

object CustomTextFieldReqCodeRefs extends AutoCompleteTestModules.ReqCodeRefs {
  override protected def app =
    super.app :+ "MF-11Collaboration: change mgnt & approval"

  override implicit val strategies =
    AutoComplete.Project.richText(Text.CustomTextField, project2, NaTags.none, plainText2, textSearch2)
}
