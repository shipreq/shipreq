package shipreq.webapp.member.feature.autocomplete.strategies

import shipreq.webapp.member.feature.AutoCompleteFeature._
import shipreq.webapp.member.feature.autocomplete.strategies.AutoCompleteTestData._
import shipreq.webapp.member.feature.autocomplete.strategies._
import shipreq.webapp.member.project.data.derivation.NaTags
import shipreq.webapp.member.project.data.{Contextualise, Plain, _}
import shipreq.webapp.member.project.text.Text
import shipreq.webapp.member.test.project.SampleProject3._

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
  implicit def autoCompleteStyle = ReqItem.Style.IdAndTitle
  override implicit val strategies = {
    val reqItems = ProjectStrategies.reqItems(project, plainText)
    ProjectStrategies.req(reqItems, textSearch).apply(Contextualise)
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
