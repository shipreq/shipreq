// rgs --color=never -l 'extends TestSuite' webapp-client-project/src/test | perl -pe 's/.*?(?=shipreq)//; s/\.scala//; s!/!.!g; s!^!;webappClientProject/testOnly !' | sort >> wcp.sbt

addCommandAlias("wcpt",
"""

;webappClientProject/testOnly shipreq.webapp.client.project.app.GlobalTest
;webappClientProject/testOnly shipreq.webapp.client.project.app.pages.config.fields.DerivativeTagRuleEditorTest
;webappClientProject/testOnly shipreq.webapp.client.project.app.pages.config.fields.FieldConfigTest
;webappClientProject/testOnly shipreq.webapp.client.project.app.pages.config.issues.IssueConfigTest
;webappClientProject/testOnly shipreq.webapp.client.project.app.pages.config.reqtypes.ReqTypeConfigTest
;webappClientProject/testOnly shipreq.webapp.client.project.app.pages.config.tags.TagConfigTest
;webappClientProject/testOnly shipreq.webapp.client.project.app.pages.content.issues.IssuesPageTest
;webappClientProject/testOnly shipreq.webapp.client.project.app.pages.content.reqdetail.ReqDetailTest
;webappClientProject/testOnly shipreq.webapp.client.project.app.pages.content.reqgraph.ReqGraphTest
;webappClientProject/testOnly shipreq.webapp.client.project.app.pages.content.reqtable.LogicPropTest
;webappClientProject/testOnly shipreq.webapp.client.project.app.pages.content.reqtable.LogicTest
;webappClientProject/testOnly shipreq.webapp.client.project.app.pages.content.reqtable.ReqTableTest
;webappClientProject/testOnly shipreq.webapp.client.project.app.pages.content.reqtable.ReqTableTest2
;webappClientProject/testOnly shipreq.webapp.client.project.app.pages.root.ProjectHomeTest
;webappClientProject/testOnly shipreq.webapp.client.project.app.ProjectSpaTest
;webappClientProject/testOnly shipreq.webapp.client.project.app.state.ProjectStateTest
;webappClientProject/testOnly shipreq.webapp.client.project.feature.deletion.DeletionLogicTest
;webappClientProject/testOnly shipreq.webapp.client.project.feature.deletion.RestorationLogicTest
;webappClientProject/testOnly shipreq.webapp.client.project.feature.savedview.SavedViewLogicTest
;webappClientProject/testOnly shipreq.webapp.client.project.widgets.editors_with_controls.ReqCodeEditorTest
;webappClientProject/testOnly shipreq.webapp.client.project.widgets.FilterEditorTest
;webappClientProject/testOnly shipreq.webapp.client.project.widgets.UserDefinedGraphTest

""".replace('\n', ' ').trim
)
