package shipreq.webapp.base.test

import shipreq.webapp.base.protocol._

object MockRemotes {

  lazy val projectSpa =
    ProjectSpa(RemoteFn.Instance("projectInit"  , ProjectInit          ),
               RemoteFn.Instance("issueTypeCrud", CustomIssueTypeCrud  ),
               RemoteFn.Instance("reqTypeCrud"  , CustomReqTypeCrud    ),
               RemoteFn.Instance("reqTypeImpMod", ReqTypeImplicationMod),
               RemoteFn.Instance("fieldMandMod" , FieldMandatorinessMod),
               RemoteFn.Instance("fieldCrud"    , FieldCrud.Fn         ),
               RemoteFn.Instance("tagCrud"      , TagCrud.Fn           ),
               RemoteFn.Instance("createContent", CreateContentFn      ),
               RemoteFn.Instance("updateContent", UpdateContentFn      ))
}
