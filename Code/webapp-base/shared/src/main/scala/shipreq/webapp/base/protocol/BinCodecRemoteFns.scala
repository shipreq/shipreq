package shipreq.webapp.base.protocol

import boopickle._
import BoopickleMacros._
import BinCodecGeneric._

object BinCodecRemoteFns {

  private def pickleRemoteFn(fn: RemoteFn): Pickler[fn.Instance] =
    xmap[fn.Instance, String](RemoteFn.Instance(_, fn))(_.key)

  implicit val pickleProjectInit     = pickleRemoteFn(ProjectInit)
  implicit val pickleIssueTypeCrud   = pickleRemoteFn(CustomIssueTypeCrud)
  implicit val pickleReqTypeCrud     = pickleRemoteFn(CustomReqTypeCrud)
  implicit val pickleReqTypeImpMod   = pickleRemoteFn(ReqTypeImplicationMod)
  implicit val pickleFieldMandMod    = pickleRemoteFn(FieldMandatorinessMod)
  implicit val pickleFieldCrud       = pickleRemoteFn(FieldCrud.Fn)
  implicit val pickleTagCrud         = pickleRemoteFn(TagCrud.Fn)
  implicit val pickleCreateContentFn = pickleRemoteFn(CreateContentFn)
  implicit val pickleUpdateContentFn = pickleRemoteFn(UpdateContentFn)

  implicit val pickleProjectSpa      = pickleCaseClass[ProjectSpa]
}
