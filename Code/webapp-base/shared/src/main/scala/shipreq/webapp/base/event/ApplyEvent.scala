package shipreq.webapp.base.event

import ApplyEventLib._
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.data.{Validators => V, _}
import DataImplicits._
import DeletionAction._

class ApplyEvent(implicit val trust: Trust) {

  // TODO Doesn't check DataProp at the end
  def apply(event: Event): AP =
    event match {
      case e: CreateCustomReqType => CustomReqTypeEvents applyCreate e
      case e: UpdateCustomReqType => CustomReqTypeEvents applyUpdate e
      case e: DeleteCustomReqType => CustomReqTypeEvents applyDelete e
    }

  // ===================================================================================================================
  object CustomReqTypeEvents extends GenericDataApp {
    override val ^ = CustomReqTypeGD
    override type Data = CustomReqType

    val L = Project.customReqTypes ^|-> RevAnd.data
    val imap = IMapApp.data(CustomReqType)

    // Doesn't check mnemonic/name uniqueness - DataProp will do that
    val validateName     = validateWith (V.reqType.nameU)
    val validateMnemonic = validateWithF(V.reqType.mnemonicU)(_.value)

    val readName     = need(^.Name)     >=> validateName
    val readMnemonic = need(^.Mnemonic) >=> validateMnemonic
    val readImp      = need(^.Imp)

    val updateName     = updateL(CustomReqType.name)              <<=< validateName
    val updateMnemonic = updateF[Data, Mnemonic](_ setMnemonic _) <<=< validateMnemonic
    val updateImp      = updateL(CustomReqType.imp)
    val updateLive     = updateL(CustomReqType.live)

    val updateValues = updateEachValue {
      case v: ^.ValueForName     => updateName    (v.value)
      case v: ^.ValueForImp      => updateImp     (v.value)
      case v: ^.ValueForMnemonic => updateMnemonic(v.value)
    }

    val ensureLive = ensureLiveBy[Data](_.live)

    def applyCreate(e: CreateCustomReqType): AP = {
      implicit val vs = e.vs

      val newObject =
        for {
          n <- readName
          m <- readMnemonic
          i <- readImp
        } yield CustomReqType(e.id, m, Set.empty, n, i, Live)

      L @=> (newObject ?=>> imap.add)
    }

    def applyUpdate(e: UpdateCustomReqType): AP =
      L @=> imap.update(e.id, ensureLive >=> updateValues(e.vs))

    def applyDelete(e: DeleteCustomReqType): AP =
      e.da match {
        case Restore => setLive(e.id, Live)
        case SoftDel => setLive(e.id, Dead)
        case HardDel => L @=> imap.remove(e.id) // TODO verify not in use - or is DataProps enough?
      }

    private def setLive(id: CustomReqTypeId, newValue: Live): AP =
      L @=> imap.update(id, updateLive(newValue))
  }

}
