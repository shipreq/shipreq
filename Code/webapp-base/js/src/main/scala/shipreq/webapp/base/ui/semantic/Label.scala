package shipreq.webapp.base.ui.semantic

object Label {

//  sealed abstract class Dir(cls: ClassName) extends HasClass(cls)
//  object Dir {
//    case object Up    extends Dir(NoClass)
//    case object Down  extends Dir("below")
//    case object Left  extends Dir("left")
//    case object Right extends Dir("right")
//    implicit def univEq: UnivEq[Dir] = UnivEq.derive
//  }
//  case class Pointing(dir: Dir) extends Type("pointing" <+ dir)

  sealed abstract class Type(cls: ClassName) extends HasClass(cls)
  object Type {
    case object Basic              extends Type("basic")
    case object BasicPointingUp    extends Type("basic pointing")
    case object BasicPointingDown  extends Type("basic pointing below")
    case object BasicPointingLeft  extends Type("basic pointing left")
    case object BasicPointingRight extends Type("basic pointing right")
    case object Default            extends Type(NoClass)
    case object Floating           extends Type("floating")
    case object Horizontal         extends Type("horizontal")
    case object PointingUp         extends Type("pointing")
    case object PointingDown       extends Type("pointing below")
    case object PointingLeft       extends Type("pointing left")
    case object PointingRight      extends Type("pointing right")
    case object RibbonLeft         extends Type("ribbon")
    case object RibbonRight        extends Type("ribbon right")
    case object Tag                extends Type("tag")
    implicit def univEq: UnivEq[Type] = UnivEq.derive
  }

  case class Style(tipe  : Type   = Type.Default,
                   colour: Colour = Colour.Default,
                   size  : Size   = Size.Default) {

    val div = divCls("ui label" <+ tipe <+ colour <+ size)
  }

}
