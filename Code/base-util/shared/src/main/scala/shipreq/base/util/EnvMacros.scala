package shipreq.base.util

import japgolly.microlibs.macro_utils.MacroUtils
import scala.reflect.macros.blackbox

object EnvMacros {

  def isReleaseMode: Boolean =
    macro EnvMacroImpls.isReleaseMode

  def devOrRel[A](dev: A, rel: A): A =
    macro EnvMacroImpls.devOrRel[A]
}

class EnvMacroImpls(val c: blackbox.Context) extends MacroUtils {
  import c.universe._

  def sbtInReleaseMode: Boolean =
    sys.props get "MODE" contains "release"

  def isReleaseMode: c.Expr[Boolean] =
    c.Expr(Literal(Constant(sbtInReleaseMode)))

  def devOrRel[A](dev: c.Expr[A], rel: c.Expr[A]): c.Expr[A] =
    if (sbtInReleaseMode) rel else dev
}
