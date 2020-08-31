package shipreq.fix

import scala.meta.Tree
import scala.meta.inputs.Input
import scalafix.v1._

object Helpers {

  // ===================================================================================================================

  final implicit class TreeExt(private val t: Tree) extends AnyVal {

    def descLoc(implicit sdoc: SemanticDocument): String =
      s"${sdoc.filename}:${t.pos.startLine}:${t.pos.endLine}"
  }

  // ===================================================================================================================

  final implicit class SemanticTreeExt(private val s: SemanticTree) extends AnyVal {

    def arg1SymbolMatches(m: SymbolMatcher): Boolean =
      s match {
        case TypeApplyTree(_, a :: _) => a.symbolMatches(m)
        case _                        => false
      }
  }

  // ===================================================================================================================

  final implicit class SemanticTypeExt(private val s: SemanticType) extends AnyVal {
    def symbol: Option[Symbol] =
      s match {
        case TypeRef   (_, symbol, _) => Some(symbol)
        case SingleType(_, symbol)    => Some(symbol)
        case ThisType  (symbol)       => Some(symbol)
        case SuperType (_, symbol)    => Some(symbol)
        case _                        => None
      }

    def symbolMatches(m: SymbolMatcher): Boolean =
      symbol.fold(false)(m.matches)
  }


  // ===================================================================================================================

  final implicit class SemanticDocumentExt(private val s: SemanticDocument) extends AnyVal {
    def filename: String =
      s.input match {
        case Input.File(path, _)        => path.toFile.getAbsolutePath
        case Input.VirtualFile(path, _) => path
        case i                          => i.toString
      }
  }
  // ===================================================================================================================

  final implicit class SymbolMatcherExt(private val s: SymbolMatcher) extends AnyVal {
    def ignoreErrors = new SymbolMatcherIgnoreErrors(s)
  }

  final class SymbolMatcherIgnoreErrors(s: SymbolMatcher) {

    def unapply(sym: Symbol): Option[Symbol] =
      try
        s.unapply(sym)
      catch {
        case e: Throwable =>
          println(s"SymbolMatcher failed: $e on $sym")
          None
      }

    def unapply(tree: Tree)(implicit sdoc: SemanticDocument): Option[Tree] =
      try
        s.unapply(tree)
      catch {
        case e: Throwable =>
          println(s"SymbolMatcher failed: $e on ${tree.structure} (${tree.descLoc})")
          None
      }
  }
}
