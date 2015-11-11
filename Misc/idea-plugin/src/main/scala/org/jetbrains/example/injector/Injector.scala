package org.jetbrains.example.injector

import org.jetbrains.plugins.scala.lang.psi.api.statements.ScValue
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector
import org.jetbrains.plugins.scala.lang.psi.types.result.TypingContext

/**
  * @author Alefas
  * @since  14/10/15
  */
class Injector extends SyntheticMembersInjector {
  override def injectFunctions(source: ScTypeDefinition): Seq[String] = {
    source.members.flatMap {
      case v: ScValue if v.hasAnnotation("example.JavaGetter").isDefined =>
        v.declaredElements.map { td =>
          s"def get${td.name.capitalize} : ${td.getType(TypingContext.empty).getOrAny.canonicalText} = ???"
        }
      case _ => Seq.empty
    }
  }
}