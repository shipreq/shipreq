package shipreq.intellij

import scala.reflect.ClassTag

object Utils {

  implicit class AnyExt[A](private val a: A) extends AnyVal {
    def tryCast[B <: A](implicit c: ClassTag[B]): Option[B] =
      c.unapply(a)
  }

  def fakeCaseClass1(cls: String, arg1Name: String, arg1Type: String, mod: String = "", ext: String = "")(body: String): List[String] =
    s"$mod class $cls(val $arg1Name: $arg1Type) extends ${Option(ext).filter(_.nonEmpty).fold("")(_ + " with")} Product with Serializable {$body}" ::
    s"object $cls{def apply($arg1Name: $arg1Type): $cls = ???; def unapply(x: $cls): Option[$arg1Type] = ???}" ::
    Nil

}
