package com.beardedlogic.shipreq.util

import net.liftweb.util.Props
import net.liftweb.common.{Empty, Box, Full, Failure}
import ExternalValueReader._

object ExternalValueReader {

  case class Retriever[T](run: String => Box[T])

  final case class PropScope(run: String => String)

  val GlobalScope = PropScope(identity)
  def scopeByPrefix(prefix: String) = PropScope(prefix + _)
  def scopeByNS(ns: String) = if (ns.isEmpty) GlobalScope else scopeByPrefix(s"$ns.")

  def getVar[T](name: String, moreNames: String*)(implicit s: PropScope, r: Retriever[T]): Box[T] =
    get(name) or moreNames.collectFirst {
      case n => get(n)
    }.getOrElse(Empty)

  def get[T](name: String)(implicit s: PropScope, r: Retriever[T]): Box[T] =
    r.run(s.run(name))

  def need[T](name: String)(implicit s: PropScope, r: Retriever[T]): T =
    get(name) match {
      case Full(t)            => t
      case Failure(msg, _, _) => throw new RuntimeException(msg)
      case Empty              => throw new RuntimeException(s"Unable to retrieve external value: ${s.run(name)}")
    }

  def useIfAvailable[T](name: String)(f: T => Unit)(implicit s: PropScope, r: Retriever[T]): Unit =
    get(name) foreach f

}

object RuntimePropReaders {
  implicit val retrieverS = Retriever[String] (n => Props.get(n)     ?~ s"Property not found: $n")
  implicit val retrieverI = Retriever[Int]    (n => Props.getInt(n)  ?~ s"Property not found: $n")
  implicit val retrieverL = Retriever[Long]   (n => Props.getLong(n) ?~ s"Property not found: $n")
  implicit val retrieverB = Retriever[Boolean](n => Props.getBool(n) ?~ s"Property not found: $n")
}
