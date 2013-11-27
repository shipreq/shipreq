package com.beardedlogic.shipreq.util

import net.liftweb.util.Props
import net.liftweb.common.{Box, Full, Failure}

final object RuntimeProps {

  final case class PropScope(run: String => String)
  val GlobalScope = PropScope(identity)

  def tryS(name: String, default: String )(implicit n: PropScope): String  = Props get     n.run(name) openOr default
  def tryI(name: String, default: Int    )(implicit n: PropScope): Int     = Props getInt  n.run(name) openOr default
  def tryL(name: String, default: Long   )(implicit n: PropScope): Long    = Props getLong n.run(name) openOr default
  def tryB(name: String, default: Boolean)(implicit n: PropScope): Boolean = Props getBool n.run(name) openOr default

  def wantS(name: String)(implicit n: PropScope): Box[String]  = Props.get    (n.run(name)) ?~ s"Property not found: ${n.run(name)}"
  def wantI(name: String)(implicit n: PropScope): Box[Int]     = Props.getInt (n.run(name)) ?~ s"Property not found: ${n.run(name)}"
  def wantL(name: String)(implicit n: PropScope): Box[Long]    = Props.getLong(n.run(name)) ?~ s"Property not found: ${n.run(name)}"
  def wantB(name: String)(implicit n: PropScope): Box[Boolean] = Props.getBool(n.run(name)) ?~ s"Property not found: ${n.run(name)}"

  @inline private def need[T](box: Box[T]): T = box match {
    case Full(t)            => t
    case Failure(msg, _, _) => throw new RuntimeException(msg)
    case _                  => throw new RuntimeException("Missing property: " + box)
  }
  def needS(name: String)(implicit n: PropScope): String  = need(wantS(name))
  def needI(name: String)(implicit n: PropScope): Int     = need(wantI(name))
  def needL(name: String)(implicit n: PropScope): Long    = need(wantL(name))
  def needB(name: String)(implicit n: PropScope): Boolean = need(wantB(name))

  def useIfDefinedS(name: String, f: String  => Unit)(implicit n: PropScope): Unit = wantS(name) foreach f
  def useIfDefinedI(name: String, f: Int     => Unit)(implicit n: PropScope): Unit = wantI(name) foreach f
  def useIfDefinedL(name: String, f: Long    => Unit)(implicit n: PropScope): Unit = wantL(name) foreach f
  def useIfDefinedB(name: String, f: Boolean => Unit)(implicit n: PropScope): Unit = wantB(name) foreach f

}
