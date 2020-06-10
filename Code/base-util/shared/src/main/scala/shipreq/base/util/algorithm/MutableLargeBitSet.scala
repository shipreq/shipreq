package shipreq.base.util.algorithm

import java.util.Random
import scala.annotation.{elidable, tailrec}

final class MutableLargeBitSet(val bits: Int) {
  import MutableLargeBitSet.BitsPerBlock

  val blocks = Math.ceil(bits.toDouble / BitsPerBlock).toInt
  val data   = new Array[Long](blocks)

  def randomise(rng: Random): this.type = {
    var i = 0
    while (i < blocks) {
      data(i) = rng.nextLong()
      i += 1
    }
    this
  }

  def clear(): this.type = {
    var i = 0
    while (i < blocks) {
      data(i) = 0L
      i += 1
    }
    this
  }

  def soleBit(n: Int): this.type = {
    val block = blocks - 1 - n / BitsPerBlock
    val bit   = n % BitsPerBlock
    val mask  = 1L << bit
    var i = 0
    while (i < blocks) {
      val value = if (i == block) mask else 0L
      data(i) = value
      i += 1
    }
    this
  }

  override def clone(): MutableLargeBitSet = {
    val c = new MutableLargeBitSet(bits)
    java.lang.System.arraycopy(data, 0, c.data, 0, blocks)
    c
  }

  def flipBit(n: Int): Unit = {
    val block = blocks - 1 - n / BitsPerBlock
    val bit   = n % BitsPerBlock
    val mask  = 1L << bit
    data(block) ^= mask
  }

  val bit: Int => Boolean = n => {
    val block = blocks - 1 - n / BitsPerBlock
    val bit   = n % BitsPerBlock
    val mask  = 1L << bit
    // println(s"${self.toBinaryStr(9)}.bit($n) -- $block:$bit = ${Array(self(block)).toBinaryStr(8)} & ${Array(mask).toBinaryStr(9)} = ${Array((self(block) & mask)).toBinaryStr(8)}")
    (data(block) & mask) != 0
  }

  @elidable(elidable.FINE)
  override def toString: String =
    toBinaryStr

  def toBinaryStr: String =
    data.iterator
      .map(b => "%64s".format(java.lang.Long.toBinaryString(b)).replace(' ', '0'))
      .mkString
      .takeRight(bits)

  def fitnessToGoal(goal: MutableLargeBitSet, bits: Int): Double =
    fitnessToGoal(goal, bits, bits.toDouble)

  def fitnessToGoal(goal: MutableLargeBitSet, bits: Int, bitsD: Double): Double = {
    var badBits = 0
    var b = 0
    while (b < bits) {
      if (bit(b) != goal.bit(b))
        badBits += 1
      b += 1
    }
    val f = badBits.toDouble

//    var okBits = 0
//    var b = 0
//    while (b < bits) {
//      if (bit(b) == goal.bit(b))
//        okBits += 1
//      b += 1
//    }
//    val okPct = okBits.toDouble / bitsD
//    val f = 1.0 - okPct

    // val dbg = (0 until bits).reverse.map { b =>
    //   (bit(b), goal.bit(b)) match {
    //     case (true, true)   => "1"
    //     case (false, false) => "0"
    //     case (true, false)  => "<"
    //     case (false, true)  => ">"
    //   }
    // }.mkString
    // println(s"  $this has ${okBits} OK bits of $goal. f=$f : $dbg}")
    f
  }
}

object MutableLargeBitSet {

  final val BitsPerBlock = 64 // Long

  def apply(bits: Int): MutableLargeBitSet =
    new MutableLargeBitSet(bits)

  def empty: MutableLargeBitSet =
    apply(0)

  def fromArray(bits: Int, data: Array[Long]): MutableLargeBitSet = {
    val bs = apply(bits)
    java.lang.System.arraycopy(data, 0, bs.data, 0, bs.blocks)
    bs
  }

  def fromBinaryString(binaryString: String): MutableLargeBitSet = {
    val bits = binaryString.length

    @tailrec def go(s: String, bytes: List[Long]): List[Long] = {
      val byte   = s.iterator.take(BitsPerBlock).zipWithIndex.map { case (c, b) => if (c == '0') 0L else 1L << b}.sum
      val bytes2 = byte :: bytes
      val next   = s.drop(64)
      if (next.isEmpty)
        bytes2
      else
        go(next, bytes2)
    }
    val array = go(binaryString.reverse, Nil).toArray
    fromArray(bits, array)
  }

}