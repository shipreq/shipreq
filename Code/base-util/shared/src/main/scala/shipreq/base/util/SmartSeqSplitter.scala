package shipreq.base.util

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import shipreq.base.util.algorithm._

sealed trait SmartSeqSplitter[A] {

  /** Split a sequence according to given criteria.
    *
    * @param fitness Must be ≥ 0 where 0 is perfection
    * @param goal A solution with a fitness ≤ goal is considered success
    */
  def apply(input  : ArraySeq[A])
           (fitness: ArraySeq[ArraySeq[A]] => Double,
            goal   : Double): ArraySeq[ArraySeq[A]]
}

object SmartSeqSplitter {

  private def unpackGeneric[A: ClassTag](input: Array[A], splitAtBit: Int => Boolean): ArraySeq[ArraySeq[A]] = {
    val gaps = input.length - 1
    val b1 = ArraySeq.newBuilder[ArraySeq[A]]
    val b2 = ArraySeq.newBuilder[A]
    b2 += input(0)
    var i = 0
    while (i < gaps) {
      val split = splitAtBit(i)
      if (split) {
        b1 += b2.result()
        b2.clear()
      }
      i += 1
      b2 += input(i)
    }
    b1 += b2.result()
    b1.result()
  }

  def genetic[A: ClassTag](config: ArraySeq[A] => GeneticEvolution.Binary.Config): SmartSeqSplitter[A] =
    new SmartSeqSplitter[A] {
      override def apply(input  : ArraySeq[A])
                        (fitness: ArraySeq[ArraySeq[A]] => Double,
                         goal   : Double): ArraySeq[ArraySeq[A]] = {

        import GeneticEvolution.Binary.Chromosome

        val bits = input.length - 1
        val inputArray = input.unsafeArray.asInstanceOf[Array[A]]

        def unpack(c: Chromosome): ArraySeq[ArraySeq[A]] =
          unpackGeneric(inputArray, c.bit)

        val solutionChromosome: Chromosome =
          GeneticEvolution.Binary(
            bits    = bits,
            fitness = c => fitness(unpack(c)),
            goal    = goal,
            config  = config(input),
          )

        unpack(solutionChromosome)
      }
    }

  def idaStar[A: ClassTag]: SmartSeqSplitter[A] = {
    new SmartSeqSplitter[A] {
      override def apply(input  : ArraySeq[A])
                        (fitness: ArraySeq[ArraySeq[A]] => Double,
                         goal   : Double): ArraySeq[ArraySeq[A]] = {

        val bits = input.length - 1
        val inputArray = input.unsafeArray.asInstanceOf[Array[A]]

        def unpack(bs: MutableLargeBitSet): ArraySeq[ArraySeq[A]] =
          unpackGeneric(inputArray, bs.bit)

        val children: MutableLargeBitSet => Array[MutableLargeBitSet] =
          parent => Array.tabulate(bits) { b =>
            val c = parent.clone()
            c.flipBit(b)
            c
          }

        val f = new IterativeDeepeningA[MutableLargeBitSet](
          estCost  = c => fitness(unpack(c)),
          cost     = (_, xc, y) => Math.abs(xc - fitness(unpack(y))),
          isGoal   = (_, f) => f <= goal,
          children = children,
        )

        val solution = f.findGoalFrom(MutableLargeBitSet(bits).clear())

        solution match {
          case Some(bs) =>
            unpack(bs)

          case None =>
            val a = new Array[ArraySeq[A]](1)
            a(0) = input
            ArraySeq.unsafeWrapArray(a)
        }
      }
    }
  }

  // ===================================================================================================================

  def draw(d: ArraySeq[ArraySeq[String]]): String = {
    val getLineWidth: ArraySeq[String] => Double = _.mkString(" ").length.toDouble
    val maxLen = d.iterator.map(getLineWidth).max.max(1)
    val fmt = s"%-${maxLen.toInt}s|"
    d.iterator.map { l =>
      val pad = (maxLen - getLineWidth(l)).toInt >> 1
      fmt.format((" " * pad) + l.mkString(" "))
    }.mkString("\n")
  }

  def fillOval[A](input       : ArraySeq[A],
                  getLineWidth: ArraySeq[A] => Double,
                  lineHeight  : Double,
                  idealWidth  : Double,
                  tolerance   : Double,
                  splitter    : SmartSeqSplitter[A]): ArraySeq[ArraySeq[A]] = {

    val ref = new WHRef

    val fitness: ArraySeq[ArraySeq[A]] => Double =
      data => {
        ovalWH(data, getLineWidth, lineHeight, ref)
        import ref._

//        var idealWidthDist = (idealWidth - w).abs
//        idealWidthDist *= idealWidthDist
//        val g = idealWidthDist * area()

        val overWidth =
          if (w > idealWidth)
            Math.pow(w - idealWidth, 2)
          else
            1.0

        val orientation =
          if (h > w)
            1 + (h - w) / h
          else
            1.0

//        val g = overWidth * orientation * area()
//        val g = orientation * area()

        val idealWH = 6.0
        var idealWHDist = (idealWH - w/h).abs
//        idealWHDist *= idealWHDist
        val g = idealWHDist * area()



        // println(s"${draw(data.map(_.map(_.toString)))} g=$g, w=$w, h=$h, a=${area()}, orientation=$orientation, overWidth=$overWidth\n")

        g
      }

    splitter(input)(fitness, tolerance)
  }

  trait OvalDebugger {
    def init(lineHeight: Double, lineWidths: ArraySeq[Double]): Unit
    def addStep(step: Int, a: Double, b: Double, getX: Int => Double, getY1: Int => Double, getY: Int => Double): Unit
    def end(w: Double, h: Double): Unit
  }

  object OvalDebugger {

    object Off extends OvalDebugger {
      override def init(lineHeight: Double, lineWidths: ArraySeq[Double]) = ()
      override def addStep(step: Int, a: Double, b: Double, getX: Int => Double, getY1: Int => Double, getY: Int => Double) = ()
      override def end(w: Double, h: Double) = ()
    }

    final class ToSvg extends OvalDebugger {
      private val sb = new StringBuilder

      private var stepWidth: Double = _
      private var lineHeight: Double = _
      private var lineWidths: ArraySeq[Double] = _
      private var nextTx: Double = _

      private val scale = 40.0

      override def init(lineHeight: Double, lineWidths: ArraySeq[Double]): Unit = {
        this.lineHeight = lineHeight
        this.lineWidths = lineWidths
        this.stepWidth = lineWidths.max
        this.nextTx = stepWidth / 2
        sb.append(s"""<svg version="1.1"
                     |     baseProfile="full"
                     |     xmlns="http://www.w3.org/2000/svg">\n""".stripMargin)
        sb.append(s"""<g transform="scale($scale)" >\n""")
      }

      override def addStep(step: Int, a: Double, b: Double, getX: Int => Double, getY1: Int => Double, getY: Int => Double): Unit = {
        val tx = nextTx
        val ty = 1
        sb.append('\n')

        val cx = tx + a
        val cy = ty + b

        // Draw ellipse centre
        locally {
          val w = .05
          val h = .05
          val x = cx - w / 2
          val y = cy - h / 2
          sb.append(s"""<rect x="$x" y="$y" width="$w" height="$h" stroke="green" stroke-width="0.05" />\n""")
        }

        // Draw boxes
        for (i <- lineWidths.indices) {
          val w = lineWidths(i)
          val h = lineHeight
          val x = cx + getX(i) - w
          val y = cy + getY1(i)
          sb.append(s"""  <rect x="$x" y="$y" width="$w" height="$h" fill="blue" fill-opacity="0.1" stroke="blue" stroke-width="0.02" />\n""")
        }

        // Draw A/B
        locally {
          val i = step
          val x = cx + getX(i)
          val y = cy + getY(i)
          val w = .03
          val h = .03
          sb.append(s"""  <rect x="$x" y="$y" width="$w" height="$h" fill="transparent" stroke="black" stroke-width="0.06" />\n""")
        }

        // Draw ellipse
        sb.append(s"""  <ellipse cx="$cx" cy="$cy" rx="$a" ry="$b" fill="red" fill-opacity="0.25" />\n""")

        nextTx += stepWidth.max(a * 2) + 1
      }

      override def end(w: Double, h: Double): Unit = {
        sb.append("</g>\n</svg>")
      }

      def result(): String =
        sb.result()
    }
  }

  private[util] class WHRef {
    var a,b,w,h = 0.0
    def whRatio(): Double = w / h
    def area(): Double = Math.PI * a * b
  }

  private[this] val root2 = Math.sqrt(2.0)

  private[util] def ovalWH[A](data        : ArraySeq[ArraySeq[A]],
                              getLineWidth: ArraySeq[A] => Double,
                              lineHeight  : Double,
                              ref         : WHRef,
//                              debugger    : OvalDebugger = OvalDebugger.Off,
                             ): Unit = {

    // https://www.dummies.com/education/math/calculus/how-to-graph-an-ellipse/

    val lines       = data.length
    val last        = lines - 1
    val midLine     = lines / 2.0
    val totalHeight = lines * lineHeight
    val firstY      = -totalHeight / 2

    // a=w/2, b=h/2
    var a,b = 0.0

    @inline def aAt(x: Double): Double =
      (x.abs * 2) / root2

    @inline def bAt(y: Double): Double =
      (y.abs * 2) / root2

//    val getX: Int => Double =
//      i => getLineWidth(data(i)) / 2

    val getY1: Int => Double =
      i => firstY + lineHeight * i

    val getY: Int => Double =
      i => {
        val y1 = getY1(i)
        val y2 = y1 + lineHeight
        if (y2 <= 0)
          y1
        else if (y1 >= 0)
          y2
        else
          lineHeight / 2
      }

//    debugger.init(
//      lineHeight = lineHeight,
//      lineWidths = data.map(getLineWidth))

    // 0 1  |  2 3 - len=4, mid=2, i=2[1,j=2], 1[0,j=3], 0
    // 0 1 |2| 3 4 - len=5, mid=2.5, i=3[2], i=2[1,j=3], 1[0,j=4], 0
    var i = Math.ceil(midLine).toInt
    while (i > 0) {
      i -= 1

      var idx  = i
      var line = data(i)
      var lw   = getLineWidth(line)

      // Use the row on the other side (it's positional inverse) if it's longer
      if (i != midLine) {
        val j = last - i
        val line2 = data(j)
        val lw2   = getLineWidth(line2)
        if (lw2 > lw) {
          line = line2
          lw   = lw2
          idx  = j
        }
      }

      val x = lw / 2
      val y = getY(idx)

      val la = aAt(x = x)
      val lb = bAt(y = y)

      if (la > a) a = la
      if (lb > b) b = lb

//      debugger.addStep(step=idx, a=a, b=b, getX=getX, getY1=getY1, getY=getY)
      // println(s"[$i:$idx] ($x,$y) [$la,$lb] a=$a, b=$b")
    }

    ref.a = a
    ref.b = b
    ref.w = a * 2
    ref.h = b * 2

    //    debugger.end(w = ref.w, h = ref.h)
  }
}
