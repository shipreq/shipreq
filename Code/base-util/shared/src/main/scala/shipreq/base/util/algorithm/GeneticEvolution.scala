package shipreq.base.util.algorithm

import java.util.{Comparator, Random}
object GeneticEvolution {

  object Binary {
    import MutableLargeBitSet.BitsPerBlock

    type Chromosome = MutableLargeBitSet

    private[algorithm] type Population = Array[Chromosome]

    /**
     * @param fitness Must be ≥ 0 where 0 is perfection
     * @param goal A chromosone with a fitness ≤ goal is considered success
     */
    def apply(bits   : Int,
              fitness: Chromosome => Double,
              goal   : Double,
              config : Config): Chromosome = {

      def isSmall = bits <= 16 // so that domain doesn't overflow
      def domain = (1 << bits) - 1

      if (bits <= 0)
        MutableLargeBitSet.empty

      else if (config.allowBrute && isSmall && domain <= config.popSize)
        bruteForce(bits, fitness)

      else
        solveViaEvolution(bits, fitness, goal, config)
    }

    private[algorithm] def bruteForce(bits: Int, fitness: Chromosome => Double): Chromosome = {
      val domain      = (1 << bits) - 1
      val c           = MutableLargeBitSet(bits)
      var bestFitness = Double.PositiveInfinity
      var bestValue   = -1
      var i           = domain
      while (i > 0) {
        i -= 1
        c.data(0) = i
        val f = fitness(c)
        if (f < bestFitness) {
          bestFitness = f
          bestValue = i
        }
      }
      c.data(0) = bestValue
      c
    }

    private[algorithm] def solveViaEvolution(bits   : Int,
                                             fitness: Chromosome => Double,
                                             goal   : Double,
                                             config : Config): Chromosome = {
      assert(config.popSize >= 4)

      val popSize    = config.popSize
      val rng        = new Random(0)
      val fitnesses  = Array.fill(popSize)(new Fitness)
      val select     = selection(fitnesses)
      var fitnessSum = 0.0
      var i          = 0

      val rndInt: RndInt =
        rng.nextInt(_)

      val doCrossover = crossover(bits, rndInt)

      def newChromosome(): Chromosome =
        MutableLargeBitSet(bits).randomise(rng)

      def newPopulation(): Population =
        Array.fill(popSize)(newChromosome())

      val shouldMutate: () => Boolean =
        if (config.mutationRate >= 1)
          () => true
        else if (config.mutationRate <= 0)
          () => false
        else
          () => rng.nextDouble() <= config.mutationRate

      @tailrec def run(generation: Int, population: Population): Chromosome = {

        // Calculate fitness of each chromosone
        fitnessSum = 0.0
        i = fitnesses.length
        while (i > 0) {
          i -= 1
          val chromosone = population(i)
          val x = fitnesses(i)
          val f = fitness(chromosone)
          x.popIdx = i
          x.fitness = f
          fitnessSum += f
        }

        // Rank fitness of each chromosone
        if (fitnessSum > 0) {
          java.util.Arrays.sort(fitnesses, fitnessCmp)
          var acc = 0.0
          i = fitnesses.length
          while (i > 0) {
            i -= 1
            val f = fitnesses(i)
            acc += f.fitness / fitnessSum
            f.score = acc
          }
        } else {
          // clear scores
          i = fitnesses.length
          while (i > 0) {
            i -= 1
            val f = fitnesses(i)
            f.score = 0
          }
        }

        // Print progress
//        println(s"---------------- Gen ${generation} ----------------")
//        for (f <- fitnesses.iterator.take(3)) {
//          val c = population(f.popIdx)
//          printf("[%s] fitness = %012.10f, score = %012.10f\n", c.toBinaryStr, f.fitness, f.score)
//        }

        // Check for success
        val best = fitnesses(0)
        if (best.fitness <= goal || generation >= config.maxGens)
          population(best.popIdx)

        else {

          // Evolve
          val nextGeneration: Population =
            Array.tabulate(popSize) { n =>

              // Elitism: retain the best few without modification
              if (n < config.eliteSize) {
                population(fitnesses(n).popIdx)

              } else {

                // Select parents
                val f1 = select(rng.nextDouble(), -1)
                val f2 = select(rng.nextDouble(), f1)
                val c1 = population(fitnesses(f1).popIdx)
                val c2 = population(fitnesses(f2).popIdx)

                // Crossover
                val c = doCrossover(c1, c2)

                // Mutate
                var i = bits
                while (i > 0) {
                  i -= 1
                  if (shouldMutate())
                    c.flipBit(i)
                }

                c
              }
            }

          run(generation + 1, nextGeneration)
        }
      }

      run(0, newPopulation())
    }

    private[algorithm] trait RndInt {
      /**
        * @param bound ≥ 0
        * @return [0,bound)
        */
      def nextInt(bound: Int): Int
    }

    private[algorithm] def selection(fitnesses: Array[Fitness]): (Double, Int) => Int =
      (p: Double, skipIdx: Int) => {
        // TODO scale down p when skipIdx >= 0
        @tailrec def go(i: Int): Int =
          if (i == skipIdx)
            go(i + 1)
          else if (i == fitnesses.length) {
            val j = i - 1
            if (j == skipIdx) j - 1 else j
          } else {
            val s = fitnesses(i).score
            if (s >= p)
              go(i + 1)
            else
              i - 1
          }
        go(0).max(0)
      }

    private[algorithm] def crossover(bits: Int, rndInt: RndInt): (Chromosome, Chromosome) => Chromosome = {
      val gaps  = bits - 1
      val bound = gaps << 1
      (aa, bb) => {
        var a = aa
        var b = bb
        var c = rndInt.nextInt(bound)
        if (c >= gaps) {
          c -= gaps
          a = bb
          b = aa
        }
        val x = a.clone()
        var i = a.data.length - 1
        while (c >= BitsPerBlock) {
          x.data(i) = b.data(i)
          i -= 1
          c -= BitsPerBlock
        }
        if (i >= 0) {
          val mb = (2L << c) - 1
          val ma = mb ^ -1
          x.data(i) = (a.data(i) & ma) | (b.data(i) & mb)
        }
        x
      }
    }

    final case class Config(popSize     : Int,
                            eliteSize   : Int,
                            maxGens     : Int,
                            mutationRate: Double,
                            allowBrute  : Boolean)

    private[algorithm] final class Fitness {
      var popIdx : Int    = _
      var fitness: Double = _ // [0,∞) where 0 = perfection
      var score  : Double = _ // [0,1] where 1 = 100% chance of selection
    }

    private val fitnessCmp: Comparator[Fitness] =
      (x, y) =>
        if (x.fitness == y.fitness)
          0
        else if (x.fitness < y.fitness)
          -1
        else
          1
  }

}
