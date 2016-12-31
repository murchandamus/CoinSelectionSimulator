package one.murch.bitcoin.coinselection

import scala.util.Random

/**
  * Created by murch on 31.12.16.
  */
class ParetoGenerator(low: Int, high: Int, k: Int) {
    val rnd = new Random()
    def next(): Long = {
        var accepted = false
        var nextValue: Long = -1
        val signum = rnd.nextInt(4)
        while (!accepted) {
            val candidate: Long = rnd.nextInt(high.toInt).toLong
            val score = rnd.nextFloat()
            val prob: Double = 1.0 - math.pow(low.toFloat / candidate, k)
            if (score <= prob) {
                accepted = true
                nextValue = candidate
            }
        }

        if (signum == 0) {
            // Create incoming of quadruple value with 1/4 chance.
            nextValue * 4
        } else {
            nextValue * (-1)
        }
    }
}
