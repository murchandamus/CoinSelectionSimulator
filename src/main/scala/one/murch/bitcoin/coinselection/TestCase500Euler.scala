package one.murch.bitcoin.coinselection

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Created by murch on 31.12.16.
  */
object TestCase500Euler extends Scenario(Set(), ListBuffer(), "TESTCASE 4: Start with 1-500 CENT, then spend 200 times 250 CENT") {
    val rnd = new Random()

    for (i <- 1 to 500) {
        var utxo: Utxo = new Utxo(i, i.toLong * WalletConstants.CENT)
        startingUtxoSet += utxo
    }
    var index = 1
    var nLockTime = 1

    for (i <- 1 to 500) {
        nLockTime = math.max(0, ((rnd.nextGaussian() + 1) * 10).toInt) + nLockTime
        operations += new Payment(index, -250 * WalletConstants.CENT, nLockTime)
        index += 1
    }
}
