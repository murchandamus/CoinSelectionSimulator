package one.murch.bitcoin.coinselection

import scala.util.Random

/**
  * Created by murch on 31.12.16.
  */
object TestCaseJunkWallet extends Scenario(Set(), ListBuffer(), "TESTCASE 5: Start with 1,000 sub-CENT, then 100 random inc/outgoing Gaussian payments around CENT") {
    val rnd = new Random()

    for (i <- 1 to 1000) {
        val nextBalance = rnd.nextInt(250000 - 2500)
        var utxo: Utxo = new Utxo(i, nextBalance + 2500)
        startingUtxoSet += utxo
    }
    var index = 1
    var nLockTime = 1

    for (i <- 1 to 100) {
        nLockTime = math.max(0, ((rnd.nextGaussian() + 1) * 10).toInt) + nLockTime
        operations += new Payment(index, ((rnd.nextGaussian() + 1) * WalletConstants.CENT).toLong, nLockTime)
        index += 1
    }
}
