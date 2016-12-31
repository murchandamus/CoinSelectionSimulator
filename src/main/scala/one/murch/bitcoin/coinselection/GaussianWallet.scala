package one.murch.bitcoin.coinselection

import scala.util.Random

/**
  * Created by murch on 31.12.16.
  */
object GaussianWallet extends Scenario(Set(), ListBuffer(), "TESTCASE 6: Start with 1,000 sub-CENT, then 10,000 random inc/outgoing Gaussian payments around 2.5*COIN") {

    val rnd = new Random()

    for (i <- 1 to 1000) {
        val nextBalance: Long = rnd.nextInt(1000000 - 2500).toLong
        var utxo = new Utxo(i, nextBalance + 2500)
        startingUtxoSet += utxo
    }

    var timer = 0
    for (i <- 1 to 10000) {
        timer += rnd.nextInt(15)
        var nextOp: Payment = new Payment(i, (rnd.nextGaussian() * (250000000.toLong)).toLong, timer)
        if (nextOp.value >= 0 && nextOp.value < WalletConstants.DUST_LIMIT) {
            nextOp = new Payment(nextOp.id, nextOp.value + WalletConstants.DUST_LIMIT, timer)
        } else if (nextOp.value > -WalletConstants.DUST_LIMIT) {
            nextOp = new Payment(nextOp.id, nextOp.value - WalletConstants.DUST_LIMIT, timer)
        }
        operations += nextOp
    }
}
