package one.murch.bitcoin.coinselection

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

/**
  * Created by murch on 31.12.16.
  */
object TestCaseMoneyPot15Coins extends Scenario(Set(), ListBuffer(), "TESTCASE 11: Starts with fifteen 1BTC UTXO, then performs all operations from MoneyPot.com's data.") {
    val rnd = new Random()
    for (i <- 1 to 15) {
        startingUtxoSet += new Utxo(i, WalletConstants.COIN)
    }

    var index = 1
    var nLockTime = 1

    for (line <- Source.fromFile("scenarios/moneypot.csv").getLines()) {
        var nextOp: Long = (line).toLong
        nLockTime = math.max(0, ((rnd.nextGaussian() + 1) * 10).toInt) + nLockTime
        operations += new Payment(index, nextOp, nLockTime)
        index += 1
    }
}
