package one.murch.bitcoin.coinselection

import scala.io.Source
import scala.util.Random

/**
  * Created by murch on 31.12.16.
  */
object TestCaseMoneyPot15 extends Scenario(Set(), ListBuffer(), "TESTCASE 10: Starts with one 15BTC UTXO, then performs all operations from MoneyPot.com's data.") {
    val rnd = new Random()
    var btc15: Long = (15 * 100) * (1000000).toLong
    var utxo15btc = new Utxo(1, btc15)
    startingUtxoSet += utxo15btc

    var index = 1
    var nLockTime = 1

    for (line <- Source.fromFile("scenarios/moneypot.csv").getLines()) {
        var nextOp: Long = (line).toLong
        nLockTime = math.max(0, ((rnd.nextGaussian() + 1) * 10).toInt) + nLockTime
        operations += new Payment(index, nextOp, nLockTime)
        index += 1
    }
}
