package one.murch.bitcoin.coinselection

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

/**
  * Created by murch on 31.12.16.
  */
object TestCaseMoneyPot50 extends Scenario(Set(), ListBuffer(), "TESTCASE 9: Starts with one 50BTC UTXO, then performs all operations from MoneyPot.com's data.") {
    val rnd = new Random()
    var btc50: Long = (50 * 100) * (1000000).toLong
    var utxo50btc = new Utxo(1, btc50)
    startingUtxoSet += utxo50btc

    var index = 1
    var nLockTime = 1

    for (line <- Source.fromFile("scenarios/moneypot.csv").getLines()) {
        var nextOp: Long = (line).toLong
        nLockTime = math.max(0, ((rnd.nextGaussian() + 1) * 10).toInt) + nLockTime
        operations += new Payment(index, nextOp, nLockTime)
        index += 1
    }
}
