package one.murch.bitcoin.coinselection

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

/**
  * Created by murch on 31.12.16.
  */
object MPRepeated extends Scenario(Set(), ListBuffer(), "TESTCASE 14: Empty Wallet, executes the Moneypot scenario twice.") {
    val rnd = new Random()

    var index = 1
    var nLockTime = 1

    for(i <- 1 to 2) {
        for (line <- Source.fromFile("scenarios/moneypot.csv").getLines()) {
            var nextOp: Long = (line).toLong
            nLockTime = math.max(0, ((rnd.nextGaussian() + 1) * 10).toInt) + nLockTime
            operations += new Payment(index, nextOp, nLockTime)
            index += 1
        }
    }
}
