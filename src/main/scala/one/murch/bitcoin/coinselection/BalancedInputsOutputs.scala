package one.murch.bitcoin.coinselection

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

/**
  * Created by murch on 31.12.16.
  */
object BalancedInputsOutputs extends Scenario(Set(), ListBuffer(), "TESTCASE 13: Empty Wallet, executes the Moneypot scenario but compacts two incoming payments to one.") {
    val rnd = new Random()

    var index = 1
    var nLockTime = 1
    var tempOp : Long = 0

    for (line <- Source.fromFile("scenarios/moneypot.csv").getLines()) {
        var nextOp: Long = (line).toLong

        // Either stores an incoming payment, or combines two incoming payments to one
        if(nextOp > 0 && tempOp > 0) {
            nextOp = tempOp + nextOp
            tempOp = 0
        } else if (nextOp > 0) {
            tempOp = nextOp
            nextOp = 0
        }

        if(nextOp != 0) {
            nLockTime = math.max(0, ((rnd.nextGaussian() + 1) * 10).toInt) + nLockTime
            operations += new Payment(index, nextOp, nLockTime)
            index += 1
        }
    }
}
