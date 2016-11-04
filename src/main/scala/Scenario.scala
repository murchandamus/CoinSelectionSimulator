package main.scala

import scala.collection.mutable.ListBuffer
import util.Random
import scala.io.Source

class Scenario(var startingUtxoSet: Set[Utxo], var operations: ListBuffer[Payment], var descriptor: String) {

}

object TestCase1 extends Scenario(Set(), ListBuffer(), "") {
    var paymentValues: List[Long] = List(1, -1, 1, 1, -2, 1, 1, 1, 10, -9, -4, 3, 3, 3, 3, 11, -10, -1, -8)

    var i = 1
    paymentValues.foreach {
        x =>
            operations += new Payment(i, x * WalletConstants.CENT, 0)
            i += 1
    }
}

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

object EmptyGaussianWallet extends Scenario(Set(), ListBuffer(), "TESTCASE 7: Start with empty wallet, then 10,000 random inc/outgoing Gaussian payments around 2.5*COIN") {

    val rnd = new Random()

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

object TestCaseMoneyPot50 extends Scenario(Set(), ListBuffer(), "TESTCASE 9: Starts with one 50BTC UTXO, then performs all operations from MoneyPot.com's data.") {
    val rnd = new Random()
    var btc50: Long = (50 * 100) * (1000000).toLong
    var utxo50btc = new Utxo(1, btc50)
    startingUtxoSet += utxo50btc

    var index = 1
    var nLockTime = 1

    for (line <- Source.fromFile("../resources/scenarios/moneypot.csv").getLines()) {
        var nextOp: Long = (line).toLong
        nLockTime = math.max(0, ((rnd.nextGaussian() + 1) * 10).toInt) + nLockTime
        operations += new Payment(index, nextOp, nLockTime)
        index += 1
    }
}

object TestCaseMoneyPot15 extends Scenario(Set(), ListBuffer(), "TESTCASE 10: Starts with one 15BTC UTXO, then performs all operations from MoneyPot.com's data.") {
    val rnd = new Random()
    var btc15: Long = (15 * 100) * (1000000).toLong
    var utxo15btc = new Utxo(1, btc15)
    startingUtxoSet += utxo15btc

    var index = 1
    var nLockTime = 1

    for (line <- Source.fromFile("../resources/scenarios/moneypot.csv").getLines()) {
        var nextOp: Long = (line).toLong
        nLockTime = math.max(0, ((rnd.nextGaussian() + 1) * 10).toInt) + nLockTime
        operations += new Payment(index, nextOp, nLockTime)
        index += 1
    }
}

object TestCaseMoneyPot15Coins extends Scenario(Set(), ListBuffer(), "TESTCASE 11: Starts with fifteen 1BTC UTXO, then performs all operations from MoneyPot.com's data.") {
    val rnd = new Random()
    for (i <- 1 to 15) {
        startingUtxoSet += new Utxo(i, WalletConstants.COIN)
    }

    var index = 1
    var nLockTime = 1

    for (line <- Source.fromFile("../resources/scenarios/moneypot.csv").getLines()) {
        var nextOp: Long = (line).toLong
        nLockTime = math.max(0, ((rnd.nextGaussian() + 1) * 10).toInt) + nLockTime
        operations += new Payment(index, nextOp, nLockTime)
        index += 1
    }
}

object TestCaseMoneyPotEmpty extends Scenario(Set(), ListBuffer(), "TESTCASE 12: Starts with empty wallet, then performs all operations from MoneyPot.com's data.") {
    val rnd = new Random()

    var index = 1
    var nLockTime = 1

    for (line <- Source.fromFile("../resources/scenarios/moneypot.csv").getLines()) {
        var nextOp: Long = (line).toLong
        nLockTime = math.max(0, ((rnd.nextGaussian() + 1) * 10).toInt) + nLockTime
        operations += new Payment(index, nextOp, nLockTime)
        index += 1
    }
}

object BalancedInputsOutputs extends Scenario(Set(), ListBuffer(), "TESTCASE 13: Empty Wallet, executes the Moneypot scenario but compacts two incoming payments to one.") {
    val rnd = new Random()

    var index = 1
    var nLockTime = 1
    var tempOp : Long = 0

    for (line <- Source.fromFile("../resources/scenarios/moneypot.csv").getLines()) {
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

object MPRepeated extends Scenario(Set(), ListBuffer(), "TESTCASE 14: Empty Wallet, executes the Moneypot scenario twice.") {
    val rnd = new Random()

    var index = 1
    var nLockTime = 1

    for(i <- 1 to 2) {
        for (line <- Source.fromFile("../resources/scenarios/moneypot.csv").getLines()) {
            var nextOp: Long = (line).toLong
            nLockTime = math.max(0, ((rnd.nextGaussian() + 1) * 10).toInt) + nLockTime
            operations += new Payment(index, nextOp, nLockTime)
            index += 1
        }
    }
}
