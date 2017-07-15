package one.murch.bitcoin.coinselection

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

/**
  * Created by murch on 2017-01-19
  */
object MoneyPotAfterLF extends Scenario(Set(), ListBuffer(), "Imports final UTXO set of LF after running MP, then runs MP") {
    val rnd = new Random(1)
    var index = 1
    for (line <- Source.fromResource("scenarios/UTXO-post-LF.csv").getLines()) {
        val nextUTXO: Utxo = new Utxo(index, (line).toLong)
        if(index < 30) {println("Loading UTXO for starting set: Index " + index + " Value " + nextUTXO.value)}
        startingUtxoSet += nextUTXO
        index += 1
    }

    var nLockTime = 1

    for (line <- Source.fromResource("scenarios/moneypot.csv").getLines()) {
        val nextOp: Long = (line).toLong
        nLockTime = math.max(0, ((rnd.nextGaussian() + 1) * 10).toInt) + nLockTime
        operations += new Payment(index, nextOp, nLockTime)
        index += 1
    }
}
