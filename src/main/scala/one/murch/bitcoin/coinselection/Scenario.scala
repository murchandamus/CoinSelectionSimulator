package one.murch.bitcoin.coinselection

import scala.collection.mutable.ListBuffer

/**
  * Created by murch on 31.12.16.
  */
class Scenario(var startingUtxoSet: Set[Utxo], var operations: ListBuffer[Payment], var descriptor: String) {

}
