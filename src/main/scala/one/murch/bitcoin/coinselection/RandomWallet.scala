package one.murch.bitcoin.coinselection


import scala.util.Random

class RandomWallet(name: String, utxoList: Set[Utxo], feePerKB: Long, debug: Boolean, minChange: Long = 0) extends AbstractWallet(name, utxoList, feePerKB, debug) {
    val MIN_CHANGE = minChange
    val MIN_CHANGE_BEFORE_ADDING_TO_FEE = WalletConstants.DUST_LIMIT

    def selectCoins(target: Long, feePerKB: Long, nLockTime: Int): Option[Set[Utxo]] = {

        if (debug == true) {
            println(name + " is selecting for " + target + " in block " + nLockTime)
        }

        var selectedCoins: Set[Utxo] = Set()
        var randomizedUtxo = Random.shuffle((utxoPool).toList)

        while (randomizedUtxo.nonEmpty && selectionTotal(selectedCoins) < target + estimateFeeWithChange(target, selectedCoins, feePerKB) + MIN_CHANGE) {
            selectedCoins += randomizedUtxo.head
            if (debug == true) {
                println(name + " added " + randomizedUtxo.head + ". Combination is now " + selectedCoins + ".")
            }
            randomizedUtxo = randomizedUtxo.tail
        }
        if (selectionTotal(selectedCoins) < target + estimateFeeWithChange(target, selectedCoins, feePerKB)) {
            return None
        }
 
        return Some(selectedCoins)
    }

    def estimateFeeWithChange(target: Long, selectedCoins: Set[Utxo], feePerKB: Long): Long = {
        var fee: Long = 0
        var transactionSize = WalletConstants.BYTES_TRANSACTION_OVERHEAD + selectedCoins.size * WalletConstants.BYTES_PER_INPUT + 2 * WalletConstants.BYTES_PER_OUTPUT

        fee += transactionSize * feePerKB / 1000

        return fee
    }
}
