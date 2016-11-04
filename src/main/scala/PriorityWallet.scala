package main.scala

class PriorityWallet(name: String, utxoList: Set[Utxo], feePerKB: Long, debug: Boolean) extends AbstractWallet(name, utxoList, feePerKB, debug) {
    val MIN_CHANGE_BEFORE_ADDING_TO_FEE = WalletConstants.DUST_LIMIT

    def selectCoins(target: Long, feePerKB: Long, nLockTime: Int): Set[Utxo] = {

        if (debug == true) {
            println(name + " is selecting for " + target + " in block " + nLockTime)
        }
        var selectedCoins: Set[Utxo] = Set()
        var sortedUtxo = utxoPool.toList.sortWith(_.value > _.value)
        sortedUtxo = sortedUtxo.sortWith(_.getCoinAge(nLockTime) > _.getCoinAge(nLockTime))

        while (selectionTotal(selectedCoins) < target + estimateFeeWithChange(target, selectedCoins, feePerKB)) {
            selectedCoins += sortedUtxo.head
            if (debug == true) {
                println(name + " added " + sortedUtxo.head + ". Combination is now " + selectedCoins + ".")
            }
            sortedUtxo = sortedUtxo.drop(1)
        }

        return selectedCoins
    }

    def estimateFeeWithChange(target: Long, selectedCoins: Set[Utxo], feePerKB: Long): Long = {
        var fee: Long = 0
        var transactionSize = WalletConstants.BYTES_TRANSACTION_OVERHEAD + selectedCoins.size * WalletConstants.BYTES_PER_INPUT + 2 * WalletConstants.BYTES_PER_OUTPUT

        fee += transactionSize * feePerKB / 1000

        return fee
    }
}
