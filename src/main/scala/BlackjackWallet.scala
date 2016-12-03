package main.scala

class BlackjackWallet(name: String, utxoList: Set[Utxo], feePerKB: Long, debug: Boolean) extends AbstractWallet(name, utxoList, feePerKB, debug) {
    val MIN_CHANGE_BEFORE_ADDING_TO_FEE = WalletConstants.DUST_LIMIT

    def selectCoins(target: Long, feePerKB: Long, nLockTime: Int): Set[Utxo] = {
        if (debug == true) {
            println(name + " is selecting for " + target)
        }
        var selectedCoins: Set[Utxo] = Set()
        var sortedUtxo = utxoPool.toList.sortBy(_.value > _.value)
        var notSelected = List[Utxo]

        while (sortedUtxo.nonEmpty && selectionTotal(selectedCoins) < target + estimateFeeWithChange(target, selectedCoins, feePerKB)) {
            if(sortedUtxo.head.value < target - selectionTotal(selectedCoins)) {
                selectedCoins += sortedUtxo.head
                if (debug == true) {
                    println(name + " added " + sortedUtxo.head + ". Combination is now " + selectedCoins + ".")
                }
            } else {
                notSelected.add(sortedUtxo.head)
            }
            sortedUtxo = sortedUtxo.drop(1)
        }
    
    // How did Daniel Cousens solve fee estimation?

        return selectedCoins
    }

    def estimateFeeWithChange(target: Long, selectedCoins: Set[Utxo], feePerKB: Long): Long = {
        var fee: Long = 0
        var transactionSize = WalletConstants.BYTES_TRANSACTION_OVERHEAD + selectedCoins.size * WalletConstants.BYTES_PER_INPUT + 2 * WalletConstants.BYTES_PER_OUTPUT

        fee += transactionSize * feePerKB / 1000

        return fee
    }
}
