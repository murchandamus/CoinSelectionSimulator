package one.murch.bitcoin.coinselection



class MyceliumWallet(name: String, utxoList: Set[Utxo], feePerKB: Long, debug: Boolean) extends AbstractWallet(name, utxoList, feePerKB, debug) {
    val MIN_CHANGE_BEFORE_ADDING_TO_FEE: Long = 5460

    def selectCoins(target: Long, feePerKB: Long, nLockTime: Int): Option[Set[Utxo]] = {
        // not sure how this wallet behaves with unsufficient funds
        if (Wallet.minWalletValue(target) >= getWalletTotal()) {
            return None
        }

        if (debug == true) {
            println(name + " is selecting for " + target)
        }
        var selectedCoins: Set[Utxo] = Set()
        var sortedUtxo = utxoPool.toList.sortBy(_.id)

        while (sortedUtxo.nonEmpty && selectionTotal(selectedCoins) < target + estimateFeeWithChange(target, selectedCoins, feePerKB)) {
            selectedCoins += sortedUtxo.head
            if (debug == true) {
                println(name + " added " + sortedUtxo.head + ". Combination is now " + selectedCoins + ".")
            }
            sortedUtxo = sortedUtxo.tail
        }

        if (debug == true) {
            println(name + " is going into post-selection with combination " + selectedCoins + ".")
        }

        //Post-selection pruning
        var selectedUtxoBySize = selectedCoins.toList.sortWith(_.value > _.value)
        selectedCoins = Set()
        while (selectedUtxoBySize.isEmpty == false && selectionTotal(selectedCoins) < target + estimateFeeWithChange(target, selectedCoins, feePerKB)) {
            selectedCoins += selectedUtxoBySize.head
            if (debug == true) {
                println(name + " kept " + selectedUtxoBySize.head + ". Combination is now " + selectedCoins + ".")
            }
            selectedUtxoBySize = selectedUtxoBySize.tail
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
