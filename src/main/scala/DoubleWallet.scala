class DoubleWallet(name: String, utxoList: Set[Utxo], feePerKB: Long, debug: Boolean) extends CoreWallet(name, utxoList, feePerKB, debug) {

    override def spend(target: Long, nLockTime: Int) {
        if (target > getWalletTotal()) {
            throw new IllegalArgumentException("Wallet was requested to spend " + target + " but only contained " + getWalletTotal() + ".");
        }

        val starttime: Long = System.currentTimeMillis
        if (getWalletTotal() > target * 3) {
            MIN_CHANGE = target
        } else {
            MIN_CHANGE = WalletConstants.CENT
        }
        var selectedCoins: Set[Utxo] = selectCoins(target, feePerKB, nLockTime)

        //var fee: Long = estimateFee(target, selectedCoins, feePerKB, MIN_CHANGE) ← Core estimates fee in Selection
        var fee = currentTransactionFee

        var change: Long = selectionTotal(selectedCoins) - target - fee

        val duration = (System.currentTimeMillis) - starttime
        if(selectedCoins != null && selectedCoins.nonEmpty) {
            var tx = new Transaction(name, target, change, fee, selectedCoins, nLockTime, duration)
            executeTransaction(tx)
            utxoSetSizes += utxoPool.size
            inTransitRatio += change.toDouble / getWalletTotal()
        }
    }
}
