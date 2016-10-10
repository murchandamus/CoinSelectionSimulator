import scala.collection.mutable.ListBuffer

class AvTargetWallet(name: String, utxoList: Set[Utxo], feePerKB: Long, debug: Boolean) extends CoreWallet(name, utxoList, feePerKB, debug) {
    var targetsTotal: Long = 0
    var numTargets: Int = 0

    override def spend(target: Long, nLockTime: Int) {
        if (target > getWalletTotal()) {
            throw new IllegalArgumentException("Wallet was requested to spend " + target + " but only contained " + getWalletTotal() + ".");
        }

        val starttime: Long = System.currentTimeMillis
        targetsTotal += target
        numTargets += 1

        MIN_CHANGE = targetsTotal / numTargets
        println("Current MIN_CHANGE is now: " + MIN_CHANGE)

        var selectedCoins: Set[Utxo] = selectCoins(target, feePerKB, nLockTime)

        //var fee: Long = estimateFee(target, selectedCoins, feePerKB, MIN_CHANGE) ← Core estimates fee in Selection
        var fee = currentTransactionFee

        var change: Long = selectionTotal(selectedCoins) - target - fee

        val duration = (System.currentTimeMillis) - starttime

        var tx = new Transaction(name, target, change, fee, selectedCoins, nLockTime, duration)
        executeTransaction(tx)
        utxoSetSizes += utxoPool.size
        inTransitRatio += change.toDouble / getWalletTotal()
    }
}
