package one.murch.bitcoin.coinselection

class AvTargetWallet(name: String, utxoList: Set[Utxo], feePerKB: Long, debug: Boolean) extends CoreWallet(name, utxoList, feePerKB, debug) {
    var targetsTotal: Long = 0
    var numTargets: Int = 0

    override def adjustMinChange(target: Long) {
        targetsTotal += target
        numTargets += 1

        MIN_CHANGE = targetsTotal / numTargets
        println("Current MIN_CHANGE is now: " + MIN_CHANGE)
    }
}
