package one.murch.bitcoin.coinselection

class DoubleWallet(name: String, utxoList: Set[Utxo], feePerKB: Long, debug: Boolean) extends CoreWallet(name, utxoList, feePerKB, debug) {

    override def adjustMinChange(target: Long) {
        if (getWalletTotal() > target * 3) {
            MIN_CHANGE = target
        } else {
            MIN_CHANGE = WalletConstants.CENT
        }
    }

}
