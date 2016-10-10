class Transaction(val walletName: String, val target: Long, val change: Long, val fee: Long, val inputSet: Set[Utxo], val block: Int, val duration: Long) {
    var inputsValue: Long = 0
    inputSet.foreach(inputsValue += _.value)
    var outputsValue: Long = 0
    outputsValue += target
    outputsValue += change
    outputsValue += fee
    if (inputsValue != outputsValue) {
        println("ERROR: Transaction inputs and outputs don't match in value for " + walletName)
    }

    def createTransactionReport() {
        println("To spend " + target + ", " + walletName + " selected " + inputSet.size + " inputs, with a total.value of " + inputsValue + " satoshi. The change was " + change + " It took " + duration + " ms to calculate.")
    }
}
