import util.Random

class CoreCppWallet(name: String, utxoList: Set[Utxo], debug: Boolean, knapsackLimit: Int) extends Wallet(name, utxoList, debug, knapsackLimit) {
    @native override def knapsack(target: Long, tries: Int): Set[Utxo];
}

object CoreCppWallet extends App {
    System.loadLibrary("libmurchjni")
    val utxoList: Set[Utxo] = Set()
    val coreCppWallet = new CoreCppWallet("CoreCppWallet", utxoList, false, 5)
}
