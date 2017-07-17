package one.murch.bitcoin.coinselection

import scala.collection.mutable.{ListBuffer, Queue}

/**
  * Created by murch on 31.12.16.
  */
class Simulator(utxo: Set[Utxo], operations: ListBuffer[Payment], descriptor: String) {
    //    val coreWallet = new CoreWallet("CoreWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
    //    val coreWalletOutput = new CoreWallet("CoreWalletDonateOutputCost", utxo, WalletConstants.FEE_PER_KILOBYTE, false, WalletConstants.OUTPUT_COST)
    //    val coreWalletInput = new CoreWallet("CoreWalletDonateInputCost", utxo, WalletConstants.FEE_PER_KILOBYTE, false, WalletConstants.INPUT_COST)
    //    val coreWalletDust = new CoreWallet("CoreWalletDonateDustLimit", utxo, WalletConstants.FEE_PER_KILOBYTE, false, WalletConstants.DUST_LIMIT)

    //val myceliumWallet = new MyceliumWallet("MyceliumWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
      //val bnbWallet = new BnBWallet("BranchAndBoundWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
      val setrBnB= new StackEfficientTailRecursiveBnB("SETRBnB", utxo, WalletConstants.FEE_PER_KILOBYTE, 0.5, false)
      val eBnbWallet = new EfficientBnB("EfficientBnBWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
      val lfWallet = new LargestFirstWallet("LargestFirstWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
      val bjWallet = new BlackjackWallet("BlackjackWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
      val sfWallet = new SmallestFirstWallet("SmallestFirstWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
      val yfWallet = new YoungestFirstWallet("YoungestFirstWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
      val ofWallet = new BreadWallet("OldestFirstWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
    val randomWallet0Z = new RandomWallet("RandomWallet0Z", utxo, WalletConstants.FEE_PER_KILOBYTE, false, 0)
    val randomWallet4Z = new RandomWallet("RandomWallet4Z", utxo, WalletConstants.FEE_PER_KILOBYTE, false, 10000)
    val randomWallet6Z = new RandomWallet("RandomWallet6Z", utxo, WalletConstants.FEE_PER_KILOBYTE, false, 1000000)
 //   val bnbWallet2 = new BnBWallet("BranchAndBoundWallet2", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
  //  val bnbWallet3 = new BnBWallet("BranchAndBoundWallet3", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
   // val bnbWallet4 = new BnBWallet("BranchAndBoundWallet4", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
   // val bnbWallet5 = new BnBWallet("BranchAndBoundWallet5", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
    //    val breadWallet = new BreadWallet("BreadWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
    //    val androidWallet = new AndroidWallet("AndroidWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
    //    val randomWallet = new RandomWallet("RandomWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
    //    val doubleWallet = new DoubleWallet("DoubleWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
    // val avTargetWallet = new AvTargetWallet("AvTargetWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)

    /*
    val randomWallet0Z = new RandomWallet("RandomWallet0Z", utxo, WalletConstants.FEE_PER_KILOBYTE, false, 0)
    val randomWallet1Z = new RandomWallet("RandomWallet1Z", utxo, WalletConstants.FEE_PER_KILOBYTE, false, 10)
    val randomWallet2Z = new RandomWallet("RandomWallet2Z", utxo, WalletConstants.FEE_PER_KILOBYTE, false, 100)
    val randomWallet3Z = new RandomWallet("RandomWallet3Z", utxo, WalletConstants.FEE_PER_KILOBYTE, false, 1000)
    val randomWallet4Z = new RandomWallet("RandomWallet4Z", utxo, WalletConstants.FEE_PER_KILOBYTE, false, 10000)
    val randomWallet5Z = new RandomWallet("RandomWallet5Z", utxo, WalletConstants.FEE_PER_KILOBYTE, false, 100000)
    val randomWallet6Z = new RandomWallet("RandomWallet6Z", utxo, WalletConstants.FEE_PER_KILOBYTE, false, 1000000)
    val randomWallet7Z = new RandomWallet("RandomWallet7Z", utxo, WalletConstants.FEE_PER_KILOBYTE, false, 10000000)
    val randomWallet8Z = new RandomWallet("RandomWallet8Z", utxo, WalletConstants.FEE_PER_KILOBYTE, false, 100000000)
*/
    var outgoingPaymentsQueue: Queue[Payment] = Queue[Payment]()
    var currentLowestBalance: Long = 0

    //    var wallets: List[AbstractWallet] = List(coreWallet, myceliumWallet, breadWallet, androidWallet, randomWallet, doubleWallet)
    //    var wallets: List[AbstractWallet] = List(randomWallet0Z, randomWallet1Z, randomWallet2Z, randomWallet3Z, randomWallet4Z, randomWallet5Z, randomWallet6Z, randomWallet7Z, randomWallet8Z)
    //   var wallets: List[AbstractWallet] = List(coreWallet, coreWalletOutput, coreWalletInput, coreWalletDust)
    var wallets: List[AbstractWallet] = List(setrBnB, lfWallet, bjWallet, sfWallet, yfWallet, ofWallet, randomWallet0Z, randomWallet4Z, randomWallet6Z)

    def simulate() {
        currentLowestBalance = 0
        operations.foreach {
            x =>
                if (x.value > 0) {
                    println("----- Incoming Payment: of " + x.value + " in " + x.nLockTime + "----- ")
                    wallets.foreach(_.receive(x.value, x.nLockTime))
                } else if (x.value < 0) {
                    println("----- Outgoing Payment: of " + x.value * (-1) + " in " + x.nLockTime + "----- ")
                    outgoingPaymentsQueue.enqueue(x)
                    println("QUEUE has now " + outgoingPaymentsQueue.size + " and current lowest balance is now " + currentLowestBalance)
                    println("Queue first element is " + outgoingPaymentsQueue.front.value)

                }
                findLowestBalance()
                println("Current lowest balance is now " + currentLowestBalance)
                while (false == outgoingPaymentsQueue.isEmpty && ((outgoingPaymentsQueue.front.value * (-1) + 2 * WalletConstants.CENT) < currentLowestBalance)) {
                    var first: Payment = outgoingPaymentsQueue.dequeue()
                    wallets.foreach(_.spend((-1) * first.value, first.nLockTime))
                    findLowestBalance()
                }
        }

        println("-------------------------------------------------------------------------------")
        println("---------------- FINAL RESULTS ------------------------------------------------")
        println("-------------------------------------------------------------------------------")
        println(descriptor)
        wallets.head.printStatusHeader()
        wallets.foreach(_.printWalletStatusCSV())
        println("-------------------------------------------------------------------------------")

        println("-------------------------------------------------------------------------------")
        println("---------------- FINAL UTXO SET DUMPS -----------------------------------------")
        println("-------------------------------------------------------------------------------")

        wallets.foreach(_.dumpUtxoSet())
        println("-------------------------------------------------------------------------------")
    }

    def findLowestBalance() = {
        currentLowestBalance = Long.MaxValue
        wallets.foreach {
            w => currentLowestBalance = Math.min(w.getWalletTotal(), currentLowestBalance)
        }
    }
}
