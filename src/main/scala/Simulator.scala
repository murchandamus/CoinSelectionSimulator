package main.scala

import util.Random
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer

class Simulator(utxo: Set[Utxo], operations: ListBuffer[Payment], descriptor: String) {
    //    val coreWallet = new CoreWallet("CoreWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
    //    val coreWalletOutput = new CoreWallet("CoreWalletDonateOutputCost", utxo, WalletConstants.FEE_PER_KILOBYTE, false, WalletConstants.OUTPUT_COST)
    //    val coreWalletInput = new CoreWallet("CoreWalletDonateInputCost", utxo, WalletConstants.FEE_PER_KILOBYTE, false, WalletConstants.INPUT_COST)
    //    val coreWalletDust = new CoreWallet("CoreWalletDonateDustLimit", utxo, WalletConstants.FEE_PER_KILOBYTE, false, WalletConstants.DUST_LIMIT)

    //val myceliumWallet = new MyceliumWallet("MyceliumWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
      val bnbWallet = new BnBWallet("BranchAndBoundWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
      val eBnbWallet = new EfficientBnB("EfficientBnBWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
      val lfWallet = new LargestFirstWallet("LargestFirstWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
      val bjWallet = new BlackjackWallet("BlackjackWallet", utxo, WalletConstants.FEE_PER_KILOBYTE, false)
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
    var wallets: List[AbstractWallet] = List(bnbWallet, eBnbWallet, lfWallet, bjWallet)

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

class ParetoGenerator(low: Int, high: Int, k: Int) {
    val rnd = new Random()
    def next(): Long = {
        var accepted = false
        var nextValue: Long = -1
        val signum = rnd.nextInt(4)
        while (!accepted) {
            val candidate: Long = rnd.nextInt(high.toInt).toLong
            val score = rnd.nextFloat()
            val prob: Double = 1.0 - math.pow(low.toFloat / candidate, k)
            if (score <= prob) {
                accepted = true
                nextValue = candidate
            }
        }

        if (signum == 0) {
            // Create incoming of quadruple value with 1/4 chance.
            nextValue * 4
        } else {
            nextValue * (-1)
        }
    }
}

object Simulation {
    def main(args: Array[String]) = {

        val testCases4 = new Simulator(TestCase500Euler.startingUtxoSet, TestCase500Euler.operations, TestCase500Euler.descriptor)
        val testCases5 = new Simulator(TestCaseJunkWallet.startingUtxoSet, TestCaseJunkWallet.operations, TestCaseJunkWallet.descriptor)
        val testCases6 = new Simulator(GaussianWallet.startingUtxoSet, GaussianWallet.operations, GaussianWallet.descriptor)
        val testCases7 = new Simulator(EmptyGaussianWallet.startingUtxoSet, EmptyGaussianWallet.operations, EmptyGaussianWallet.descriptor)

        val testCases9 = new Simulator(TestCaseMoneyPot50.startingUtxoSet, TestCaseMoneyPot50.operations, TestCaseMoneyPot50.descriptor)
        val testCases10 = new Simulator(TestCaseMoneyPot15.startingUtxoSet, TestCaseMoneyPot15.operations, TestCaseMoneyPot15.descriptor)
        val testCases11 = new Simulator(TestCaseMoneyPot15Coins.startingUtxoSet, TestCaseMoneyPot15Coins.operations, TestCaseMoneyPot15Coins.descriptor)
        val testCases12 = new Simulator(Set(), TestCaseMoneyPotEmpty.operations, TestCaseMoneyPotEmpty.descriptor)

        //        //        println("--------------------------------------------------------------------------------------")
        //        //        println("---------------TEST CASE 1 STARTING---------------------------------------------------")
        //        //        println("--------------------------------------------------------------------------------------")
        //        //        testCases.simulate()
        //        //        println("--------------------------------------------------------------------------------------")
        //        //        println("---------------TEST CASE 2 STARTING---------------------------------------------------")
        //        //        println("--------------------------------------------------------------------------------------")
        //        //        testCases2.simulate()
        //        //        println("--------------------------------------------------------------------------------------")
        //        //        println("---------------TEST CASE 3 STARTING---------------------------------------------------")
        //        //        println("--------------------------------------------------------------------------------------")
        //        //        testCases3.simulate()
        //        println("--------------------------------------------------------------------------------------")
        //        println("---------------TEST CASE 4 STARTING---------------------------------------------------")
        //        println("--------------------------------------------------------------------------------------")
        //        testCases4.simulate()
        //        println("--------------------------------------------------------------------------------------")
        //        println("---------------TEST CASE 5 STARTING---------------------------------------------------")
        //        println("--------------------------------------------------------------------------------------")
        //        testCases5.simulate()
        //        println("--------------------------------------------------------------------------------------")
        //        println("---------------TEST CASE 6 STARTING---------------------------------------------------")
        //        println("--------------------------------------------------------------------------------------")
        //        testCases6.simulate()
        //        println("--------------------------------------------------------------------------------------")
        //        println("---------------TEST CASE 7 STARTING---------------------------------------------------")
        //        println("--------------------------------------------------------------------------------------")
        //        testCases7.simulate()
        //        //        println("--------------------------------------------------------------------------------------")
        //        //        println("---------------TEST CASE 8 STARTING---------------------------------------------------")
        //        //        println("--------------------------------------------------------------------------------------")
        //        //        testCases8.simulate()
        //        println("--------------------------------------------------------------------------------------")
        //        println("---------------TEST CASE 9 STARTING---------------------------------------------------")
        //        println("--------------------------------------------------------------------------------------")
        //        testCases9.simulate()
        //        println("--------------------------------------------------------------------------------------")
        //        println("---------------TEST CASE 10 STARTING--------------------------------------------------")
        //        println("--------------------------------------------------------------------------------------")
        //        testCases10.simulate()
        //        println("--------------------------------------------------------------------------------------")
        //        println("---------------TEST CASE 11 STARTING--------------------------------------------------")
        //        println("--------------------------------------------------------------------------------------")
        //        testCases11.simulate()
        println("--------------------------------------------------------------------------------------")
        println("---------------TEST CASE 12 STARTING--------------------------------------------------")
        println("--------------------------------------------------------------------------------------")
        testCases12.simulate()
    }
}
