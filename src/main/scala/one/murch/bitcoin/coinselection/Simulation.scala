package one.murch.bitcoin.coinselection

/**
  * Created by murch on 31.12.16.
  */
object Simulation {
    def main(args: Array[String]) = {
//
//        val testCases4 = new Simulator(TestCase500Euler.startingUtxoSet, TestCase500Euler.operations, TestCase500Euler.descriptor)
//        val testCases5 = new Simulator(TestCaseJunkWallet.startingUtxoSet, TestCaseJunkWallet.operations, TestCaseJunkWallet.descriptor)
//        val testCases6 = new Simulator(GaussianWallet.startingUtxoSet, GaussianWallet.operations, GaussianWallet.descriptor)
//        val testCases7 = new Simulator(EmptyGaussianWallet.startingUtxoSet, EmptyGaussianWallet.operations, EmptyGaussianWallet.descriptor)
//
//        val testCases9 = new Simulator(TestCaseMoneyPot50.startingUtxoSet, TestCaseMoneyPot50.operations, TestCaseMoneyPot50.descriptor)
//        val testCases10 = new Simulator(TestCaseMoneyPot15.startingUtxoSet, TestCaseMoneyPot15.operations, TestCaseMoneyPot15.descriptor)
//        val testCases11 = new Simulator(TestCaseMoneyPot15Coins.startingUtxoSet, TestCaseMoneyPot15Coins.operations, TestCaseMoneyPot15Coins.descriptor)
//        val testCases12 = new Simulator(Set(), TestCaseMoneyPotEmpty.operations, TestCaseMoneyPotEmpty.descriptor)
        val mpAfterLF = new Simulator(MoneyPotAfterLF.startingUtxoSet, MoneyPotAfterLF.operations, MoneyPotAfterLF.descriptor, 10)

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
        //        println("--------------------------------------------------------------------------------------")
        //        println("---------------TEST CASE 12 STARTING--------------------------------------------------")
        //        println("--------------------------------------------------------------------------------------")
        //        testCases12.simulate()
        println("--------------------------------------------------------------------------------------")
        println("---------------TEST CASE MP-after-LF STARTING--------------------------------------------------")
        println("--------------------------------------------------------------------------------------")
        mpAfterLF.simulate()
    }
}
