import util.Random

class Simulator(utxo: Set[(Int,Long)], operations: List[Long], descriptor: String) {
    val regularWallet = new Wallet("Regular", utxo,false, 1000) 
    val pruningWallet = new PruningWallet("Pruning", true, 1, utxo,false, 1000) 
    val pruningWalletMin2 = new PruningWallet("Pruning 2Min", true, 2, utxo,false,1000) 
    val pruningWalletMin3 = new PruningWallet("Pruning 3Min", true, 3, utxo,false,1000) 
    val doubleWallet = new DoubleWallet("Double", true, 1, utxo, false, 1000)

    var wallets: List[Wallet] = List(regularWallet, pruningWallet, pruningWalletMin2, pruningWalletMin3, doubleWallet)
    
    def simulate() {
        operations.foreach{
            x => if(x > 0) {
                wallets.foreach(_.receive(x))
            } else if(x < 0) {
                wallets.foreach(_.spend((-1)*x))
            }
        }
        println("-------------------------------------------------------------------------------")
        println("---------------- FINAL RESULTS ------------------------------------------------")
        println("-------------------------------------------------------------------------------")
        println(descriptor)
        wallets.head.printStatusHeader()
        wallets.foreach(_.printWalletStatusCSV())
        println("-------------------------------------------------------------------------------")
    }
}

class ParetoGenerator (low:Int, high:Int, k:Int) {
    val rnd = new Random()
    def next() : Long = {
        var accepted = false
        var nextValue : Long = -1
        val signum = rnd.nextInt(4)
        while(!accepted) {
            val candidate : Long = rnd.nextInt(high.toInt).toLong
            val score = rnd.nextFloat()
            val prob : Double = 1.0 - math.pow(low.toFloat/candidate, k)
            if(score <= prob) {
                accepted = true
                nextValue = candidate
            }
        }
       
        if(signum == 0) {
            // Create incoming of quadruple value with 1/4 chance.
            nextValue * 4
        } else {
            nextValue *(-1)
        }
    }
}

object Simulation {
    def main(args: Array[String]) = {

        val testCases = new Simulator(Set(), List(1, -1, 1, 1, -2, 1,1,1, 10, -9, -4, 3,3,3,3,11,-10, -1, -8),"TESTCASE 1: Check of Functionality")
        val testCases2 = new Simulator(Set(), List(2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 9,-11), "TESTCASE 2: Direct Match Test")
        val testCases3 = new Simulator(Set(), List(2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, -11), "TESTCASE 3: Pruning Test, Direct Match impossible")
        
        var utxo4: Set[(Int,Long)] = Set()
        for(i <- 1 to 50000) {
            var utxo : (Int, Long) = (i,i.toLong)
            utxo4 = utxo4 + utxo
        }
        val testCases4 = new Simulator(utxo4, List(-50001), "TESTCASE 4: UTXO 1-50000, spend 50001")

        var utxo5: Set[(Int,Long)] = Set()
        for(i <- 1 to 20001) {
            var utxo : (Int, Long) = (i,1)
            if(i == 20000) {
                utxo = (i,20000)
            } else if(i == 20001) {
                utxo = (i,30000)
            }

            utxo5 = utxo5 + utxo
        }
        val testCases5 = new Simulator(utxo5, List(-50000), "TESTCASE 5: 20000 * 1 satoshi, and two that make up spending target of 50000")

        var utxo6: Set[(Int,Long)] = Set()
        var ops6: List[Long] = List()
        val rnd = new Random()
        for(i <- 1 to 5000) {
            val nextBalance = rnd.nextInt(250000-2500)
            var utxo : (Int, Long) = (i,nextBalance+2500)
            utxo6 = utxo6 + utxo
        }
        for(i <- 1 to 10000) {
            var nextOp : Long= rnd.nextInt(500000) - 250000
            if(nextOp >= 0 && nextOp < 540) {
                nextOp = nextOp +540
            } else if(nextOp > -540) {
                nextOp = nextOp -540
            }
            ops6 = nextOp :: ops6
        }
        val testCases6 = new Simulator(utxo6, ops6, "TESTCASE 6: Start with 5000 random UTXO from (2500,250000) satoshi, 10000 operations of spending or receiving (540,250000) satoshi")

        var utxo7: Set[(Int,Long)] = Set()
        var ops7: List[Long] = List()

        for(i <- 1 to 1000) {
            val nextBalance : Long = rnd.nextInt(10000000-2500).toLong
            var utxo = (i,nextBalance+2500)
            utxo7 = utxo7 + utxo
        }
        for(i <- 1 to 10000) {
            var nextOp : Long= (rnd.nextGaussian()*250000000).toLong
            if(nextOp >= 0 && nextOp < 540) {
                nextOp = nextOp +540
            } else if(nextOp > -540) {
                nextOp = nextOp -540
            }
            ops7 = nextOp :: ops7
        }
        val testCases7 = new Simulator(utxo7, ops7, "TESTCASE 7: Start with 1000 random UTXO from (2500 satoshi,0.1BTC), 10000 operations of 50-50 spending or receiving with Gaussian distribution (540 satoshi,2.5BTC).")
        
        var utxo8: Set[(Int,Long)] = Set()
        var ops8: List[Long] = List()

        for(i <- 1 to 1000) {
            val nextBalance : Long = rnd.nextInt(10000000-2500).toLong
            var utxo = (i,nextBalance+2500)
            utxo8 = utxo8 + utxo
        }
        val par = new ParetoGenerator(540, math.pow(10,8).toInt,1)
        for(i <- 1 to 10000) {
            var nextOp : Long= (par.next()).toLong
            if(nextOp >= 0 && nextOp < 540) {
                nextOp = nextOp +540
            } else if(nextOp > -540) {
                nextOp = nextOp -540
            }
            ops8 = nextOp :: ops8
        }
        val testCases8 = new Simulator(utxo8, ops8, "TESTCASE 8: Start with 1000 random UTXO from (2500 satoshi,0.1BTC), 10000 operations of 1-3 spending or receiving with Pareto distribution (540 satoshi,1BTC).")
println("--------------------------------------------------------------------------------------")
println("---------------TEST CASE 1 STARTING---------------------------------------------------")
println("--------------------------------------------------------------------------------------")
        testCases.simulate()
println("--------------------------------------------------------------------------------------")
println("---------------TEST CASE 2 STARTING---------------------------------------------------")
println("--------------------------------------------------------------------------------------")
        testCases2.simulate()
println("--------------------------------------------------------------------------------------")
println("---------------TEST CASE 3 STARTING---------------------------------------------------")
println("--------------------------------------------------------------------------------------")
        testCases3.simulate()
println("--------------------------------------------------------------------------------------")
println("---------------TEST CASE 4 STARTING---------------------------------------------------")
println("--------------------------------------------------------------------------------------")
        testCases4.simulate()
println("--------------------------------------------------------------------------------------")
println("---------------TEST CASE 5 STARTING---------------------------------------------------")
println("--------------------------------------------------------------------------------------")
        testCases5.simulate()
println("--------------------------------------------------------------------------------------")
println("---------------TEST CASE 6 STARTING---------------------------------------------------")
println("--------------------------------------------------------------------------------------")
        testCases6.simulate()
println("--------------------------------------------------------------------------------------")
println("---------------TEST CASE 7 STARTING---------------------------------------------------")
println("--------------------------------------------------------------------------------------")
        testCases7.simulate()
println("--------------------------------------------------------------------------------------")
println("---------------TEST CASE 8 STARTING---------------------------------------------------")
println("--------------------------------------------------------------------------------------")
        testCases8.simulate()
    }
}
