import util.Random

class Simulator(utxo: Set[(Int,Long)], operations: List[Long]) {
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
        wallets.foreach(_.printWalletStatus())
        println("-------------------------------------------------------------------------------")
    }
}

object Simulation {
    def main(args: Array[String]) = {

        val testCases = new Simulator(Set(), List(1, -1, 1, 1, -2, 1,1,1, 10, -9, -4, 3,3,3,3,11,-10, -1, -8))
        val testCases2 = new Simulator(Set(), List(2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 9,-11))
        val testCases3 = new Simulator(Set(), List(2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, -11))
        
        var utxo4: Set[(Int,Long)] = Set()
        for(i <- 1 to 50000) {
            var utxo : (Int, Long) = (i,i.toLong)
            utxo4 = utxo4 + utxo
        }
        val testCases4 = new Simulator(utxo4, List(-50001))

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
        val testCases5 = new Simulator(utxo5, List(-50000))

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
        val testCases6 = new Simulator(utxo6, ops6)

        var utxo7: Set[(Int,Long)] = Set()
        var ops7: List[Long] = List()

        for(i <- 1 to 10000) {
            val nextBalance : Long = rnd.nextInt(10000000-2500).toLong
            var utxo = (i,nextBalance+2500)
            utxo7 = utxo7 + utxo
        }
        for(i <- 1 to 1000) {
            var nextOp : Long= (rnd.nextGaussian()*250000000).toLong
            if(nextOp >= 0 && nextOp < 540) {
                nextOp = nextOp +540
            } else if(nextOp > -540) {
                nextOp = nextOp -540
            }
            ops7 = nextOp :: ops7
        }
        val testCases7 = new Simulator(utxo7, ops7)
        /*
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
        */
println("--------------------------------------------------------------------------------------")
println("---------------TEST CASE 6 STARTING---------------------------------------------------")
println("--------------------------------------------------------------------------------------")
        testCases6.simulate()
println("--------------------------------------------------------------------------------------")
println("---------------TEST CASE 7 STARTING---------------------------------------------------")
println("--------------------------------------------------------------------------------------")
        testCases7.simulate()
    }
}
