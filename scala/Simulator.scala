
class Simulator(utxo: Set[(Int,Int)], operations: List[Int]) {
    val regularWallet = new Wallet("Regular", utxo,false, 1000) 
    val pruningWallet = new PruningWallet("Pruning", true, 1, utxo,false, 1000) 
    val pruningWalletMin2 = new PruningWallet("Pruning 2Min", true, 2, utxo,false,1000) 
    val pruningWalletMin3 = new PruningWallet("Pruning 3Min", true, 3, utxo,false,1000) 

    var wallets: List[Wallet] = List(regularWallet, pruningWallet, pruningWalletMin2, pruningWalletMin3)
    
    def simulate() {
        operations.foreach{
            x => if(x > 0) {
                wallets.foreach(_.receive(x))
            } else if(x < 0) {
                wallets.foreach(_.spend((-1)*x))
            }
        }
        wallets.foreach(_.printWalletStatus())
    }
}

object Simulation {
    def main(args: Array[String]) = {

        val testCases = new Simulator(Set(), List(1, -1, 1, 1, -2, 1,1,1, 10, -9, -4, 3,3,3,3,11,-10, -1, -8))
        val testCases2 = new Simulator(Set(), List(2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 9,-11))
        val testCases3 = new Simulator(Set(), List(2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, -11))
        
        var utxo4: Set[(Int,Int)] = Set()
        for(i <- 1 to 50000) {
            var utxo = (i,i)
            utxo4 = utxo4 + utxo
        }
        val testCases4 = new Simulator(utxo4, List(-50001))

        var utxo5: Set[(Int,Int)] = Set()
        for(i <- 1 to 20001) {
            var utxo = (i,1)
            if(i == 20000) {
                utxo = (i,20000)
            } else if(i == 20001) {
                utxo = (i,30000)
            }

            utxo5 = utxo5 + utxo
        }
        val testCases5 = new Simulator(utxo5, List(-50000))
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
    }
}
