import util.Random

class Wallet(name: String, prune: Boolean, minInputs: Int, utxoList: Set[(Int,Int)], debug: Boolean, knapsackLimit: Int) {
    var countSpentUtxo: Int = 0
    var countReceivedUtxo: Int = 0
    var utxoPool: Set[(Int,Int)] = utxoList
    var utxoIndex: Int = utxoPool.size

    def receive(outputValue: Int) {
        if(outputValue > 0) {
            var utxo = (utxoIndex,outputValue)
            utxoIndex = utxoIndex+1
            utxoPool = utxoPool+utxo
            countReceivedUtxo = countReceivedUtxo+1
            println(name + " received " + outputValue + " satoshi. It now has " + utxoPool.size + " UTXO, with total value of " + getWalletTotal() + " satoshi.")
        }
    }

    def getWalletTotal() : Int = {
        var total = 0
        utxoPool.foreach(total+=_._2)
        return total
    }

    def findMinimalSingleInput(target: Int): (Int,Int) = {
        var currentBest = (-1,Int.MaxValue)
        for (utxo <- utxoPool) {
            if(utxo._2 >= target && utxo._2 < currentBest._2) {
                currentBest = utxo
            }
        }
        if(debug == true) {
            println(name + "'s best single input was " + currentBest +".")
        }
        return currentBest
    }

    def spend(target: Int) {
        var selectedCoins: Set[(Int, Int)] = Set()
        val bestSingleUtxo = findMinimalSingleInput(target)
        var selectionFinished: Boolean = false
        //Case 1: pool contains target
        if(bestSingleUtxo._2 == target) {
            selectedCoins = Set(bestSingleUtxo)
            selectionFinished = true
            if(debug == true) {
                println(name + " matched " + target + " directly as single UTXO " + bestSingleUtxo + ".")
            }
        }

        //Case 2: All utxo smaller than target match target
        if(true != selectionFinished) {
            var smallerElements = utxoPool.filter(x =>x._2 < target)
            var sum = 0
            for(small <- smallerElements) {
                sum = sum + small._2
            }
            if(sum == target) {
                selectedCoins = smallerElements
                selectionFinished = true
                if(debug == true) {
                    println(name + " matched " + target + " by combining all UTXO smaller than target.")
                }
            } else if (sum < target) {
        //Case 3: Sum of smaller UTXO is not sufficient
                selectedCoins = Set(bestSingleUtxo)
                selectionFinished = true
                if(debug == true) {
                    println(name + " didn't contain a combination of smaller UTXO to supersede " + target + ". Defaulting to smallest sufficient UTXO.")
                }
            }
        }
        //Case 4: Knapsack x1000
        if(!selectionFinished) {
            val bestCombination = knapsack(target, knapsackLimit)
            val total = selectionTotal(bestCombination)
            if(bestSingleUtxo._2 >= total) {
                selectedCoins = bestCombination
            } else {
        //Case 4a) Smallest sufficient is better than Knapsack result
                selectedCoins = Set(bestSingleUtxo)
            }
            selectionFinished = true
        }

        var change = selectionTotal(selectedCoins) - target

        println(name + " had selected " + selectedCoins.size + " coins before pruning.")

        if(prune && change > 0) {
            for(coin <- selectedCoins) {
                if(coin._2 <= change && selectedCoins.size > minInputs) {
                    println(name + " pruned input with " + coin._2 + " because it was smaller than change of " + change + ".")
                    change -= coin._2
                    selectedCoins = selectedCoins - coin
                }
            }
        }
        println(name + " has selected " + selectedCoins.size + " coins after pruning.")

        countSpentUtxo = countSpentUtxo + selectedCoins.size
        var utxoPoolSizeBefore = utxoPool.size
        if(debug == true) {
            println("UtxoPool is " + utxoPool)
        }
        for(coin <- selectedCoins) {
            if(debug == true) {
                println("UtxoPool is " + utxoPool + " before removing " + coin)
            }
            utxoPool = utxoPool - coin
            if(debug == true) {
                println("UtxoPool is " + utxoPool +" after removing " + coin)
            }
        }

        var utxoPoolSizeAfter = utxoPool.size
        if (utxoPoolSizeBefore - utxoPoolSizeAfter != selectedCoins.size) {
            var expected = utxoPoolSizeBefore - selectedCoins.size 
            println("ERROR: utxoPool.size was " + utxoPoolSizeBefore + " and is now " + utxoPoolSizeAfter +". It should be " + expected + " instead though.")
        }
        println("To spend " + target + ", " + name + " selected " + selectedCoins.size + " inputs, with a total value of " + selectionTotal(selectedCoins) + " satoshi. The change was " + change + ". The wallet now has " + utxoPool.size + " utxo, worth "+ getWalletTotal() + " satoshi.")
        if(change > 0) {
            receive(change)
        }
    }

    def selectionTotal(selection: Set[(Int,Int)]) : Int = {
        var totalValue = 0
        selection.foreach(totalValue+= _._2)
        return totalValue
    }

    def knapsack(target: Int, tries: Int) : Set[(Int,Int)] = {
        var bestSelection: Set[(Int,Int)] = Set()
        var bestTotal: Int = getWalletTotal+1
        val rnd = new Random()

        for( i <- 1 to tries) {
            var total = 0
            var currentSelection: Set[(Int,Int)] = Set()
            while (total < target) {
                val utxoVec = utxoPool.toArray
                val randomUtxo = utxoVec(rnd.nextInt(utxoPool.size))
                currentSelection = currentSelection + randomUtxo

                if(debug == true) {
                    println(name + " randomed " + randomUtxo + ". Combination is now " + currentSelection + " in try number " + i + ".")
                }
                total = selectionTotal(currentSelection)
            }
            if(total == target) {
                println(name + " matched " + target + " by combining random UTXO. " + currentSelection + " Knapsack stopped after " + i + " tries.")
                return currentSelection
            } else if(total < bestTotal) {
                bestSelection = currentSelection
                bestTotal = total
                //if(debug == true) {
                    if(currentSelection.size <=20) {
                        println(name + " found better combination with " + currentSelection.size + " inputs, with a total of " + total +", using " + currentSelection + " in try " + i + ".")
                    } else {
                        println(name + " found better combination with " + currentSelection.size + " inputs, with a total of " + total +", in try " + i + ".")
                    }
                //}
            }
        }
        return bestSelection
    }

    def printWalletStatus() {
        println(name + " finally has "+ getWalletTotal() + " satoshi, in " + utxoPool.size + " UTXO, by receiving " +countReceivedUtxo + " UTXO and spending " + countSpentUtxo + ".")
    }
}

class Simulator(utxo: Set[(Int,Int)], operations: List[Int]) {
    val regularWallet = new Wallet("Regular", false, 1, utxo,false, 1000) 
    val pruningWallet = new Wallet("Pruning", true, 1, utxo,false, 1000) 
    val pruningWalletMin2 = new Wallet("Pruning 2Min", true, 2, utxo,false,1000) 
    val pruningWalletMin3 = new Wallet("Pruning 3Min", true, 3, utxo,false,1000) 

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
        //testCases.simulate()
        //testCases2.simulate()
        //testCases3.simulate()
        testCases4.simulate()

        var utxo5: Set[(Int,Int)] = Set()
        for(i <- 1 to 201) {
            var utxo = (i,1)
            if(i == 200) {
                utxo = (i,20000)
            } else if(i == 201) {
                utxo = (i,30000)
            }

            utxo5 = utxo5 + utxo
        }
        val testCases5 = new Simulator(utxo5, List(-50000))
        //testCases.simulate()
        //testCases2.simulate()
        //testCases3.simulate()
        testCases5.simulate()
    }
}
