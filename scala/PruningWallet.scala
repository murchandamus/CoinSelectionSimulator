import util.Random

class PruningWallet(name: String, prune: Boolean, minInputs: Int, utxoList: Set[(Int,Int)], debug: Boolean, knapsackLimit: Int) extends Wallet (name, utxoList, debug, knapsackLimit) {
    override def spend(target: Int) {
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

    override def knapsack(target: Int, tries: Int) : Set[(Int,Int)] = {
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

            println(name + " had selected " + currentSelection.size + " coins before pruning.")
            var change = total - target
            if(prune && change > 0) {
                for(coin <- currentSelection) {
                    if(coin._2 <= change && currentSelection.size > minInputs) {
                        println(name + " pruned input with " + coin._2 + " because it was smaller than change of " + change + ".")
                        change -= coin._2
                        currentSelection = currentSelection - coin
                    }
                }
            }
            println(name + " has selected " + currentSelection.size + " coins after pruning.")

            total = selectionTotal(currentSelection)

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
}
