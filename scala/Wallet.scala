import util.Random

class Wallet(name: String, utxoList: Set[(Int,Long)], debug: Boolean, knapsackLimit: Int) {
    var countSpentUtxo: Int = 0
    var countReceivedUtxo: Int = 0
    var utxoPool: Set[(Int,Long)] = utxoList
    var utxoIndex: Int = utxoPool.size

    def receive(outputValue: Long) {
        if(outputValue > 0) {
            var utxo = (utxoIndex,outputValue)
            utxoIndex = utxoIndex+1
            utxoPool = utxoPool+utxo
            countReceivedUtxo = countReceivedUtxo+1
            println(name + " received " + outputValue + " satoshi. It now has " + utxoPool.size + " UTXO, with total value of " + getWalletTotal() + " satoshi.")
        }
    }

    def getWalletTotal() : Long = {
        var total : Long = 0
        utxoPool.foreach(total+=_._2)
        return total
    }

    def findMinimalSingleInput(target: Long): (Int,Long) = {
        var currentBest = (-1,Long.MaxValue)
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

    def spend(target: Long) {
        val starttime: Long = System.currentTimeMillis /1000
        var selectedCoins: Set[(Int, Long)] = Set()
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
            var sum : Long = 0
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

        var change : Long = selectionTotal(selectedCoins) - target

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
        if(change > 0) {
            receive(change)
        }
        val duration = starttime - (System.currentTimeMillis / 1000) 
        println("To spend " + target + ", " + name + " selected " + selectedCoins.size + " inputs, with a total value of " + selectionTotal(selectedCoins) + " satoshi. The change was " + change + ". The wallet now has " + utxoPool.size + " utxo, worth "+ getWalletTotal() + " satoshi. It took " + duration + " to calculate.")
    }

    def selectionTotal(selection: Set[(Int,Long)]) : Long = {
        var totalValue : Long = 0
        selection.foreach(totalValue+= _._2)
        return totalValue
    }

    def knapsack(target: Long, tries: Int) : Set[(Int,Long)] = {
        var bestSelection: Set[(Int,Long)] = Set()
        var bestTotal: Long = getWalletTotal+1
        val rnd = new Random()
        val utxoVec = utxoPool.toArray

        for( i <- 1 to tries) {
            var total : Long = 0
            var selected: Vector[Boolean] = Vector.fill(utxoVec.size)(false)
            var currentSelection: Set[(Int,Long)] = Set()
            while (total < target) {
                val randomIndex = rnd.nextInt(utxoPool.size)
                if(selected(randomIndex) == false) {
                    selected=selected.updated(randomIndex,true) 

                    val randomUtxo = utxoVec(randomIndex)
                    currentSelection = currentSelection + randomUtxo

                    if(debug == true) {
                        println(name + " randomed " + randomUtxo + ". Combination is now " + currentSelection + " in try number " + i + ".")
                    }
                    total = total + randomUtxo._2
                    if(total > target) {
                        total = selectionTotal(currentSelection)
                    }
                }
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

