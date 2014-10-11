import util.Random

class DoubleWallet(name: String, prune: Boolean, minInputs: Int, utxoList: Set[(Int,Long)], debug: Boolean, knapsackLimit: Int) extends Wallet (name, utxoList, debug, knapsackLimit) {
    override def spend(target: Long) {
        val starttime: Long = System.currentTimeMillis
        var selectedCoins: Set[(Int, Long)] = Set()
        val bestSingleUtxo = findMinimalSingleInput(target)
        var selectionFinished: Boolean = false
        //Case 0: Spend target bigger than wallet content
        if(target > getWalletTotal) {
            selectionFinished = true
        }
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
            val bestSingleUtxo = findMinimalSingleInput(target*2)
            if(bestSingleUtxo._2 >= total) {
                selectedCoins = bestCombination
            } else {
        //Case 4a) Smallest sufficient is better than Knapsack result
                selectedCoins = Set(bestSingleUtxo)
            }
            selectionFinished = true
        }

        var change : Long = selectionTotal(selectedCoins) - target

        if(debug == true) {
            println(name + " had selected " + selectedCoins.size + " coins before pruning.")
        }

        if(prune && change > target) {
            for(coin <- selectedCoins) {
                if(coin._2 <= change - target*3/4 && selectedCoins.size > minInputs) {
                    println(name + " pruned input with " + coin._2 + " because it was smaller than change of " + change + ".")
                    change -= coin._2
                    selectedCoins = selectedCoins - coin
                }
            }
        }
        if(debug == true) {
            println(name + " has selected " + selectedCoins.size + " coins after pruning.")
        }

        countSpentUtxo = countSpentUtxo + selectedCoins.size
        inputSetSizes = selectedCoins.size :: inputSetSizes 
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
        val duration = (System.currentTimeMillis) - starttime  
        println("To spend " + target + ", " + name + " selected " + selectedCoins.size + " inputs, with a total value of " + selectionTotal(selectedCoins) + " satoshi. The change was " + change + ". The wallet now has " + utxoPool.size + " utxo, worth "+ getWalletTotal() + " satoshi. It took " + duration + "ms to calculate.")
        if(change > 0) {
            receive(change)
            changesCreated = change :: changesCreated 
        }
    }

    override def knapsack(target: Long, tries: Int) : Set[(Int,Long)] = {
        var bestSelection: Set[(Int,Long)] = Set()
        var bestTotal: Long = getWalletTotal+1
        val rnd = new Random()
        val utxoVec = utxoPool.toArray

        for( i <- 1 to tries) {
            var total : Long = 0
            var selected: Vector[Boolean] = Vector.fill(utxoVec.size)(false)
            var currentSelection: Set[(Int,Long)] = Set()
            if(target*2 < getWalletTotal) {
                while (total < target*2) {
                    val randomIndex = rnd.nextInt(utxoPool.size)
                    if(selected(randomIndex) == false) {
                        selected=selected.updated(randomIndex,true) 

                        val randomUtxo = utxoVec(randomIndex)
                        currentSelection = currentSelection + randomUtxo

                        if(debug == true) {
                            println(name + " randomed " + randomUtxo + ". Combination is now " + currentSelection + " in try number " + i + ".")
                        }
                        total = total + randomUtxo._2
                        if(total > target*2) {
                            total = selectionTotal(currentSelection)
                        }
                    }
                }

                if(debug) {
                    println(name + " had selected " + currentSelection.size + " coins before pruning.")
                }
                var change : Long = total - target
                if(prune && change > target) {
                    for(coin <- currentSelection) {
                        if(coin._2 <= change - target && currentSelection.size > minInputs) {
                            if(debug) {
                                println(name + " pruned input with " + coin._2 + " because it was smaller than change of " + change + ".")
                            }
                            change -= coin._2
                            currentSelection = currentSelection - coin
                        }
                    }
                }
                if(debug) {
                    println(name + " has selected " + currentSelection.size + " coins after pruning.")
                }
            } else {
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

            if(debug) {
                println(name + " had selected " + currentSelection.size + " coins before pruning.")
            }
            var change = total - target
            if(prune && change > 0) {
                for(coin <- currentSelection) {
                    if(coin._2 <= change && currentSelection.size > minInputs) {
                        if(debug) {
                            println(name + " pruned input with " + coin._2 + " because it was smaller than change of " + change + ".")
                        }
                        change -= coin._2
                        currentSelection = currentSelection - coin
                    }
                }
            }
            if(debug) {
                println(name + " has selected " + currentSelection.size + " coins after pruning.")
            }

            }

            total = selectionTotal(currentSelection)

            if(total == target) {
                println(name + " matched " + target + " by combining random UTXO. " + currentSelection + " Knapsack stopped after " + i + " tries.")
                return currentSelection
            } else if(total < bestTotal) {
                bestSelection = currentSelection
                bestTotal = total
                //if(debug == true) {
                    if(currentSelection.size <=20) {
                        if(debug) {
                            println(name + " found better combination with " + currentSelection.size + " inputs, with a total of " + total +", using " + currentSelection + " in try " + i + ".")
                        }
                    } else {
                        if(debug) {
                            println(name + " found better combination with " + currentSelection.size + " inputs, with a total of " + total +", in try " + i + ".")
                        }
                    }
                //}
            }
        }
        return bestSelection
    }
}
