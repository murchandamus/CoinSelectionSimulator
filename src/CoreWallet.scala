import util.Random

class CoreWallet(name: String, utxoList: Set[Utxo], feePerKB: Long, debug: Boolean, directMatchWidth: Long = 0) extends AbstractWallet(name, utxoList, feePerKB, debug) {
    var MIN_CHANGE = WalletConstants.CENT
    val MIN_CHANGE_BEFORE_ADDING_TO_FEE = WalletConstants.DUST_LIMIT
    val MIN_TX_FEE_1_1 = (WalletConstants.BYTES_PER_INPUT + WalletConstants.BYTES_PER_OUTPUT + WalletConstants.BYTES_TRANSACTION_OVERHEAD) * feePerKB / 1000
    val DIRECT_MATCH_ALLOWANCE = directMatchWidth
    val TRIES = 1000
    var currentTransactionFee: Long = 0 // Ugly hack to get transactionFee estimate from Core's selectCoins into spend.

    override def spend(target: Long, nLockTime: Int) {
        if (target > getWalletTotal()) {
            throw new IllegalArgumentException("Wallet was requested to spend " + target + " but only contained " + getWalletTotal() + ".");
        }
        val starttime: Long = System.currentTimeMillis
        var selectedCoins: Set[Utxo] = selectCoins(target, feePerKB, nLockTime)

        //var fee: Long = estimateFee(target, selectedCoins, feePerKB, MIN_CHANGE) ← Core estimates fee in Selection
        var fee = currentTransactionFee

        var change: Long = selectionTotal(selectedCoins) - target - fee

        val duration = (System.currentTimeMillis) - starttime

        var tx = new Transaction(name, target, change, fee, selectedCoins, nLockTime, duration)
        executeTransaction(tx)
        utxoSetSizes += utxoPool.size
        inTransitRatio += change.toDouble / getWalletTotal()
    }

    def selectCoins(target: Long, feePerKB: Long, nLockTime: Int): Set[Utxo] = {
        //currentTransactionFee = 0 <--- This would be an accurate depiction of CoreWallet, but really there aren't any zero-fee transactions anymore. Therefore:
        currentTransactionFee = WalletConstants.ONE_ONE_TX_MIN_FEE
        var selectionSuccess: Boolean = false
        var adjustedTarget = target
        var selection: Set[Utxo] = Set()

        while (!selectionSuccess) {
            adjustedTarget = target + currentTransactionFee

            selection = selectCoinsMinConf(adjustedTarget)

            var change = Math.max(0, selectionTotal(selection) - target - currentTransactionFee)
            if (change <= WalletConstants.DUST_LIMIT) {
                currentTransactionFee += change
                change = 0
            }

            var requiredFee: Long = estimateRequiredFee(selection.size, feePerKB, change > 0)

            //if (selectionTotal(selection) - requiredFee < target) {
            if (selectionTotal(selection) - currentTransactionFee < target || requiredFee > currentTransactionFee) {
                //Didn't have enough fee.

                if (debug == true) {
                    println(name + "'s selection couldn't pay the required fee of " + requiredFee + " after estimating fee of " + currentTransactionFee + ".")
                }
                currentTransactionFee = requiredFee

            } else {
                selectionSuccess = true
            }
        }

        return selection
    }

    def selectCoinsMinConf(adjustedTarget: Long): Set[Utxo] = {
        if (debug == true) {
            println(name + " is restarting selectCoinsMinConf with " + adjustedTarget + " as target.")
        }
        var selectedCoins: Set[Utxo] = Set()
        var smallerCoins: Set[Utxo] = Set()

        if (debug == true) {
            println(name + " is trying for a direct match:")
        }
        val bestSingleUtxoForDirectMatch = findMinimalSingleInput(adjustedTarget)

        //Case 1: Direct Match with Single UTXO
        if (bestSingleUtxoForDirectMatch != null && bestSingleUtxoForDirectMatch.value >= adjustedTarget && bestSingleUtxoForDirectMatch.value <= adjustedTarget + DIRECT_MATCH_ALLOWANCE) {
            if (debug == true) {
                println(name + " matched " + adjustedTarget + " directly as single UTXO " + bestSingleUtxoForDirectMatch + ".")
            }
            return Set(bestSingleUtxoForDirectMatch)
        }

        //Collect all utxo of lower value than target
        smallerCoins = utxoPool.filter(x => x.value < adjustedTarget + MIN_CHANGE)
        val bestSingleUtxo = findMinimalSingleInput(adjustedTarget + MIN_CHANGE)

        //Case 2: Direct Match with smaller UTXO
        var smallerCoinsTotal: Long = selectionTotal(smallerCoins)
        if (smallerCoinsTotal >= adjustedTarget && smallerCoinsTotal <= adjustedTarget + DIRECT_MATCH_ALLOWANCE) {
            if (debug == true) {
                println(name + " matched " + adjustedTarget + " by combining all UTXO smaller than target.")
            }
            return smallerCoins
        }

        //Case 3: Sum of smaller Coins doesn't exceed target → return bestSingle
        if (selectionTotal(smallerCoins) < adjustedTarget) {
            if (debug == true) {
                println(name + " didn't contain a combination of smaller UTXO to supersede " + adjustedTarget + ". Defaulting to smallest sufficient UTXO.")
            }
            return Set(bestSingleUtxo)
        }

        //Case 4: Knapsack variants
        var knapsackSelection: Set[Utxo] = knapsack(adjustedTarget, smallerCoins, DIRECT_MATCH_ALLOWANCE)

        // If no Direct Match in knapsack and enough to create change, overwrite knapsack aiming to create change
        if (selectionTotal(knapsackSelection) != adjustedTarget && selectionTotal(smallerCoins) >= adjustedTarget + MIN_CHANGE) {
            knapsackSelection = knapsack(adjustedTarget + MIN_CHANGE, smallerCoins, DIRECT_MATCH_ALLOWANCE)
        }

        // If we have a bestSingleUtxo, and either no direct match or small change, return bestSingle instead.
        if (bestSingleUtxo != null &&
            (selectionTotal(knapsackSelection) != adjustedTarget && selectionTotal(knapsackSelection) < adjustedTarget + MIN_CHANGE || bestSingleUtxo.value <= selectionTotal(knapsackSelection))) {

            return Set(bestSingleUtxo)
        } else {
            return knapsackSelection
        }

        return selectedCoins
    }

    def knapsack(adjustedTarget: Long, smallerCoins: Set[Utxo], directMatchAllowance: Long = 0): Set[Utxo] = {
        if (debug == true) {
            println(name + " is starting new round of knapsack with " + adjustedTarget + ".")
        }
        var bestSelection: Set[Utxo] = smallerCoins
        var bestTotal: Long = selectionTotal(smallerCoins)
        val rnd = new Random()
        val utxoVec = smallerCoins.toArray
        val utxoVecSorted = utxoVec.sortWith(_.value > _.value)
        if (debug == true) {
            println("utxoPool is " + utxoPool)
            println("utxoVec is " + utxoVec.mkString(", "))
            println("utxoVecSorted is " + utxoVecSorted.mkString(", "))
        }

        for (i <- 1 to TRIES) {
            var total: Long = 0
            var selected: Vector[Boolean] = Vector.fill(utxoVec.size)(false)
            var currentSelection: Set[Utxo] = Set()
            var targetReached: Boolean = false
            var pass: Int = 0

            while (pass < 2 && targetReached == false) {
                for (u <- 0 until utxoVecSorted.size) {
                    if ((rnd.nextBoolean() == true || pass > 0) && selected(u) == false) {
                        selected = selected.updated(u, true)
                        val randomUtxo = utxoVecSorted(u)
                        total = total + randomUtxo.value

                        if (debug == true) {
                            var debugSelection = currentSelection + randomUtxo
                            if (debugSelection.size <= 20) {
                                println(name + " randomed " + randomUtxo + ". Combination is now " + debugSelection + " in try number " + i + ".")
                            } else {
                                println(name + " randomed " + randomUtxo + ". Combination has now " + debugSelection.size + " inputs, with a total of " + total + "  in try number " + i + ".")
                            }
                        }

                        if (total >= adjustedTarget && total <= adjustedTarget + directMatchAllowance) {
                            currentSelection += randomUtxo
                            println(name + " matched " + adjustedTarget + " by combining random UTXO. " + currentSelection + " Knapsack stopped after " + i + " tries.")
                            return currentSelection
                        } else if (total > adjustedTarget) {
                            targetReached = true

                            if (total < bestTotal) {
                                bestTotal = total
                                bestSelection = currentSelection + randomUtxo

                                if (debug) {
                                    if (bestSelection.size <= 20) {
                                        println(name + " found better combination with " + bestSelection.size + " inputs, with a total of " + total + ", using " + bestSelection + " in try " + i + ".")
                                    } else {
                                        println(name + " found better combination with " + bestSelection.size + " inputs, with a total of " + total + ", in try " + i + ".")
                                    }
                                }

                            }
                            selected = selected.updated(u, false)
                            total = total - randomUtxo.value
                        } else {
                            currentSelection += randomUtxo
                        }
                    }
                }
                pass = pass + 1
            }
        }

        return bestSelection
    }
}
