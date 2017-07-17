package one.murch.bitcoin.coinselection

import scala.util.Random

class StackEfficientTailRecursiveBnB(name: String, utxoList: Set[Utxo], feePerKB: Long, debug: Boolean, minChange: Long = 100000) extends AbstractWallet(name, utxoList, feePerKB, debug) {
    val MIN_CHANGE = minChange
    val MIN_CHANGE_BEFORE_ADDING_TO_FEE = WalletConstants.DUST_LIMIT
    var branchAndBoundTries = 0
    val maxTries = 1000000
    var utxoVecSorted: Array[Utxo] = Array()
    var lookaheadVec: Array[Long] = Array()
    var lastIncluded: Int = -1
    var selectedUtxo: Array[Boolean] = Array()
    var depth: Int = -1
    var remainingValueToSelect: Long = -1

    val COST_PER_INPUT = WalletConstants.BYTES_PER_INPUT * feePerKB / 1000

    val extraCostForChange = (WalletConstants.BYTES_PER_OUTPUT + WalletConstants.BYTES_PER_INPUT) * feePerKB / 1000

    def selectCoins(target: Long, feePerKB: Long, nLockTime: Int): Set[Utxo] = {

        if (debug == true) {
            println(name + " is selecting for " + target + " in block " + nLockTime)
        }

        var selectedCoins: Set[Utxo] = Set()

        remainingValueToSelect = target + (WalletConstants.ONE_IN_ONE_OUT_TRANSACTION_SIZE - WalletConstants.BYTES_PER_INPUT) * feePerKB / 1000

        // Try Branch-and-Bound
        val utxoVec = utxoPool.toArray
        utxoVecSorted = utxoVec.sortWith(_.value > _.value)

        lookaheadVec = createLookahead(utxoVecSorted, feePerKB)
        selectedUtxo  = new Array[Boolean](utxoVec.size)

        for (i <- 0 until utxoVec.size) {
            selectedUtxo(i) = false
        }

        branchAndBoundTries = maxTries
        depth = 0
        if(branchAndBound() == true) {
            for(i <- 0 to lastIncluded) {
                if(selectedUtxo(i) == true) {
                    selectedCoins += utxoVecSorted(i)
                }
            }
        }

        //Else select randomly.
        if (selectedCoins == Set()) {
            var randomizedUtxo = Random.shuffle((utxoPool).toList)

            while (randomizedUtxo.nonEmpty && selectionTotal(selectedCoins) < target + estimateFeeWithChange(target, selectedCoins, feePerKB) + MIN_CHANGE) {
                if (randomizedUtxo.head.value >= COST_PER_INPUT) {
                    selectedCoins += randomizedUtxo.head
                    if (debug == true) {
                        println(name + " added " + randomizedUtxo.head + ". Combination is now " + selectedCoins + ".")
                    }
                }
                randomizedUtxo = randomizedUtxo.tail
            }
        }

        return selectedCoins
    }

    def branchAndBound(): Boolean = {
        while (true) {
            if(debug == true) {
                println(name + " trying to select " + remainingValueToSelect + " at depth " + depth + " after " + (maxTries - branchAndBoundTries) + " tries.")
            }
            branchAndBoundTries -= 1
            if (remainingValueToSelect + extraCostForChange < 0) {
                // Cut: Selected more than target plus cost for change.
                remainingValueToSelect = remainingValueToSelect + utxoVecSorted(lastIncluded).value - COST_PER_INPUT
                selectedUtxo(lastIncluded) = false
                depth = lastIncluded + 1
                while (lastIncluded >= 0 && selectedUtxo(lastIncluded) == false) {
                    lastIncluded -= 1
                }
            } else if (remainingValueToSelect <= 0) {
                println(name + " matched with donation of " + (remainingValueToSelect * -1) + " in Branch-and-Bound at depth " + depth + " after " + (maxTries - branchAndBoundTries) + " tries.")
                return true
            } else if (branchAndBoundTries <= 0) {
                return false
            } else if (depth >= utxoVecSorted.size) {
              if(lastIncluded < 0) {
                  return false
              }
                remainingValueToSelect = remainingValueToSelect + utxoVecSorted(lastIncluded).value - COST_PER_INPUT
                selectedUtxo(lastIncluded) = false
                depth = lastIncluded + 1
                while (lastIncluded >= 0 && selectedUtxo(lastIncluded) == false) {
                    lastIncluded -= 1
                }
            } else if (remainingValueToSelect > lookaheadVec(depth)) {
                if (debug == true) {
                    println(name + " cut a branch at depth " + depth + " due to remainder of " + remainingValueToSelect + " exceeding the lookahead " + lookaheadVec(depth) + ".")
                }
                if (lastIncluded < 0) {
                    return false
                }
                remainingValueToSelect = remainingValueToSelect + utxoVecSorted(lastIncluded).value - COST_PER_INPUT
                selectedUtxo(lastIncluded) = false
                depth = lastIncluded + 1
                while (lastIncluded >= 0 && selectedUtxo(lastIncluded) == false) {
                    lastIncluded -= 1
                }
            } else {
                remainingValueToSelect = remainingValueToSelect - utxoVecSorted(depth).value + COST_PER_INPUT
                selectedUtxo(depth) = true
                lastIncluded = depth
                depth += 1
            }
        }
      return false
    }

    def estimateFeeWithChange(target: Long, selectedCoins: Set[Utxo], feePerKB: Long): Long = {
        var fee: Long = 0
        val transactionSize = WalletConstants.BYTES_TRANSACTION_OVERHEAD + selectedCoins.size * WalletConstants.BYTES_PER_INPUT + 2 * WalletConstants.BYTES_PER_OUTPUT

        fee += transactionSize * feePerKB / 1000

        return fee
    }
    
    def createLookahead(utxoVecSorted: Array[Utxo], feePerKB: Long) : Array[Long] = {
        var lookaheadList : List[Long] = List()
    
        if(utxoVecSorted.length > 0) {
            var sumOfTail : Long = 0
            for(i <- utxoVecSorted.length - 1 to 0 by -1) {
                val effValue : Long = utxoVecSorted(i).value - COST_PER_INPUT
                if(effValue > 0) {
                    sumOfTail = sumOfTail + effValue
                } 
                lookaheadList = sumOfTail :: lookaheadList
            }
        }
        return lookaheadList.toArray
    }
}
