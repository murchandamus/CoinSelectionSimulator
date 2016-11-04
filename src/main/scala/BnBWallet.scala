package main.scala

import util.Random

class BnBWallet(name: String, utxoList: Set[Utxo], feePerKB: Long, debug: Boolean, minChange: Long = 100000) extends AbstractWallet(name, utxoList, feePerKB, debug) {
    val MIN_CHANGE = minChange
    val MIN_CHANGE_BEFORE_ADDING_TO_FEE = WalletConstants.DUST_LIMIT
    var branchAndBoundTries = 0

    var COST_PER_INPUT = WalletConstants.BYTES_PER_INPUT * feePerKB / 1000
    val extraCostForChange = WalletConstants.BYTES_PER_OUTPUT + WalletConstants.BYTES_PER_INPUT * feePerKB / 1000

    def selectCoins(target: Long, feePerKB: Long, nLockTime: Int): Set[Utxo] = {

        if (debug == true) {
            println(name + " is selecting for " + target + " in block " + nLockTime)
        }

        var selectedCoins: Set[Utxo] = Set()
        var smallerCoins: Set[Utxo] = Set()

        var adjustedTarget = target + WalletConstants.ONE_IN_ONE_OUT_TRANSACTION_SIZE * feePerKB / 1000

        for (utxo <- utxoPool) {
            if (utxo.value >= adjustedTarget && utxo.value <= adjustedTarget + extraCostForChange) {
                return Set(utxo) //Direct Match with one
            } else if (utxo.value < adjustedTarget) {
                smallerCoins += utxo
            }
        }

        val smallerCoinsTotal = selectionTotal(smallerCoins)
        val costToSpendSmallerCoins = (WalletConstants.ONE_IN_ONE_OUT_TRANSACTION_SIZE + (smallerCoins.size - 1) * WalletConstants.BYTES_PER_INPUT) * feePerKB / 1000
        if (smallerCoinsTotal >= target + costToSpendSmallerCoins
            && smallerCoinsTotal <= target + costToSpendSmallerCoins + extraCostForChange) {
            return smallerCoins
        }

        // Try Branch-and-Bound
        val utxoVec = smallerCoins.toArray
        val utxoVecSorted = utxoVec.sortWith(_.value > _.value)
        branchAndBoundTries = 1000000
        selectedCoins = branchAndBound(100, 0, Set[Utxo](), 0, target, utxoVecSorted, feePerKB)

        //Else select randomly.
        if (selectedCoins == Set()) {
            var randomizedUtxo = Random.shuffle((utxoPool).toList)

            while (randomizedUtxo.nonEmpty && selectionTotal(selectedCoins) < target + estimateFeeWithChange(target, selectedCoins, feePerKB) + MIN_CHANGE) {
                selectedCoins += randomizedUtxo.head
                if (debug == true) {
                    println(name + " added " + randomizedUtxo.head + ". Combination is now " + selectedCoins + ".")
                }
                randomizedUtxo = randomizedUtxo.drop(1)
            }
        }

        return selectedCoins
    }

    def branchAndBound(maxInputs: Int, depth: Int, selectedCoins: Set[Utxo], effectiveValueSelected: Long, target: Long, utxoVecSorted: Array[Utxo], feePerKB: Long): Set[Utxo] = {
        branchAndBoundTries -= 1
        if (effectiveValueSelected > target + WalletConstants.ONE_IN_ONE_OUT_TX_MIN_FEE - COST_PER_INPUT + extraCostForChange) {
            return Set()
        } else if (effectiveValueSelected >= target + WalletConstants.ONE_IN_ONE_OUT_TX_MIN_FEE - COST_PER_INPUT) {
            println(name + " matched " + target + " with " + selectedCoins + " in Branch-and-Bound at depth " + depth + ".")
            return selectedCoins
        } else if (branchAndBoundTries <= 0) {
            return Set()
        } else if (depth >= utxoVecSorted.size) {
            return Set()
        } else if (maxInputs == selectedCoins.size) {
            return Set()
        } else {
            var withThis = branchAndBound(maxInputs, depth + 1, selectedCoins + utxoVecSorted(depth), effectiveValueSelected + utxoVecSorted(depth).value - COST_PER_INPUT, target, utxoVecSorted, feePerKB)
            if (withThis != Set()) {
                return withThis
            } else {
                var withoutThis = branchAndBound(maxInputs, depth + 1, selectedCoins, effectiveValueSelected, target, utxoVecSorted, feePerKB)
                if (withoutThis != Set()) {
                    return withoutThis
                }
            }
        }
        return Set()
    }

    def estimateFeeWithChange(target: Long, selectedCoins: Set[Utxo], feePerKB: Long): Long = {
        var fee: Long = 0
        var transactionSize = WalletConstants.BYTES_TRANSACTION_OVERHEAD + selectedCoins.size * WalletConstants.BYTES_PER_INPUT + 2 * WalletConstants.BYTES_PER_OUTPUT

        fee += transactionSize * feePerKB / 1000

        return fee
    }
}
