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

        var adjustedTarget = target + WalletConstants.ONE_ONE_TRANSACTION_SIZE * feePerKB / 1000

        for (utxo <- utxoPool) {
            if (utxo.value >= adjustedTarget && utxo.value <= adjustedTarget + extraCostForChange) {
                return Set(utxo) //Direct Match with one
            } else if (utxo.value < adjustedTarget) {
                smallerCoins += utxo
            }
        }

        val smallerCoinsTotal = selectionTotal(smallerCoins)
        val costToSpendSmallerCoins = +(WalletConstants.ONE_ONE_TRANSACTION_SIZE + (smallerCoins.size - 1) * WalletConstants.BYTES_PER_INPUT) * feePerKB / 1000
        if (smallerCoinsTotal >= target + costToSpendSmallerCoins
            && smallerCoinsTotal <= target + costToSpendSmallerCoins + extraCostForChange) {
            return smallerCoins
        }

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

    def branchAndBound(maxInputs: Int, depth: Int, selectedSoFar: Set[Utxo], valSoFar: Long, target: Long, utxoVecSorted: Array[Utxo], feePerKB: Long): Set[Utxo] = {
        branchAndBoundTries -= 1
        if (valSoFar > target + WalletConstants.ONE_ONE_TX_MIN_FEE - COST_PER_INPUT + extraCostForChange) {
            return Set()
        } else if (valSoFar >= target + WalletConstants.ONE_ONE_TX_MIN_FEE - COST_PER_INPUT) {
            println(name + " matched " + target + " with " + selectedSoFar + " in Branch-and-Bound at depth " + depth + ".")
            return selectedSoFar
        } else if (branchAndBoundTries <= 0) {
            return Set()
        } else if (depth >= utxoVecSorted.size) {
            return Set()
        } else if (maxInputs == selectedSoFar.size) {
            return Set()
        } else {
            var withThis = branchAndBound(maxInputs, depth + 1, selectedSoFar + utxoVecSorted(depth), valSoFar + utxoVecSorted(depth).value - COST_PER_INPUT, target, utxoVecSorted, feePerKB)
            if (withThis != Set()) {
                return withThis
            } else {
                var withoutThis = branchAndBound(maxInputs, depth + 1, selectedSoFar, valSoFar, target, utxoVecSorted, feePerKB)
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
