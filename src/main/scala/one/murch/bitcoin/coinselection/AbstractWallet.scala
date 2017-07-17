package one.murch.bitcoin.coinselection

import scala.collection.mutable.ListBuffer

abstract class AbstractWallet(var name: String, var utxoList: Set[Utxo], var feePerKB: Long = WalletConstants.FEE_PER_KILOBYTE, var debug: Boolean = false) {
    var countSpentUtxo: Int = 0
    var inputSetSizes: ListBuffer[Int] = new ListBuffer[Int]()
    var pastTransactions: ListBuffer[Transaction] = new ListBuffer[Transaction]()
    var countReceivedUtxo: Int = 0
    var utxoPool: Set[Utxo] = utxoList
    var utxoSetSizes: ListBuffer[Int] = new ListBuffer[Int]()
    if (debug == true) { println(name + " has set " + utxoList + " which should be also in " + utxoPool) }
    var utxoIndex: Int = utxoPool.size + 1
    var changesCreated: ListBuffer[Long] = new ListBuffer[Long]()
    var inTransitRatio: ListBuffer[Double] = new ListBuffer[Double]()
    val MIN_CHANGE_BEFORE_ADDING_TO_FEE: Long

    def receive(outputValue: Long, nLockTime: Int, isChange: Boolean = false) {
        if (outputValue > 0) {
            var utxo = new Utxo(utxoIndex, outputValue, nLockTime)
            utxoIndex = utxoIndex + 1
            utxoPool = utxoPool + utxo
            countReceivedUtxo = countReceivedUtxo + 1
            if (!isChange) {
                println(name + " received " + outputValue + " satoshi in " + nLockTime + ". It now has " + utxoPool.size + " UTXO, with total value of " + getWalletTotal() + " satoshi.")
            }
        }
        utxoSetSizes += utxoPool.size
    }

    def getWalletTotal(): Long = {
        var total: Long = 0
        utxoPool.foreach(total += _.value)
        return total
    }

    def findMinimalSingleInput(target: Long): Utxo = {
        var currentBest = findBiggestInput()

        for (utxo <- utxoPool) {
            if (utxo.value >= target && utxo.value < currentBest.value) {
                currentBest = utxo
            }
        }
        if (debug == true) {
            println(name + "'s best single input was " + currentBest + ".")
        }
        if (currentBest.value < target) {
            return null
        }
        return currentBest
    }

    def findBiggestInput(): Utxo = {
        var biggestUtxo = utxoPool.head

        for (utxo <- utxoPool) {
            if (utxo.value > biggestUtxo.value) {
                biggestUtxo = utxo
            }
        }
        return biggestUtxo
    }

    def selectCoins(target: Long, feePerKB: Long, nLockTime: Int): Option[Set[Utxo]]

    def spend(selectedCoins: Set[Utxo], target: Long, nLockTime: Int) {
        var fee: Long = estimateFee(target, selectedCoins, feePerKB, MIN_CHANGE_BEFORE_ADDING_TO_FEE)

        var change: Long = selectionTotal(selectedCoins) - target - fee

        var tx = new Transaction(name, target, change, fee, selectedCoins, nLockTime, 0)
        executeTransaction(tx)
        utxoSetSizes += utxoPool.size
        inTransitRatio += change.toDouble / getWalletTotal()
    }

    def estimateFee(target: Long, selectedCoins: Set[Utxo], feePerKB: Long, limitForAddingChangeToFee: Long): Long = {
        var fee: Long = 0
        var transactionSize = WalletConstants.BYTES_TRANSACTION_OVERHEAD + selectedCoins.size * WalletConstants.BYTES_PER_INPUT + WalletConstants.BYTES_PER_OUTPUT

        fee = transactionSize * feePerKB / 1000
        var change = Math.max(0, selectionTotal(selectedCoins) - target - fee)

        if (change - (WalletConstants.BYTES_PER_OUTPUT * feePerKB / 1000) <= limitForAddingChangeToFee) {
            //Not enough for change, add to fee
            fee = fee + change
        } else {
            transactionSize += WalletConstants.BYTES_PER_OUTPUT
            fee = transactionSize * feePerKB / 1000
        }
        return fee
    }

    def estimateRequiredFee(inputs: Int, feePerKB: Long, change: Boolean): Long = {
        var fee: Long = 0
        var transactionSize = WalletConstants.BYTES_TRANSACTION_OVERHEAD + inputs * WalletConstants.BYTES_PER_INPUT + WalletConstants.BYTES_PER_OUTPUT

        if (change == true) {
            transactionSize += WalletConstants.BYTES_PER_OUTPUT
        }

        fee = transactionSize * feePerKB / 1000
        return fee
    }

    def executeTransaction(tx: Transaction) {
        var utxoPoolSizeBefore = utxoPool.size

        if (debug == true) {
            println("UtxoPool is " + utxoPool)
        }
        for (coin <- tx.inputSet) {
            if (debug == true) {
                println("UtxoPool is " + utxoPool + " before removing " + coin)
            }
            utxoPool = utxoPool - coin
            if (debug == true) {
                println("UtxoPool is " + utxoPool + " after removing " + coin)
            }
        }

        var utxoPoolSizeAfter = utxoPool.size
        if (utxoPoolSizeBefore - utxoPoolSizeAfter != tx.inputSet.size) {
            var expected = utxoPoolSizeBefore - tx.inputSet.size
            println("ERROR: utxoPool.size was " + utxoPoolSizeBefore + " and is now " + utxoPoolSizeAfter + ". It should be " + expected + " instead though.")
        }

        pastTransactions += tx
        inputSetSizes += tx.inputSet.size
        countSpentUtxo += tx.inputSet.size

        if (tx.change > 0) {
            receive(tx.change, tx.block, true)
            changesCreated += tx.change
        }
        printTransactionReport(tx)
    }

    def selectionTotal(selection: Set[Utxo]): Long = {
        var totalValue: Long = 0
        if(selection != null && selection.nonEmpty) {
          selection.foreach(totalValue += _.value)
        }
        return totalValue
    }

    def printTransactionReport(tx: Transaction) {
        println("To spend " + tx.target + " in " + tx.block + ", " + name + " selected " + tx.inputSet.size + " inputs, with a total value of " + tx.inputsValue + " satoshi. The change was " + tx.change + " and the fee was " + tx.fee + ". The wallet now has " + utxoPool.size + " utxo, worth " + getWalletTotal() + " satoshi. It took " + tx.duration + " ms to calculate.")
    }

    def printStatusHeader() {
        println("one.murch.bitcoin.coinselection.Wallet Type;final value;mean #UTXO;final #UTXO;#received;#spent;"
            + "#changes created;smallest change;biggest change;mean change;stDev of change;in transit ratio;"
            + "total fees;average fees;fees to spend remaining UTXO;total cost;"
            + "smallest input set;biggest input set;mean size of input set;stdev of input set size")
    }

    def printWalletStatusCSV() {
        val meanUtxo = utxoSetSizes.reduce(_ + _) / (utxoSetSizes.size).toDouble

        var mainInfo = name + ";" + getWalletTotal() + ";" + meanUtxo + ";" + utxoPool.size + ";" + countReceivedUtxo + ";" + countSpentUtxo + ";"
        var changeInfo = ""
        var feeInfo = ""
        var inputSetInfo = ""

        if (changesCreated.length > 0) {
            val meanChange: Double = changesCreated.reduce(_ + _) / (changesCreated.length).toDouble
            val biggestChange: Long = changesCreated.max
            val smallestChange: Long = changesCreated.min
            val deviations = changesCreated.map(change => (change - meanChange) * (change - meanChange))
            val stdDev: Double = math.sqrt(deviations.reduce(_ + _) / deviations.length)
            val meanInTransit: Double = inTransitRatio.reduce(_ + _) / (inTransitRatio.length)
            changeInfo = changesCreated.length + ";" + smallestChange + ";" + biggestChange + ";" + meanChange + ";" + stdDev + ";" + meanInTransit + ";"
        } else {
            changeInfo = 0 + ";" + 0 + ";" + 0 + ";" + 0.00 + ";" + 0.00 + ";" + 0.00 + ";"
        }

        var totalFees: Long = 0
        pastTransactions.foreach(totalFees += _.fee)
        val averageFees = totalFees / (pastTransactions.size).toDouble
        val costToEmpty = utxoPool.size * WalletConstants.BYTES_PER_INPUT * WalletConstants.FEE_PER_KILOBYTE / 1000
        val totalCost = totalFees + costToEmpty
        feeInfo = totalFees + ";" + averageFees + ";" + costToEmpty + ";" + totalCost + ";"

        if (inputSetSizes.length > 0) {
            val meanInputSetSize: Double = inputSetSizes.reduce(_ + _).toDouble / inputSetSizes.length
            val biggestInputSet: Long = inputSetSizes.max
            val smallestInputSet: Long = inputSetSizes.min
            val inputDeviations = inputSetSizes.map(inputSetSize => (inputSetSize - meanInputSetSize) * (inputSetSize - meanInputSetSize))
            val inputStdDev: Double = math.sqrt(inputDeviations.reduce(_ + _) / inputDeviations.length)
            inputSetInfo = smallestInputSet + ";" + biggestInputSet + ";" + meanInputSetSize + ";" + inputStdDev + ";"
        } else {
            inputSetInfo = 0 + ";" + 0 + ";" + 0 + ";" + 0 + ";"
        }
        println(mainInfo + changeInfo + feeInfo + inputSetInfo)
    }

    def dumpUtxoSet() {
        println("UTXO Set of " + name + ":")
        println(utxoPool)
        println("End of UTXO Set of " + name)
    }

    def printWalletStatus() {
        println(name + " finally has " + getWalletTotal() + " satoshi, in " + utxoPool.size + " UTXO, by receiving " + countReceivedUtxo + " UTXO and spending " + countSpentUtxo + ".")
        if (changesCreated.length > 0) {
            val meanChange: Double = changesCreated.reduce(_ + _).toDouble / changesCreated.length
            val biggestChange: Long = changesCreated.max
            val smallestChange: Long = changesCreated.min
            val deviations = changesCreated.map(change => (change - meanChange) * (change - meanChange))
            val stdDev: Double = math.sqrt(deviations.reduce(_ + _) / deviations.length)
            println(name + " created " + changesCreated.length + " changes, the smallest was " + smallestChange + ", the biggest was " + biggestChange + ". The mean change created was " + meanChange + " with a stdDev of " + stdDev + ".")
        } else {
            println(name + " didn't create any changes.")
        }
        if (inputSetSizes.length > 0) {
            val meanInputSetSize: Double = inputSetSizes.reduce(_ + _).toDouble / inputSetSizes.length
            val biggestInputSet: Long = inputSetSizes.max
            val smallestInputSet: Long = inputSetSizes.min
            val inputDeviations = inputSetSizes.map(inputSetSize => (inputSetSize - meanInputSetSize) * (inputSetSize - meanInputSetSize))
            val inputStdDev: Double = math.sqrt(inputDeviations.reduce(_ + _) / inputDeviations.length)
            println(name + " spent " + countSpentUtxo + " utxo, the smallest input set having " + smallestInputSet + " inputs, the biggest set having " + biggestInputSet + ". The mean set size was " + meanInputSetSize + " with a stdDev of " + inputStdDev + ".")
        } else {
            println(name + " didn't use any inputs.")
        }
    }
}
