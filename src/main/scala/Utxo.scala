

class Utxo(val id: Int = -1, val value: Long = Long.MaxValue, val block: Int = 0) {

    override def toString: String = {
        return "(" + id + ", " + value + ", " + block + ")"
    }

    def getCoinAge(currentBlock: Int): Long = {
        return value * (currentBlock - block)
    }
}
