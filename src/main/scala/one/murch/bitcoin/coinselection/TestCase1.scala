package one.murch.bitcoin.coinselection

/**
  * Created by murch on 31.12.16.
  */
object TestCase1 extends Scenario(Set(), ListBuffer(), "") {
    var paymentValues: List[Long] = List(1, -1, 1, 1, -2, 1, 1, 1, 10, -9, -4, 3, 3, 3, 3, 11, -10, -1, -8)

    var i = 1
    paymentValues.foreach {
        x =>
            operations += new Payment(i, x * WalletConstants.CENT, 0)
            i += 1
    }
}
