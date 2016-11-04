package test.scala

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfterEach
import main.scala.AbstractWallet
import main.scala.BreadWallet
import main.scala.CoreWallet
import main.scala.DoubleWallet
import main.scala.MyceliumWallet
import main.scala.PriorityWallet
import main.scala.RandomWallet
import main.scala.WalletConstants

abstract class WalletTestSuite extends FunSuite with BeforeAndAfterEach {
    var wallet: AbstractWallet = new CoreWallet("CoreWallet", Set(), 0, false)

    //Edge case checklist: Empty, non-empty, dust, greater than 32 bit
    test("getWalletTotal() on empty wallet") {
        assert(wallet.getWalletTotal() == (0).toLong)
    }

    test("getWalletTotal() after receiving one input") {
        wallet.receive(WalletConstants.CENT, 0, false)
        assert(wallet.getWalletTotal() == (WalletConstants.CENT).toLong)

    }

    test("Swipe wallet with single UTXO") {
        wallet.receive(WalletConstants.CENT, 0, false)
        wallet.spend(WalletConstants.CENT - WalletConstants.ONE_IN_ONE_OUT_TRANSACTION_SIZE * WalletConstants.FEE_PER_KILOBYTE / 1000, 1)
        assert(wallet.getWalletTotal() == 0)
    }

    test("Try spending everything") {
        wallet.receive(5 * WalletConstants.CENT, 0, false)
        wallet.spend(5 * WalletConstants.CENT, 1)
        assert(wallet.getWalletTotal() >= 0)
    }

    test("Make payment with small change") {
        wallet.receive(5 * WalletConstants.CENT, 0, false)
        wallet.spend(4 * WalletConstants.CENT, 1)
        assert(wallet.getWalletTotal() < WalletConstants.CENT)
        assert(wallet.getWalletTotal() > 0)
    }

    test("Try spending more than everything") {
        wallet.receive(5 * WalletConstants.CENT, 0, false)
        assertThrows[IllegalArgumentException] {
            wallet.spend(6 * WalletConstants.CENT, 1)
        }

    }
}

class MyceliumTestSuite extends WalletTestSuite {
    override def beforeEach() {
        wallet = new MyceliumWallet("MyceliumWallet", Set(), WalletConstants.FEE_PER_KILOBYTE, false)

    }
}

class BreadWalletTestSuite extends WalletTestSuite {
    override def beforeEach() {
        wallet = new BreadWallet("BreadWallet", Set(), WalletConstants.FEE_PER_KILOBYTE, false)
    }
}

class PriorityWalletTestSuite extends WalletTestSuite {
    override def beforeEach() {
        wallet = new PriorityWallet("PriorityWallet", Set(), WalletConstants.FEE_PER_KILOBYTE, false)
    }
}

class MoneroWalletTestSuite extends WalletTestSuite {
    override def beforeEach() {
        wallet = new RandomWallet("RandomWallet", Set(), WalletConstants.FEE_PER_KILOBYTE, false)
    }
}

class CoreWalletTestSuite extends WalletTestSuite {
    override def beforeEach() {
        wallet = new CoreWallet("CoreWallet", Set(), WalletConstants.FEE_PER_KILOBYTE, true)
    }
}

class DoubleWalletTestSuite extends WalletTestSuite {
    override def beforeEach() {
        wallet = new DoubleWallet("DoubleWallet", Set(), WalletConstants.FEE_PER_KILOBYTE, true)
    }
}
