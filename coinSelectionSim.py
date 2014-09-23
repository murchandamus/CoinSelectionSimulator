from random import shuffle
from random import randrange
from random import choice

debug = False
info = False
results = True

class Wallet:
    """Manages the unspent outputs, in Satoshi. Starts with nine utxo of a total of 11.11111111 BTC."""
    def __init__(self, name, prune, utxo, minInputs):
        self.prune = prune
        self.name = name 
        self.utxo = []
        if(debug):print self.name, 'initialising with UnspentInputs'
        self.utxo.extend(utxo)
        if(debug):print self.name, 'initialising with UnspentInputs finished'
        self.totalUTXOspent = 0
        self.totalUTXOreceived = 0
        self.minInputs = minInputs

    def setUtxo(self, utxo):
        self.utxo = utxo

    def printWalletInfo(self):
        if(results):print self.name, 'has', len(self.utxo), 'utxo with a total value of ', sum(self.utxo), 'satoshi.'
        if(results):print self.name, 'received', self.totalUTXOreceived, 'transactions and spent', self.totalUTXOspent, 'UTXO.'
        self.utxo.sort()
        if(debug):print self.name, 'has these UTXO', self.utxo

    def getWalletTotal(self):
        total = sum(self.utxo)
        return total
    
    def receive(self, inc_utxo):
        assert(inc_utxo >=0 )
        if(inc_utxo >0):
            self.utxo.append(inc_utxo)
            self.totalUTXOreceived = self.totalUTXOreceived+1
            if(info):print self.name , 'received' , inc_utxo , 'satoshi. It now has' , len(self.utxo) , 'UTXO, with total value of' , sum(self.utxo) , 'satoshi.'
            self.utxo.sort()

    def spend(self, target):
        assert(target <= self.getWalletTotal());

        if(info):print self.name , 'will try to spend' , target, 'satoshi. It has' , len(self.utxo) , 'UTXO, with total value of' , sum(self.utxo) , 'satoshi.'
        selectedCoins = []
        selectionFinished = False
        if(self.utxo.count(target) > 0):
            # Case1: target is in utxo
            selectedCoins.append(target)
            selectionFinished = True
            if(debug):print self.name, "matched target as single UTXO."

        if(selectionFinished == False):
            # Case2: All utxo smaller than target match target in sum.
            totalValueOfSmaller = 0
            for unspent in self.utxo:
                if(unspent < target):
                    selectedCoins.append(unspent)
                    totalValueOfSmaller = totalValueOfSmaller + unspent
            if (totalValueOfSmaller == target):
                selectionFinished = True
                if(debug):print self.name, "matched target by combining all UTXO smaller than target."
            elif (totalValueOfSmaller < target):
                # Case3: Sum of UTXO smaller than target is not sufficient: Take smallest sufficient UTXO.
                selectedCoins = []
                smallestSufficient = self.getWalletTotal()+1; # Upper boundary

                for unspent in self.utxo:
                    if (unspent > target and unspent < smallestSufficient):
                        smallestSufficient = unspent
                selectedCoins.append(smallestSufficient)
                selectionFinished = True
                if(debug):print self.name, "didn't contain a combination of UTXO to supersede target. Defaulting to smallest sufficient UTXO."

        if(selectionFinished == False):
            # Final Case: 1000 tries of combining random inputs
            bestTotal = self.getWalletTotal()+1 #upper Boundary
            selectedCoins = []

            i = 0
            utxoTemp = []
            utxoTemp.extend(self.utxo) #create a temporary list of UTXO, so that each will only be used once
            while (selectionFinished == False and i < 1000):
                i = i+1
                shuffle(utxoTemp) #works in place

                total = 0;
                j = 0;
                currentSelection = []
                while (total < target or len(currentSelection) < self.minInputs) and j < len(utxoTemp):
                    total = total + utxoTemp[j]
                    currentSelection.append(utxoTemp[j])
                    j = j+1

                if(total == target):
                    selectedCoins = currentSelection
                    selectionFinished = True
                    if(debug):print self.name , 'found direct match. Random combination stopped after' , i+1, 'tries.'
                elif(total < bestTotal):
                    bestTotal = total
                    selectedCoins = currentSelection
                    if(len(selectedCoins) > 10):
                        if(debug):print self.name , 'found better combination:', len(selectedCoins), 'inputs with total', bestTotal, '.'
                    else:
                        if(debug):print self.name , 'found better combination:', selectedCoins, 'with total', bestTotal, '.'

        if(self.prune):
            excess = sum(selectedCoins) - target
            minimal = (0 == excess) 
            while not minimal:
                minimal = True
                for unspent in selectedCoins:
                    if (unspent <= excess and len(selectedCoins > minInputs):
                        selectedCoins.remove(unspent)
                        excess = excess - unspent
                        minimal = False

        # Is there a single UTXO that has smaller change?
        smallestSufficient = sum(selectedCoins); # Upper boundary

        for unspent in self.utxo:
            if (unspent > target and unspent < smallestSufficient):
                smallestSufficient = unspent
                selectedCoins = [unspent]

        # finally remove the selected coins and create new utxo with change
        change = sum(selectedCoins) - target
        for spent in selectedCoins:
            self.utxo.remove(spent)
        self.totalUTXOspent = self.totalUTXOspent+len(selectedCoins) 

        if(change > 0):
            self.utxo.append(change)
        if(info):print 'To spend ' , target , ' ' , self.name , ' selected ' , len(selectedCoins) , ' inputs, with a total value of ' , sum(selectedCoins) , ' satoshi. The change was ' , change , '. The wallet now has ' , len(self.utxo) , ' utxo, worth ', sum(self.utxo) , ' satoshi.'

def simulate(utxo, operations):
    regWallet = Wallet('Regular Wallet', False, utxo,1)
    pruningWallet = Wallet('Pruning Wallet', True, utxo,1)
    pruningMin2Wallet = Wallet('Pruning Wallet', True, utxo,2)
    pruningMin3Wallet = Wallet('Pruning Wallet', True, utxo,3)

    for op in operations:
        if(op > 0):
            regWallet.receive(op)
            pruningWallet.receive(op)
            pruningMin2Wallet.receive(op) 
            pruningMin3Wallet.receive(op) 
        elif(op < 0):
            regWallet.spend(abs(op))
            pruningWallet.spend(abs(op))
            pruningMin2Wallet.spend(abs(op))
            pruningMin3Wallet.spend(abs(op))
    if(results):
        print 'FINAL RESULTS'
        if(debug):print 'Initial UTXO were:', utxo
        if(debug):print 'Operations executed were:',operations
        regWallet.printWalletInfo()
        pruningWallet.printWalletInfo()
        pruningMin2Wallet.printWalletInfo()
        pruningMin3Wallet.printWalletInfo()

print '------- First trial ---------'
firstOperations = [1, -1, 1, 1, -2, 1,1,1, 10, -11]

simulate([],firstOperations)

print '------- Second trial ---------'
## 1000 random UTXO, 1000 random operations
secondUtxo = []
for i in range(1,1000):
    secondUtxo.append(randrange(1,100000000))

secondOps = []
secondUtxoTotal = sum(secondUtxo)
secondCurrentTotal = secondUtxoTotal
for i in range(1,1000):
    op = randrange(-1*secondCurrentTotal,100000000)
    secondCurrentTotal = secondCurrentTotal + op
    secondOps.append(op)

simulate(secondUtxo,secondOps)

print '------- Third trial ---------'
# 10000 single satoshi UTXO, 10000 random UTXO, 10000 random operations
thirdUtxo = []
for i in range(1,10000):
    thirdUtxo.append(1)
    thirdUtxo.append(randrange(1,100000000))

thirdOps = []
thirdCurrentTotal = sum(thirdUtxo)
for i in range(1,10000):
    op = randrange(-1*thirdCurrentTotal,100000000)
    thirdCurrentTotal = thirdCurrentTotal + op
    thirdOps.append(op)

simulate(thirdUtxo, thirdOps)

print '------- Fourth trial: "regular usage" ---------'
# 1 UTXO of 1BTC, 10000 random operations between $0.01 and $100
fourthUtxo = []
fourthUtxo.append(100000000)

fourthOps = []
fourthCurrentTotal = sum(fourthUtxo)
for i in range(1,10000):
    amount = randrange(1,10000) #US Dollar cent
    direction = choice([-1,1])
    op = direction*amount
    fourthCurrentTotal = fourthCurrentTotal + op
    fourthOps.append(op)

simulate(fourthUtxo, fourthOps)

print '------- Fifth trial: "miner" --------------'
# 1 UTXO of 1BTC, 10000 UTXO between 1 and 1000 Satoshi, 10000 random operations between $0.01 and $100
fifthUtxo = []
fifthUtxo.append(100000000)

for i in range(1,10000):
    fifthUtxo.append(randrange(1,1000))

fifthOps = []
fifthCurrentTotal = sum(fifthUtxo)
for i in range(1,10000):
    amount = randrange(1,10000) #US Dollar cent
    direction = choice([-1,1])
    op = direction*amount
    fifthCurrentTotal = fifthCurrentTotal +op
    fifthOps.append(op)

simulate(fifthUtxo, fifthOps)
