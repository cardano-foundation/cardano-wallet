# Forewords

This wiki page is a "blackboard" where anyone in the team can write ideas or suggestions about various things on the team, the project, the code or the development process. Give as many details as you can so that, during release meeting, we can discuss what's on the board and improve. 

# Ideas

## Hide CLI passphrase

### Context 

During wallet creation using `cardano-wallet` comand line, CLI shows entered mnemonic and passphrase on the screen #96 . This is not best solution as mnemonic and passhrase are sensitive data and user should be careful about not revealing them.

### Decision 

We have discussed internally and decided to have a similar approach as Daedalus UI does. For passphrase we have decided to hide the user input and replace it with `*`. Because passphrase is 10+ chars long there is high chance of user making a mistake. To be sure user didn't make a typing mistake we will ask user to retype the passphrase. If retyped passphrase matches originally typed passphrase we proceed with the process of creating a wallet. Example:
```shell
Passphrase: **********
Retype passphrase: *********
```

Mnemonics are 15+ words and it would be hard/frustrating for user to enter such a long stream without a mistake. We decided not to proceed the same approach for mnemonics and just follow the same strategy as Daedalus UI have. Mnemonics are entered in plain text so no change is necessary there.

## Better Restoration Stress Benchmark

Existing chain data doesn't necessarily include "extreme" cases that might occur in the future

* Make data generators which set up transactions for wallets of various sizes.
* Use the [weigh](https://www.fpcomplete.com/blog/2016/05/weigh-package) package to measure and display the GHC heap usage of test scenarios.

- [ ] Figure out a way of generating semi-realistic transactions in blocks
- [ ] Use a mock network layer to feed generated blocks to wallet layer
- [ ] Set up a test case which checks heap usage after applying a certain number of transactions in a certain number of blocks.
- [ ] Also measure how long it took to apply those blocks/transactions.