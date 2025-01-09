# Multi-signature wallet #

Multisig wallet, aka shared wallet, allows several participants to jointly control the wallet’s funds.
The control is exercised over the spending, delegation and minting/burning of assets and is regulated by native scripts
which specify what is the required set of witnesses and time conditions to be obeyed to
make a transaction valid upon managing the shared resources.

## Contents ##
1. [Introduction](#introduction)
2. [Wallet initialization](#wallet-initialization)
3. [Staking and spending are ortogonal](#staking-and-spending-are-ortogonal)


### Introduction ###

Typically the native script can enforce that all or some parties must agree, i.e. sign a given transaction,
in order to make the transaction valid, and hence spend from the wallet. For example, three out of four parties, or any party.
The transaction could be also required to be sent before or after some point in time in order to be valid.
As rules can be embedded inside themselves it is possible, for example, to design a rule engaging three parties, requiring two witnesses,
but always including a specific party, and enforcing the submitting of the transaction not later than some point in time.
The similar or different regulation could pertain to staking. As there is a separation of the control over the movements of funds and the rights (and obligations)
in the PoS protocol powering the Cardano that are associated with those funds the native scripts regulating staking and spending could be different.

### Wallet Initialization ###

The multisig wallet needs intermediation during the construction and making a transaction. Intermediation vehicle, eg. coordination server,
is needed to firstly agree on spending/staking ruling and to facilitate sharing cosigners identifiers.
In case of making a transaction the intermediation vehicle is needed to smooth the exchange of a partially signed transaction
to be handed over to another party to sign until a fully-signed transaction is achieved.

Figs. 1a-c introduce and visualize the construction phase.

![image](./multisig-figures/fig1a.svg)

Fig 1a. Wallet initialization: Spending/staking ruling and cosigner structure is decided.


![image](./multisig-figures/fig1b.svg)

Fig 1b. Wallet initialization: Cosigners share their extended account public keys.

![image](./multisig-figures/fig1c.svg)

Fig 1c. Wallet initiation. Each cosigner takes their other cosigner’s accXPub from the coordination server. Having spending/staking rules and all cosigners accXPubs one can create a multisig wallet. Each cosigner can search a balance and restore the wallet independently.

Parties upon multisig wallet creation agree on the script template regulating spending and optionally separate staking script template.
So they need to communicate this between parties which can be intermediated via a coordination server or by other off-chain means (Fig. 1a).
Template describes how scripts are going to be constructed. The script is formed in such a way that each cosigner tag is replaced with
the public verification keys belonging to cosigners. The script is then hashed and is a part of the address that is engaged in transactions.
Each party has its secret key (which is typically in the form of a mnemonic) that allows derivation of private and public keys
as specified by a derivation path. Private keys allow signing transactions in which the corresponding public keys are engaged as
a part of a script that give rise to spending credentials that build addresses. The secret key also allows derivation of an extended account public key
(and of course its private dual) that each party shares with other parties. Without full account public key exchange multisig wallet is in incomplete state
and cannot participate in transactions and recognize its resources. The collection of missing account public keys is performed off-chain (Fig. 1b).
When a given party collects all account public keys from each party the wallet is complete and ready to operate.
It means each party can recreate each party’s public keys and thus construct the script and its hash which acts as a credential.
Only the full collection of the keys allows each party to derive scripts that build the addresses that are commonly recognized amidst parties
and which participate in transactions. This means each cosigner needs to retrieve all missing account public keys (Fig 1c).
Script hashes act as credentials in addresses and can regulate separately spending and delegation.
When a spending transaction is to be submitted and accepted by blockchain the participants must collect all the required witnesses
as specified by the spending script template. In the situation presented in  Figs. 1 it means witnesses from both cosigner1 and cosigner2 are mandatory
to be provided to have a fully signed spending transaction.
For example, if we want to send some funds to another wallet, and the spending template requires two out of three witnesses,
plus transaction submission not later than slot 100, then it means any two parties must sign a given transaction and
submit the fully-signed transaction to the node at most at slot 100.
It entails one party to construct a transaction and sign it, then hand it over off-chain (for example via a coordination server)
to any other party for its signing. After the first sign we have a partially-signed transaction and only after adding
another witness on top we have a fully-signed transaction that, if submitted before slot 100, is expected to be accepted by cardano-node.
After sending the transaction each party separately is expected to detect the change of the balance of the shared wallet if the transaction is successful.

### Staking and spending are ortogonal ###

Spending and staking is orthogonal to each other and could be regulated by different rules.
The example presented in Fig 2 and Figs 3 show the case.

![image](./multisig-figures/fig2.svg)

Fig 2. Spending transaction. Due to spending ruling cosigner1 does not need a consent from cosigner2 to spend funds from a multisig wallet.
Hence, cosigner1 constructs, signs and submits the transaction that spends without coordination server intermediation.
Cosigner2 does not regulate spending and is irrelevant in this regard, but can follow cosigner1 action by inspecting a balance of the shared wallet.

![image](./multisig-figures/fig3a.svg)

Fig 3a. Staking transaction. Cosigner2 wants to stake funds to a specified pool.
Cosigner2 hands over the partially signed transaction to the coordination server.
Cosigner2 cannot realize this action without a consent from cosigner1 as determined by staking ruling.
Signing from both cosigner1 and cosigner2 is mandatory to move forward with staking.

![image](./multisig-figures/fig3b.svg)

Fig 3b. Staking transaction. The consent from cosigner1 is required to move forward with a staking transaction.
Cosigner1 uses the coordination server to download a partially signed transaction, inspects it, and if everything is valid he inscribes its witness
and submits it to the node. Due to time constraints present in the staking ruling he needs to realize the submission
in a time period defined by (s1, s2). After staking action is realized, both cosigners see rewards and
can initiate a withdrawal transaction that is regulated by staking ruling.
Spending of the rewards is regulated by spending ruling meaning cosigner1 can do it without consent from cosigners2.

The detailed draft specification of the multi-signature HD wallet can be found [here][ref1854]

[ref1854]: https://github.com/cardano-foundation/CIPs/tree/master/CIP-1854.
