# Transaction 

In a UTxO-based blockchain, a **Transaction** is a binding between **inputs** and **outputs**. 


```
                   input #1  >---*         *---> output #1 
                                 \        /
                   input #2  >---*--------* 
                                 /        \
                   input #3  >---*         *---> output #2
```

<p align="center">
  <small><strong>Fig. 1</strong> a Transaction</small>
</p>

In a standard payment, outputs are a combination of:

- A value
- A reference (a.k.a address, a "proof" of ownership telling _who_ owns the output).

```
                   input #1  >---*         *---> (123, DdzFFzCqr...) 
                                 \        /
                   input #2  >---*--------* 
                                 /        \
                   input #3  >---*         *---> (456, hswdEoQCp...) 
```

<p align="center">
  <small><strong>Fig. 2</strong> a Payment</small>
</p>
 
> NOTE _on address encoding_:
>
> We usually represent addresses as encoded text strings. An address has a structure
> and a binary representation that is defined by the underlying blockchain. Yet, since 
> they are often used in user-facing interfaces, addresses are usually encoded in a 
> human-friendly format to be easily shared between users.

An _address_ **does not** uniquely identify an output. As a matter of fact, multiple
transactions could send funds to a same output address! We can however uniquely identify
an output by:

- Its host transaction id
- Its index within that transaction

This combination is also called an **input**. Said differently, inputs are
outputs of previous transactions. 


```
                 *---------------- tx#42 ----------------------*
                 |                                             |
 (tx#14, ix#2) >-----------------*       *--> (123, DdzFFqr...)--- (tx#42, ix#0)
                 |               \      /                      |
 (tx#41, ix#0) >-----------------*-----*                       |
                 |               /      \                      |
 (tx#04, ix#0) >----------------*        *--> (456, hswdQCp...)--- (tx#42, ix#1)
                 |                                             |
                 *---------------------------------------------*

```

<p align="center">
  <small><strong>Fig. 3</strong> a Payment chain</small>
</p>


Therefore, new transactions spend outputs of previous transactions, and produce
new outputs that can be consumed by future transactions. An unspent transaction
output (i.e. not used as an input of any transaction) is called a **UTxO** (as
in **U**nspent **Tx** **O**utput) and represents an amount of money owned by a 
participant. 




# FAQ

<details>
  <summary>Where does the money come from? How do I make the first transaction?</summary>

  When bootstrapping a blockchain, some initial funds can be distributed among
  an initial set of stakeholders. This is usually the result of an **I**nitial
  **C**oin **O**ffering or, an agreement between multiple parties. In practice
  it means that, the genesis block of a blockchain may already contain some
  UTxOs belonging to various stakeholders.

  Beside, core nodes running the protocol and producing blocks are allowed to
  insert in every block minted (resp. mined) called a **coinbase** transaction.
  This transaction has no inputs but follows specific rules fixed by the
  protocol and is used as an incentive to encourage participants to engage in
  the protocol.
</details>

<br/>

<details>
  <summary>What is the difference between an address and a public key?</summary>

  In a very simple system that would only support payment transactions, public key
  could be substituted for addresses. In practice, addresses are meant to hold some
  extra pieces of information that are useful for other aspects of the protocol.
  For instance, in Cardano in the Shelley era, addresses may also contain:

  - A network discriminant tag, to distinguish addresses between a testNet and the
    MainNet and avoid unfortunate mistakes.

  - A stake reference to take part in delegation.

  Addresses may also be used to trigger smart contracts, in which case, they'll
  refer to a particular script rather than a public key. 

  In a nutshell, a public key is a piece of information that enables a stakeholder to
  prove one owns a particular UTxO. Whereas an address is a data-structure which contain various
  pieces of information, for example, a (reference to a) public key. 
</details>

<br/>

<details>
  <summary>What are Cardano addresses made of?</summary>

  See:

  - [About Address Format - Byron](https://github.com/input-output-hk/cardano-wallet/wiki/About-Address-Format---Byron)
  - [About Address Format - Shelley](https://github.com/input-output-hk/implementation-decisions/blob/master/text/0001-address.md)
</details>
