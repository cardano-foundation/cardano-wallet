# About Transactions Lifecycle

## Transaction states

![State Diagram](./controllers_brief.svg)<img src="state_diagram.svg">

### About `forget`

Importantly, a transaction, when sent, cannot be cancelled. One can only
request forgetting about it in order to try spending (concurrently) the same
UTxO in another transaction. But, the transaction may still show up later in a
block and therefore, appear in the wallet.

> About `discover`

Discovering a transaction happens regardless of a transaction being present
or not as `pending`. Actually, only outgoing transactions are going through 
the `pending` state. Incoming ones or, outgoing ones that have been forgotten
may be discovered directly in blocks.

## Submitting a transaction

![Sequence Diagram](./controllers_brief.svg)<img src="sequence_diagram.svg">
