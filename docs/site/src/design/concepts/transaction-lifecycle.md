# Transaction Lifecycle

> This needs to be updated to include transaction resubmission by the wallet.

## States

```mermaid
stateDiagram
  [*] --> pending: request
  [*] --> in_ledger: discover
  pending --> in_ledger: discover
  pending --> [*]: forget
  in_ledger --> pending: rollback
```

### State transition: `forget`

Importantly, a transaction, when sent, cannot be cancelled. One can only
request forgetting about it in order to try spending (concurrently) the same
UTxO in another transaction. But, the transaction may still show up later in a
block and therefore, appear in the wallet.

### State transition: `discover`

Discovering a transaction happens regardless of a transaction being present
or not as `pending` . Actually, only outgoing transactions are going through
the `pending` state. Incoming ones or, outgoing ones that have been forgotten
may be discovered directly in blocks.

## Submission

```mermaid
sequenceDiagram
  participant Wallet Client
  participant Wallet Server
  participant Network

  Wallet Client ->>+ Wallet Server: POST payment request
  Wallet Server ->> Wallet Server: Select available coins
  Wallet Server ->> Wallet Server: Construct transaction
  Wallet Server ->> Wallet Server: Sign transaction
  Wallet Server -->> Wallet Client: 403 Forbidden
  Wallet Server ->>+ Network: Submit transaction

  Network ->> Network: Validate transaction structure
  Network -->> Wallet Server: (ERR) Malformed transaction
  Wallet Server -->> Wallet Client: 500 Internal Server Error

  Network ->>- Wallet Server: Accepted
  Wallet Server ->>- Wallet Client: 202 Accepted

  Network ->> Network: Broadcast transaction to peers
  loop Every block
      Network ->> Network: Insert or discard transaction(s)
      Network ->> Wallet Server: Yield new block
      Wallet Server ->> Wallet Server: Discover transaction(s)
  end
```
