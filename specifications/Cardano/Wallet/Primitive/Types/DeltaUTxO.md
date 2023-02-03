# Delta encoding for UTxO

… and its storage in database tables.

## UTxO

The type `UTxO` describes a collection of transaction outputs. This collection is a finite map from the unique identifier of each output (`TxIn`) to the coin and asset values contained in said output (`TxOut`):

```hs
newtype UTxO = UTxO { unUTxO :: Map TxIn TxOut }
```

## DeltaUTxO

We introduce a type `DeltaUTxO` that represents changes to a `UTxO`:

```hs
data DeltaUTxO = DeltaUTxO
    { excluded :: !(Set TxIn)
    , received :: !UTxO
    }
```

We apply such a change to a `UTxO` by first removing the output identifiers in the `excluded` set, i.e. we "spend" the outputs, and then we add the outputs in `received` to the `UTxO`:

```hs
instance Delta DeltaUTxO where
    type Base DeltaUTxO = UTxO
    du `apply` u =
        (u `excluding` excluded du) <> received du

excluding :: UTxO -> Set TxIn -> UTxO
excluding (UTxO utxo) =
    UTxO . Data.Map.withoutKeys utxo
```

In other words, `DeltaUTxO` represents a spending operation followed by an addition of funds.

Throughout this discussion, we assume that `TxIn` are unique and that there is no double spending, i.e. each `TxIn` may appear at most once in the `received` and at most once in the `excluded` fields.

## Database schema

We want to persist values of type `DeltaUTxO` to disk.

We have chosen an SQLite database as our file format, so we need to encode a value of type `DeltaUTxO` in database tables. We have a spectrum of options:

* We can serialize `DeltaUTxO` to a bytestring and store this string in a single row of a table. However, this would make the database hard to inspect by human debuggers.
* We can split `DeltaUTxO` into its constituents and create separate tables for each. The tables are easier to inspect, but we need to write code that combines them again.
* We can do something in between.

Regardless of which particular encoding we choose, we can *decouple* this choice from the rest of the database by referring to `DeltaUTxO` values via a *primary key*; this allows other tables to refer to `DeltaUTxO` values without imposing parts of their schema. Another viewpoint is that we want to store multiple `DeltaUTxO` values in the same table, and in order to do that, we have to distinguish them by their primary key.

In Haskell, we can define the type for the primary key as

```hs
newtype KeyDeltaUTxO = KeyDeltaUTxO B8.ByteString
```

Here is an example schema for the database tables that leans towards the "separate tables for everything"-end of the spectrum:

```
DeltaUTxOExcluded
    KeyDeltaUTxO        sql=key
    TxId                sql=input_tx_id
    Word32              sql=input_index

    Primary 'key'

DeltaUTxOReceivedCoin
    KeyDeltaUTxO        sql=key

    TxId                sql=input_tx_id
    Word32              sql=input_index
    W.Address           sql=output_address
    W.Coin              sql=output_coin

    Primary 'key', 'input_tx_id', 'input_index'

DeltaUTxOReceivedToken
    KeyDeltaUTxO        sql=key

    TxId                sql=input_tx_id
    Word32              sql=input_index

    W.TokenPolicyId     sql=token_policy_id
    W.TokenName         sql=token_name
    W.TokenQuantity     sql=token_quantity

    Primary 'key', 'input_tx_id', 'input_index'
```

The fields `excluding` and `receiving` are split into different tables. Also, non-Ada token values in a `TxOut` are stored in a separate table. (Here, the dependence of the primary key for the `DeltaUTxOReceivedToken` on `TxId` is an example where the context, here the mapping `Map TxIn TxOut`, imposes upon the table schema of a data type, here `TxOut`.)

## Store

We use `Store` to package an encoding as database tables with primary key. Reminder: `Store m da` is a storage facility for Haskell values of type `a` with delta encoding `da`. The main idea of using `Store` is that it allows us to assign a Haskell type `a` to the "stuff" that is stored in the datbase tables.

For a given key `KeyDeltaUTxO`, the following function returns a storage facility which stores a single value at this key by manipulating the entries in the database table:

```hs
mkStoreDeltaUTxO
    :: forall m. m ~ SqlPersistT IO
    => KeyDeltaUTxO
    -> Store m (Replace DeltaUTxO)
```

Note that the type argument to `Store` is a "delta encoding of a delta encoding" — we want to store a value of type `DeltaUTxO` in the table, so we have to use a `"Delta"DeltaUTxO`.

More generally, we can package the tables as a `Map` of `DeltaUTxO` values:

```hs
mkStoreDeltasUtxO
    :: forall m. m ~ SqlPersistT IO
    => Store m
        (DeltaMap KeyDeltaUTxO (Replace DeltaUTxO))
```

This composes well with *polymorphic types*, such as `Set a`. For example, if we want to store a container `Set DeltaUTxO` in the database, we can represent it as a pair

```hs
Set DeltaUTxO ≈ (Set KeyDeltaUTxO, Map KeyDeltaUTxO DeltaUTxO)
```

where the key replaces the polymorphic values, and the `Map` assigns the keys the values again.

Note that this approach does require us to do manual key management — we have to create and destroy `KeyDeltaUTxO` as we add and remove elements from the container.