# Data model

Numbering continues `specs/5350-redelegate-previous-drep/data-model.md` (D1,
D2).

- **D3 - Ledger vote delegation**: `VoteDelegation` in
  `Cardano.Wallet.Primitive.Types.DRep`.
  - `VoteNotDelegated`: the ledger holds no vote delegation for the account,
    including an account whose stake key is not registered.
  - `VoteDelegatedTo DRep`: a delegation to a DRep credential, to `Abstain` or
    to `No Confidence`.
  - At a consumer, `Maybe VoteDelegation`: `Nothing` means unread, an era
    before Conway, or, from `readCachedVoteDelegation`, a wallet without a
    reward account.
- **D4 - Reward account state**: `RewardAccountState` in
  `Cardano.Wallet.Network.LocalStateQuery.RewardAccount`, the value of the
  reward-account cache, one entry per requested account, from one read.
  - `rewardBalance :: Coin`: zero for a requested account absent from the
    balance answer, as `getCachedRewardAccountBalance` reports today.
  - `voteDelegation :: Maybe VoteDelegation`: `Just VoteNotDelegated` for a
    requested account absent from the delegatee answer; `Nothing` before
    Conway.

  State invariant (INV-5466-COHERENT): in Conway and later, every entry holds
  both values from the same read, so a cached balance implies a cached vote
  delegation for that account.
- **D5 - API value**: the field `ledger_vote` of `ApiWalletDelegation`,
  `Maybe ApiWalletLedgerVote`, absent when the consumer value is `Nothing`.
  - `ApiLedgerVoteDelegated (ApiT DRep)` encodes as
    `{"status": "delegated", "voting": <DRep>}`.
  - `ApiLedgerVoteNotDelegated` encodes as `{"status": "not_delegated"}`.
  - Decoding rejects `delegated` without `voting` and `not_delegated` with
    `voting`; the type has no value for either.
- **D6 - Formal ledger vote observation**: in the Agda model,
  `LedgerVote = Unread | NoDelegation | DelegatedTo DRep`,
  `Phase = Bootstrap | PostBootstrap` and
  `CredKind = KeyHashCred | ScriptHashCred`.
  - `Unread` mirrors `Nothing`, `NoDelegation` mirrors
    `Just VoteNotDelegated` and `DelegatedTo d` mirrors
    `Just (VoteDelegatedTo d)`.
  - `Bootstrap` mirrors `hardforkConwayBootstrapPhase` of the protocol
    version; `KeyHashCred` and `ScriptHashCred` mirror `FromKeyHash` and
    `FromScriptHash` of `RewardAccount`.
  - The history's vote is `Maybe DRep`, the DRep of the latest delegation
    status, which equals `statusDRep` of D1's effective status.
