# Specification: DRep voting

This document specifies those aspects of the "Transactions New > Construct" HTTP endpoint that relate to DRep voting in the Voltaire era.

## Voting during delegation and delegate with abstain

The Voltaire voting system interacts with stake pool delegation. The following options are available:
1. Delegate to a stake pool without participating in the voting system.
2. Delegate to a stake pool while also participating in the voting system.

After a grace period (6–12 weeks), it will not be possible to withdraw staking rewards under option 1. (The rewards will still be accumulated, and are redeemable again once the user switches to option 2.)
In order to allow passive participation in the voting system, as part of option 2, there exists the possibility to delegate with `Abstain` which translates always into voting to abstain from any particular decision. While very passive, the spirit of voting system participation still requires that the user actively opts for the voting preference. The other two options include: passing no confidence vote and voting for a particular representative.

The "Transactions New > Construct" HTTP endpoint allows delegation and withdrawals.

Specifically:

1. Creation of a withdrawal transaction in the case of delegation without participating in the voting.

    Example `POST` data for the endpoint:

    ```
    {
    ...
      "withdrawal": "self"
    ...
    }
    ```
    When in Voltaire era user should expect 403 error with response: "Withdrawals are not possible without participating in the voting. Please re-join the stake pool. If `voting` field is missing abstaining from voting is set."

2. Re-joining with explicit abstain from voting (this is default when `vote` field is missing).

    ```
    {
      "delegations":
           [
                { "join":
                  { "pool": "pool1wqaz0q0zhtxlgn0ewssevn2mrtm30fgh2g7hr7z9rj5856457mm"
                  , "stake_key_index": "0H"
                  }
                }
           ],
      "vote": "abstain"
    }
    ```

3. Re-joining with no confidence vote.

    ```
    {
      "delegations":
           [
                { "join":
                  { "pool": "pool1wqaz0q0zhtxlgn0ewssevn2mrtm30fgh2g7hr7z9rj5856457mm"
                  , "stake_key_index": "0H"
                  }
                }
           ],
      "vote": "no_confidence"
    }
    ```

4. Re-joining with a vote casted to a representative by specifying its registered key hash or script hash

    ```
    {
      "delegations":
           [
                { "join":
                  { "pool": "pool1wqaz0q0zhtxlgn0ewssevn2mrtm30fgh2g7hr7z9rj5856457mm"
                  , "stake_key_index": "0H"
                  }
                }
           ],
      "vote": "drep1jklcrnsdzqp65wjgrg55sy9723kw09mlgvlcp65wjgrg55sy9723mm"
    }
    ```
