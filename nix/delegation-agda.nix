# Typecheck the formal delegation model of issues #5350 and #5466 with a
# pinned Agda.
#
# `specifications/Cardano/Wallet/Delegation.agda` is library-free: it imports
# nothing, so this check needs no Agda library set, no agda2hs, and no
# installer fetched at CI time. The Agda version is pinned by the flake's
# locked `nixpkgs-unstable` input.
#
# The check is self-falsifying. After typechecking the model it applies one
# mutation per named law and requires Agda to reject each mutated model. A
# mutation whose anchor line has drifted fails the check instead of silently
# testing nothing, so the negative control cannot become vacuous.
#
# Every directory below is created fresh inside the build sandbox and nothing
# is ever deleted, and `AGDA_DIR` is a task-specific directory so that `HOME`
# is neither read nor repurposed.
{
  pkgs,
  model,
}:
pkgs.runCommand "delegation-agda"
  {
    nativeBuildInputs = [ pkgs.agda ];
    inherit model;
  }
  ''
    export AGDA_DIR="$NIX_BUILD_TOP/agda"
    mkdir -p "$AGDA_DIR"
    mkdir -p "$out"

    # The sandbox locale cannot encode the model's Unicode, so Agda would
    # abort while printing an error and leave only its first lines in each
    # mutant's log. Agda reads its output encoding from the locale.
    export LC_ALL=C.UTF-8

    relative=Cardano/Wallet/Delegation.agda

    # The model imports nothing, so no library needs to resolve. Each caller
    # passes its own fresh sandbox directory as the include root, so Agda
    # writes its interface files there and the copies stay independent.
    # (`--local-interfaces` was removed in Agda 2.8.0 and is not passed.)
    typecheck() {
      ( cd "$1" && agda --include-path=. "$relative" )
    }

    echo "+++ delegation-agda: typechecking $relative"
    mkdir -p model/Cardano/Wallet
    cp "$model" "model/$relative"
    typecheck model

    # Negative control: one law-breaking mutation per named law. Each mutation
    # must apply to exactly one line, and the exact same Agda command must
    # reject the result. Each mutant lives in its own fresh directory.
    mutate() {
      local law=$1 from=$2 to=$3
      local dir="mutant-$law"
      local anchors

      anchors=$(awk -v from="$from" '$0 == from { c++ } END { print c+0 }' \
        "model/$relative")
      if [ "$anchors" != 1 ]; then
        echo "delegation-agda: $law anchor matched $anchors lines, wanted 1" >&2
        echo "  anchor: $from" >&2
        exit 1
      fi

      mkdir -p "$dir/Cardano/Wallet"
      awk -v from="$from" -v to="$to" '$0 == from { print to; next } { print }' \
        "model/$relative" > "$dir/$relative"
      if cmp -s "model/$relative" "$dir/$relative"; then
        echo "delegation-agda: $law mutation did not change the model" >&2
        exit 1
      fi

      if typecheck "$dir" > "$out/$law.log" 2>&1; then
        echo "delegation-agda: $law mutation still typechecks" >&2
        exit 1
      fi
      echo "+++ delegation-agda: $law mutation rejected"
      cat "$out/$law.log"
    }

    mutate AGDA-5350-EMPTY \
      'effectiveDelegationStatus (MkDelegation a []) = a' \
      'effectiveDelegationStatus (MkDelegation a []) = Inactive'

    mutate AGDA-5350-LAST \
      'lastStatus s (t ∷ ts) = lastStatus t ts' \
      'lastStatus s (t ∷ ts) = s'

    mutate AGDA-5350-HISTORY \
      '    decideAgainst t (statusDRep (effectiveDelegationStatus d))' \
      '    decideAgainst t (statusDRep (activeStatus d))'

    mutate AGDA-5350-SAME \
      '  chooseVote True = SameVote' \
      '  chooseVote True = DifferentVote'

    mutate AGDA-5466-WDRL-LEDGER \
      'withdrawalDecision PostBootstrap KeyHashCred NoDelegation history = False' \
      'withdrawalDecision PostBootstrap KeyHashCred NoDelegation history = True'

    mutate AGDA-5466-WDRL-EXEMPT \
      'withdrawalDecision Bootstrap credKind ledgerVote history = True' \
      'withdrawalDecision Bootstrap credKind ledgerVote history = False'

    mutate AGDA-5466-WDRL-UNREAD \
      'withdrawalDecision PostBootstrap KeyHashCred Unread history = isJust history' \
      'withdrawalDecision PostBootstrap KeyHashCred Unread history = True'

    mutate AGDA-5466-VOTE-LEDGER \
      '  ledgerVoteDecision t (DelegatedTo d) dlg = decideAgainst t (Just d)' \
      '  ledgerVoteDecision t (DelegatedTo d) dlg = voteDecision t dlg'

    mutate AGDA-5466-VOTE-UNREAD \
      '  ledgerVoteDecision t Unread dlg = voteDecision t dlg' \
      '  ledgerVoteDecision t Unread dlg = DifferentVote'

    cp "model/$relative" "$out/Delegation.agda"
    agda --version > "$out/agda-version.txt"
    echo "delegation-agda: model typechecks, all 9 law mutations rejected"
  ''
