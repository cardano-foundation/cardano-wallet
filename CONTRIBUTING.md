If you find a bug or you'd like to propose a feature, please feel free to raise an issue on our [issue tracker](https://github.com/cardano-foundation/cardano-wallet/issues).

> [!IMPORTANT]
>
> Starting April 1st, 2025, the Cardano Wallet is entering
> maintenance-only mode. The Cardano Foundation is committed to
> maintaining it for the foreseeable future by upgrading to new
> versions of the [cardano-node][], fixing bugs, improving quality and
> stability of both the code and server stability, plus providing
> general user support.

  [cardano-node]: https://github.com/IntersectMBO/cardano-node
> Contributions for new features are welcome and
> will be thoroughly reviewed, but no new features will be developed
> by the Cardano Foundation.

Notwithstanding the above statement, **Pull requests are welcome!**
and will be thoroughly and diligently reviewed and shepherded.

When creating a pull request, please make sure that your code adheres
to our [coding
standards](https://github.com/input-output-hk/adrestia/blob/master/docs/code/Coding-Standards.md).

**Formatting.** Haskell, Cabal and Nix files have canonical formatters
(`fourmolu`, `cabal-fmt`, `nixfmt`) pinned in the dev shell and
enforced by CI. Run `just fmt` before every push.

**No drive-by reformatting.** A style change and a semantic change
must not share a commit, and typically must not share a PR. Pull
requests that reformat files beyond what they meaningfully change —
or that switch formatters — will be rejected. See the [coding
standards](https://cardano-foundation.github.io/cardano-wallet/contributor/what/coding-standards)
for details.

For more information, please consult our [Contributor
Manual](https://cardano-foundation.github.io/cardano-wallet/contributor).
