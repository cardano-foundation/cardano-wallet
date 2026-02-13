# **Fourmolu Migration**

|              |                 |
|--------------|-----------------|
| Decided      | 2026-02-12      |
| Decided by   | Paolo Veronelli |

## **Why**

The project previously used [stylish-haskell](https://github.com/haskell/stylish-haskell) for formatting imports and language pragmas. However, stylish-haskell only handled a narrow subset of formatting concerns — it did not format function definitions, type signatures, expressions, or any other code outside of imports and pragmas. This left most formatting decisions to manual convention enforcement.

[Fourmolu](https://github.com/fourmolu/fourmolu) is a full-source Haskell formatter that handles all code, not just imports. Adopting it eliminates formatting discussions during code review and ensures consistent style across the entire codebase.

## **Decision**

Replace stylish-haskell with fourmolu as the project's code formatter. Configuration is in `fourmolu.yaml` at the repository root.

Key settings:

```yaml
comma-style: leading
function-arrows: leading
import-export-style: leading
indentation: 4
column-limit: 70
haddock-style: single-line
record-brace-space: false
respectful: true
```

This supersedes the stylish-haskell section in [Coding Standards](../what/coding-standards.md).

## **Rationale**

- **Full coverage**: fourmolu formats all Haskell source code, not just imports and pragmas
- **Deterministic**: running `fourmolu --mode inplace` on any file produces canonical output, eliminating formatting debates
- **Leading style**: the leading commas/arrows setting aligns with the project's existing variable-length indentation avoidance policy
- **70-character column limit**: slightly narrower than the historical 80-character guideline, producing more readable diffs

## **References**

- [PR #5169](https://github.com/cardano-foundation/cardano-wallet/pull/5169) — initial fourmolu migration
- [fourmolu configuration docs](https://fourmolu.github.io/fourmolu/config/current/)
