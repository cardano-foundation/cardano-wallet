# Coding Standards

## Foreword

This file contains agreed-upon coding standards and best practices, as well as proposals for changes or new standards. Proposals are prefixed with `[PROPOSAL]` and are voted on by the Adrestia team through polls on Slack. To be accepted, a practice should be voted with majority + 1, with neutral votes counting as positive votes.

Each proposal should start with a section justifying the standard with rational arguments. When it makes sense, we should also provide examples of good and bad practices to make the point clearer.

## Summary

- [Coding Standards](#coding-standards)
  - [Foreword](#foreword)
  - [Summary](#summary)
  - [Code Formatting](#code-formatting)
    - [Editor Configuration via `.editorconfig`](#editor-configuration-via-editorconfig)
    - [Limit line length to 80 characters](#limit-line-length-to-80-characters)
      - [Examples](#examples)
        - [Strategy 1: Wrap code](#strategy-1-wrap-code)
        - [Strategy 2: Place comments on their own line instead of attempting to align them vertically](#strategy-2-place-comments-on-their-own-line-instead-of-attempting-to-align-them-vertically)
        - [Strategy 3: Break up long string literals](#strategy-3-break-up-long-string-literals)
        - [Strategy 4: Reduce nesting](#strategy-4-reduce-nesting)
      - [Exceptions](#exceptions)
        - [Exception 1: URLs in comments](#exception-1-urls-in-comments)
    - [Use only a single blank line between top-level definitions](#use-only-a-single-blank-line-between-top-level-definitions)
    - [Avoid Variable-Length Indentation](#avoid-variable-length-indentation)
    - [Stylish-Haskell is used to format grouped imports \& language pragmas](#stylish-haskell-is-used-to-format-grouped-imports--language-pragmas)
  - [Haskell Practices](#haskell-practices)
    - [Favor `newtype` and tagged type over type-aliases](#favor-newtype-and-tagged-type-over-type-aliases)
    - [Language extensions are specified on top of each module](#language-extensions-are-specified-on-top-of-each-module)
    - [HLint is used for hints and general code style](#hlint-is-used-for-hints-and-general-code-style)
    - [We use explicit imports by default, and favor qualified imports for ambiguous functions](#we-use-explicit-imports-by-default-and-favor-qualified-imports-for-ambiguous-functions)
    - [All modules begin with a helpful documentation comment](#all-modules-begin-with-a-helpful-documentation-comment)
    - [Prefer named constants over magic numbers](#prefer-named-constants-over-magic-numbers)
      - [BAD](#bad)
      - [GOOD](#good)
    - [Avoid wildcards when pattern-matching on sum types](#avoid-wildcards-when-pattern-matching-on-sum-types)
    - [Prefer pattern-matching to equality testing on sum types.](#prefer-pattern-matching-to-equality-testing-on-sum-types)
    - [\[PROPOSED\] Don't spit back malformed values in errors from user inputs.](#proposed-dont-spit-back-malformed-values-in-errors-from-user-inputs)
  - [QuickCheck](#quickcheck)
    - [See your property fail](#see-your-property-fail)
    - [Define properties as separate functions](#define-properties-as-separate-functions)
    - [Provide readable counter-examples on failures](#provide-readable-counter-examples-on-failures)
    - [Tag interesting cases in complex properties](#tag-interesting-cases-in-complex-properties)
    - [Write properties to assert the validity of complex generators (and shrinkers)](#write-properties-to-assert-the-validity-of-complex-generators-and-shrinkers)
    - [Use `checkCoverage` to measure coverage requirements](#use-checkcoverage-to-measure-coverage-requirements)
    - [Avoid `liftIO` in monadic properties](#avoid-liftio-in-monadic-properties)
  - [Testing](#testing)
    - [Test files are separated and self-contained](#test-files-are-separated-and-self-contained)
    - [Unit test files names match their corresponding module](#unit-test-files-names-match-their-corresponding-module)

## Code Formatting

### Editor Configuration via `.editorconfig`

A `.editorconfig` (see https://editorconfig.org/) at the root of the project specifies for various filetype:

- Line length
- Indentation style (spaces vs tabs)
- Encoding

This file should be parsed and enforced by any contributor's editor.

> *Why*
>
> This is the kind of details we don't want to be fighting over constantly. The `.editorconfig` are widely used,
> easily written and supported by any decent editor out there. Agreeing on such rules prevent our version control
> system to go crazy because people use different encoding or indentation style. It makes the overall code more consistent.

### Limit line length to 80 characters

Source code, including comments, should **not** exceed 80 characters in length, unless in exceptional situations.

> *Why*
>
> * **To maximize readability**. Human eyes find it much harder to scan long lines. For people with imperfect vision, it can be even harder. Narrower code can be read quickly without having to scan from side to side. Although monitors have grown in width and resolution in recent years, human eyes haven't changed.
> * **To easily view multiple sources side-by-side**. This is particularly important when working on a laptop. With a readable font size of around 11 pt, 80 characters is about half the horizontal distance across a laptop monitor. Trying to fit 90 or 100 characters into the same width requires a smaller font, raising the level of discomfort for people with poorer vision.
> * **To avoid horizontal scrolling when viewing source code**. When reading most source code, we already have to scroll vertically. Horizontal scrolling means that readers have to move their viewpoint in two dimensions rather than one. This requires more effort and can cause strain for people reading your code.

<details><summary>See Examples and Exceptions</summary>

#### Examples

If you find yourself exceeding 80 characters, there are several strategies you can use.

##### Strategy 1: Wrap code

By inserting carriage returns in the right place, we can often reveal the underlying structure of an expression. Haskell allows you to break up long expressions so that they occur over multiple lines. For example:

```Haskell
-- BAD
instance Bi Block where
    encode block = encodeListLen 3 <> encode (blockHeader block) <> encode (blockBody block) <> encode (blockExtraData block)
```
```Haskell
-- GOOD
instance Bi Block where
    encode block = encodeListLen 3
        <> encode (blockHeader block)
        <> encode (blockBody block)
        <> encode (blockExtraData block)
```

Another example of wrapping:
```hs
-- BAD:
describe "Lemma 2.6 - Properties of balance" $ do
    it "2.6.1) dom u ⋂ dom v ==> balance (u ⋃ v) = balance u + balance v" (checkCoverage prop_2_6_1)
    it "2.6.2) balance (ins⋪ u) = balance u - balance (ins⊲ u)" (checkCoverage prop_2_6_2)
```
```hs
-- GOOD:
describe "Lemma 2.6 - Properties of balance" $ do
    it "2.6.1) dom u ⋂ dom v ==> balance (u ⋃ v) = balance u + balance v"
        (checkCoverage prop_2_6_1)
    it "2.6.2) balance (ins⋪ u) = balance u - balance (ins⊲ u)"
        (checkCoverage prop_2_6_2)
```

##### Strategy 2: Place comments on their own line instead of attempting to align them vertically

```Haskell
-- BAD
mkMagicalBlock
    :: MagicProtocolId                     -- A unique key specifying a magic protocol.
    -> MagicType                           -- The type of magic used in this block signing.
    -> MagicalKey                          -- The magical key used in this block signing.
    -> Maybe Delegation.MagicalCertificate -- A magical certificate of delegation, in case the specified 'MagicalKey' does not have the right to sign this block.
    -> Block
```
```Haskell
-- GOOD
mkMagicalBlock
    :: MagicProtocolId
    -- ^ A unique key specifying a magic protocol.
    -> MagicType
    -- ^ The type of magic used in this block signing.
    -> MagicalKey
    -- ^ The magical key used in this block signing.
    -> Maybe Delegation.MagicalCertificate
    -- ^ A magical certificate of delegation, in case the specified
    -- 'MagicalKey' does not have the right to sign this block.
    -> Block
```

##### Strategy 3: Break up long string literals

Haskell provides convenient support for multi-line string literals:

```Haskell
-- BAD
errorAccountFundsCompletelyExhausted = "The funds in this account have been completely spent, and its balance is now zero. Either add more funds to this account or use a different account for this transaction."
```
```Haskell
-- GOOD
errorAccountFundsCompletelyExhausted =
    "The funds in this account have been completely spent, and its balance \
    \is now zero. Either add more funds to this account or use a different \
    \account for this transaction."
```

```Haskell
-- BAD:
spec = do
    scenario "Only this account's balance can be retrieved while standing on one leg on the side of an extremely tall mountain, and breathing thin air with only very limited amounts of oxygen." $ do
```
```Haskell
-- GOOD:
spec = do
    scenario
        "Only this account's balance can be retrieved while standing on one \
        \leg on the side of an extremely tall mountain, and breathing thin \
        \air with only very limited amounts of oxygen." $ do
```

##### Strategy 4: Reduce nesting

If your function contains so many levels of nesting that it's hard to keep things within 80 characters (even with careful use of wrapping), consider breaking your function up into smaller parts.

#### Exceptions

Sometimes, it's **impossible** to adhere to this rule.

Here is a list of allowed exceptions:

##### Exception 1: URLs in comments

According to the standard, URLs can be extremely long. In some situations, we need to place URLs in source code comments. If a URL is longer than 80 characters, then place it on its own line:

```hs
--| For more information about this implementation, see:
--  https://an.exceptionally.long.url/7919ce329e804fc0bc1fa2df8a141fd3d996c484cf7a49e79f14d7bd974acadd
```

</details>

### Use only a single blank line between top-level definitions

A source code file **should not** contain multiple consecutive blank lines.

Use only a **single** blank line between the following top-level definitions:
* function definitions
* data type definitions
* class definitions
* instance definitions

> *Why*
>
> * Consistency with other Haskell code.
> * Excessive vertical space increases the amount of unnecessary scrolling required to read a module.

<details>
  <summary>See Examples</summary>

```hs
-- BAD
newtype Foo = Foo Integer
    deriving (Eq, Show)



newtype Bar = Bar Integer
    deriving (Eq, Show)
```

```hs
-- GOOD
newtype Foo = Foo Integer
    deriving (Eq, Show)

newtype Bar = Bar Integer
    deriving (Eq, Show)
```

```hs
-- BAD
instance FromCBOR Block where
    fromCBOR = Block <$> decodeBlock



newtype BlockHeader = BlockHeader
    { getBlockHeader :: Primitive.BlockHeader
    }
    deriving Eq
```

```hs
-- GOOD
instance FromCBOR Block where
    fromCBOR = Block <$> decodeBlock

newtype BlockHeader = BlockHeader
    { getBlockHeader :: Primitive.BlockHeader
    }
    deriving Eq
```

</details>

### Avoid Variable-Length Indentation

Variables, arguments, fields and tokens in general shouldn't be aligned based
on the length of a previous token. Rather, tokens should go over a new line and
be indented one-level extra when it makes sense, or not be aligned at all.

> *Why*
>
> Haskellers have a tendency to over-align everything vertically for the sake
> of readability. In practice, this is much more of an habit than a real gain
> in readability. Aligning content based on a function name, variable name or
> record field tends to create unnecessarily long diffs and needless conflicts
> in version control systems when making a change to add an argument, variable
> or parameters. Favoring new-line and fixed-length alignment plays nicer with
> version control.

<details>
  <summary>See Examples</summary>

```hs
-- GOOD
data AddressPool address = AddressPool
    { _addresses :: !(Map address Word32)
    , _gap :: !AddressPoolGap
    }

-- GOOD
data AddressPool address = AddressPool
    { _addresses
        :: !(Map address Word32)
    , _gap
        :: !AddressPoolGap
    }

-- GOOD
deriveAccountPrivateKey
    :: PassPhrase
    -> EncryptedSecretKey
    -> Word32
    -> Maybe EncryptedSecretKey
deriveAccountPrivateKey passPhrase masterEncPrvKey accountIx =

-- BAD
myFunction :: Word64 -> Maybe String
myFunction w = let res = Wrap w in
               case someOp res of
                 Left _err -> Nothing
                 Right ()  -> Just coin

-- BAD
myFunction :: Int
           -> Maybe ByteString
           -> Set Word32
           -> Update DB (Either [Word32]
                        (Map Word32 ([String], Set ByteString)))

-- BAD
data MyRecord = MyRecord
    { _myRecordLongNameField :: !String
    , _myRecordShort         :: ![Int]
    }
```
</details>


### Stylish-Haskell is used to format grouped imports & language pragmas

Contributors' editors should pick up and enforce the rules defined by the `.stylish-haskell.yaml`
configuration file at the root of the project. Also, in order to maximize readability, imports
should be grouped into three groups, separated by a blank newline.

- Prelude import
- Explicit imports
- Qualified imports

> **Why**
>
> It is rather annoying and time-consuming to align import lines or statement
> as we code and it's much simpler to leave that to our editor. Yet, we do want
> to enforce some common formatting such that everyone gets to be aligned (pun
> intended).
>
> We can use Stylish-Haskell with various set of rules, yet, the same arguments
> from 'Avoid Variable-Length Indentation' applies when it comes to automatic
> formatting. Imports are a real pain with git and Haskell when they are vertically
> aligned based on the imported module's name.

<details>
    <summary>See examples</summary>

```hs
-- GOOD
import Prelude

import Cardano.Wallet.Binary
    ( txId )
import Data.Set
    ( Set )
import Data.Traversable
    ( for )

import qualified Data.Map as Map
import qualified Data.Set as Set

-- BAD
import Cardano.Wallet.Binary
    ( txId )
import Data.Set
    ( Set )
import Prelude
import Data.Traversable
    ( for )

import qualified Data.Map as Map
import qualified Data.Set as Set

-- BAD
import Prelude

import Cardano.Wallet.Binary
    ( txId )
import qualified Data.Set as Set
import Data.Set
    ( Set )
import qualified Data.Map as Map
import Data.Traversable
    ( for )
```
</details>

Here below is a proposal for the initial set of rules:

```yaml
columns: 80 # Should match .editorconfig
steps:
  - imports:
      align: none
      empty_list_align: inherit
      list_align: new_line
      list_padding: 4
      long_list_align: new_line_multiline
      pad_module_names: false
      separate_lists: true
      space_surround: true

  - language_pragmas:
      align: false
      remove_redundant: true
      style: vertical
```

<details>
  <summary>See example</summary>

  ```hs
  {-# LANGUAGE BangPatterns #-}
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE DeriveGeneric #-}
  {-# LANGUAGE DerivingStrategies #-}
  {-# LANGUAGE FlexibleContexts #-}
  {-# LANGUAGE TupleSections #-}
  {-# LANGUAGE TypeApplications #-}
  {-# LANGUAGE TypeFamilies #-}

  module Main where

  import Control.Applicative
      ( (<|>) )
  import Control.Arrow
      ( first )
  import Control.Concurrent.MVar
      ( modifyMVar_, newMVar, putMVar, readMVar, takeMVar )
  import Crypto.Hash.Algorithms
      ( Blake2b_224, Blake2b_256, SHA3_256, SHA512 (..) )
  import Lens.Micro
      ( at, (%~), (&), (.~), (^.) )
  import Network.HTTP.Client
      ( Manager
      , defaultRequest
      , httpLbs
      , path
      , port
      , responseBody
      , responseStatus
      )

  import qualified Codec.CBOR.Decoding as CBOR
  import qualified Codec.CBOR.Encoding as CBOR
  import qualified Codec.CBOR.Read as CBOR
  import qualified Codec.CBOR.Write as CBOR
  import qualified Crypto.Cipher.ChaChaPoly1305 as Poly
  ```
</details>

## Haskell Practices

### Favor `newtype` and tagged type over type-aliases

Instead of writing type aliases, one should favor wrapping up values in newtype
when it makes sense, or, have them wrapped into a tagged type with a phantom
type to convey some extra meaning while still preserving type safeness. By
using newtypes, we actually extend our program vocabulary and increase its
robustness.

> **Why**
>
> Type-aliases convey a false sense of type-safety. While they usually make
> things a bit better for the reader, they have a tendency to spread through
> the code-base transforming those sweet help spot into traps. We can't define
> proper instances on type aliases, and we treat them as different type whereas
> they are behind the scene, just another one.

<details>
  <summary>See examples</summary>

  ```hs
  -- GOOD
  newtype HardenedIndex = HardenedIndex { getHardenedIndex :: Word32 }
  deriveAccount :: HardenedIndex -> XPrv -> XPrv

  -- GOOD
  data Scheme = Seq | Rnd
  newtype Key (* :: Scheme) = Key { getKey :: XPrv }
  deriveAccount :: Word32 -> Key 'Seq -> Key 'Seq

  -- GOOD
  newtype Tagged (* :: Symbol) = Tagged { getTagged :: String }
  startNode :: Tagged "nodeId" -> IO ()

  -- BAD
  type HardenedIndex = Word32
  deriveAccount :: HardenedIndex -> XPrv -> XPrv
  ```
</details>

### Language extensions are specified on top of each module

Haskell's language extension are specified on top of each module.

> **Why**
>
> Having a lot of default extensions enabled across the whole project can sometimes lead to cryptic
> errors where GHC would interpret things differently because of the enabled extensions. Yet, it's
> sometimes hard to distinguish by simply looking at the module themselves.
>
> Also, being more explicit on extensions used by a module can help speeding up compile-time of such simple modules
> that don't need to be pull in a lot of extra complexity.

<details>
  <summary>See examples</summary>

  ```hs
  -- GOOD
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE GeneralizedNewtypeDeriving #-}
  {-# LANGUAGE DerivingStrategies #-}

  module Cardano.Wallet where

  -- BAD
  default-extensions:
    - DataKinds
    - GeneralizedNewtypeDeriving
    - DerivingStrategies
  ```
</details>

### HLint is used for hints and general code style

Contributors' editors should pick up and enforce the rules defined by the
.hlint.yaml configuration file at the root of the project. File should be
committed without warnings or errors. When it make senses, developer may ignore
lints at a function site using a proper annotation:

> **Why**
>
> Linters are common practices in software development and help maintaining consistency across a large codebase with
> many developers. Hlint is de de-facto linter in Haskell and comes with a lot of different rules and features that
> are _most of the time_ rather relevant and convey good practices, agreed upon and shared across the team.

e.g.

```hs
{-# ANN decodeBlock ("HLint: ignore Use <$>" :: String) #-}
```

As a start, we'll use the following built-in rules from `hlint` with the following configuration, and refine this as we move forward:

```yaml
- modules:
  # Enforce some common qualified imports aliases across the codebase
  - {name: [Data.Aeson, Data.Aeson.Types], as: Aeson}
  - {name: [Data.ByteArray], as: BA}
  - {name: [Data.ByteString.Base16], as: B16}
  - {name: [Data.ByteString.Char8], as: B8}
  - {name: [Data.ByteString.Lazy], as: BL}
  - {name: [Data.ByteString], as: BS}
  - {name: [Data.Foldable], as: F}
  - {name: [Data.List.NonEmpty], as: NE}
  - {name: [Data.List], as: L}
  - {name: [Data.Map.Strict], as: Map}
  - {name: [Data.Sequence], as: Seq}
  - {name: [Data.Set, Data.HashSet], as: Set}
  - {name: [Data.Text, Data.Text.Encoding], as: T}
  - {name: [Data.Vector], as: V}

# Ignore some build-in rules
- ignore: {name: "Reduce duplication"} # This is a decision left to developers and reviewers
- ignore: {name: "Redundant bracket"} # Not everyone knows precedences of every operators in Haskell. Brackets help readability.
- ignore: {name: "Redundant do"} # Just an annoying hlint built-in, GHC may remove redundant do if he wants
```

### We use explicit imports by default, and favor qualified imports for ambiguous functions

Apart from the chosen prelude, there should be no implicit imports. Instead,
every function or class used from a given module should be listed explicitly.
In case where a function name is ambiguous or requires context, a qualified
import should be used instead (this is mainly the case for modules coming from
`containers`, `bytestring` and `aeson`).

> **Why**
>
> Imports can be a great source of pain in Haskell. When dealing with some
> foreign code (and every code becomes quite hostile after a while, even if we
> originally wrote it), it can be hard to understand where functions and
> abstractions are pulled from. On the other hand, fully qualified imports can
> become verbose and a real impediment to readability.

<details>
  <summary>See examples</summary>

  ```hs
  -- GOOD
  import Prelude
  import Control.DeepSeq
      ( NFData (..) )
  import Data.ByteString
      ( ByteString )
  import Data.Map.Strict
      ( Map )
  import Data.Aeson
      ( FromJSON (..), ToJSON (..) )


  -- GOOD
  import qualified Data.Map.Strict as Map
  import qualified Data.ByteString as BS

  isSubsetOf :: UTxO -> UTxO -> Bool
  isSubsetOf (UTxO a) (UTxO b) =
      a `Map.isSubmapOf` b

  (magic, filetype, version) =
      ( BS.take 8 bytes
      , BS.take 4 $ BS.drop 8 bytes
      , BS.take 4 $ BS.drop 12 bytes
      )


  -- BAD
  import Options.Applicative


  -- BAD
  import qualified Data.Aeson as Aeson

  instance Aeson.FromJSON MyType where
      -- ...


  -- BAD
  import Data.Map.Strict
      ( filter )
  import Data.Set
      ( member )

  restrictedTo :: UTxO -> Set TxOut ->  UTxO
  restrictedTo (UTxO utxo) outs =
      UTxO $ filter (`member` outs) utxo
  ```
</details>


### All modules begin with a helpful documentation comment

The comments might answer the question _why?_ They _might_:
1. Explain the relation to other modules
2. Explain the relation to business functionality
3. Provide some other good-to-know information

We should keep an eye out out-of-date comments. For instance when creating and reviewing PRs.

> **Why**
>
> Even if individual functions are well-documented, it can be difficult to grasp how it all fits together.
>
> In the legacy code-base, it was common to have multiple functions with the same or similar names, in different modules.
> Try seaching for `applyBlocks` or `switchToFork`. What is the difference between `DB.Spec.Update.switchToFork` and `DB.AcidState.switchToFork`?
>
> Having a comment at the top of each module would be an easy-to-follow rule to better document this. It is also very appropriate for
> generated [haddock docs](https://cardano-foundation.github.io/cardano-wallet/haddock/).
>
> If we re-design a module and forget to update the comment, the comment is no longer useful.

<details>
  <summary>See examples</summary>

  ```hs
-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains the core primitive of a Wallet. This is roughly a
-- Haskell translation of the [Formal Specification for a Cardano Wallet](https://github.com/cardano-foundation/cardano-wallet/blob/master/specifications/wallet/formal-specification-for-a-cardano-wallet.pdf)
--
-- It doesn't contain any particular business-logic code, but define a few
-- primitive operations on Wallet core types as well.
```
(https://github.com/cardano-foundation/cardano-wallet/blob/d3cca01f66f0abe93012343dab093a2551b6cbea/src/Cardano/Wallet/Primitive.hs#L12-L20)

```hs
-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Provides the wallet layer functions that are used by API layer and uses both
-- "Cardano.DBLayer" and "Cardano.NetworkLayer" to realize its role as being
-- intermediary between the three.
```

(https://cardano-foundation.github.io/cardano-wallet/haddock/cardano-wallet-2.0.0/Cardano-WalletLayer.html)

</details>

### Prefer named constants over magic numbers

> **Why**
>
> A **magic number** (or magic value) is a value that appears in source code without an accompanying explanation, which could (preferably) be replaced with a named constant.
>
> The use of an unnamed magic number often obscures the developer's intent in choosing that number, increases opportunities for subtle errors and makes it more difficult for the program to be adapted and extended in the future.
>
> Replacing all significant magic numbers with named constants makes programs easier to read, understand and maintain. Named constants can also be reused in multiple places, making it obvious that the value is supposed to be the same across all places that its used.

<details>
  <summary>See examples</summary>

#### BAD
```hs
humanReadableCharIsValid :: Char -> Bool
humanReadableCharIsValid c = c >= chr 33 && c <= chr 126

bech32CharSet :: Set Char
bech32CharSet =
    Set.filter (not . isUpper) $
        Set.fromList [chr 33 .. chr 126]
            `Set.union` (Set.singleton '1')
            `Set.union` (Set.fromList "qpzry9x8gf2tvdw0s3jn54khce6mua7l")

instance Arbitrary HumanReadableChar where
    arbitrary = HumanReadableChar <$>
        choose (chr 33, chr 126)
```
#### GOOD
```hs
-- | The lower bound of the set of characters permitted to appear within the
--   human-readable part of a Bech32 string.
humanReadableCharMinBound :: Char
humanReadableCharMinBound = chr 33

-- | The upper bound of the set of characters permitted to appear within the
--   human-readable part of a Bech32 string.
humanReadableCharMaxBound :: Char
humanReadableCharMaxBound = chr 126

-- | The separator character. This character appears immediately after the
-- human-readable part and before the data part.
separatorChar :: Char
separatorChar = '1'

-- | A list of all characters that are permitted to appear within the data part
--   of a Bech32 string.
dataCharList :: String
dataCharList = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

humanReadableCharIsValid :: Char -> Bool
humanReadableCharIsValid c =
    c >= humanReadableCharMinBound &&
    c <= humanReadableCharMaxBound

bech32CharSet :: Set Char
bech32CharSet =
    Set.filter (not . isUpper) $
        Set.fromList [humanReadableCharMinBound .. humanReadableCharMaxBound]
            `Set.union` (Set.singleton separatorChar)
            `Set.union` (Set.fromList dataCharList)

instance Arbitrary HumanReadableChar where
    arbitrary = HumanReadableChar <$>
        choose (humanReadableCharMinBound, humanReadableCharMaxBound)
```

</details>

### Avoid wildcards when pattern-matching on sum types

When pattern-matching on sum types or finite structures, we should avoid
the use of the wildcard `_` as much as possible, and instead favor explicit
handling of all branches. This way, we get compiler errors when extending
the underlying ADT and avoid silently handling (probably incorretly) some
of the new branches.

> **Why**
>
> When pattern-matching on sum types it is tempting to handle a few similar cases
> using a wildcard `_`. However, this often leads to undesirable behavior when
> adding new branches to an ADT. Compilers won't trigger any warnings and, as
> developers, we might miss some necessary logic updates in existing pattern
> matches.

<details>
  <summary>See examples</summary>

  ```hs
  -- GOOD
  isPositive = \case
      InLedger -> True
      Pending -> False
      Invalidated -> False

  -- BAD
  isPositive = \case
      InLedger -> True
      _ -> False

  -- BAD
  handleErr = \case
      ErrWalletNotFound -> {- ... -}
      _ -> ErrUnknown
  ```
</details>

### Prefer pattern-matching to equality testing on sum types.

For expressions that evaluate differently depending on a value of a sum type,
prefer pattern matching over equality testing for values of that type.

> **Why**
>
> When conditional evaluation depends on the value of a sum type, it's tempting
> to use a test for equality or inequality to branch on a particular
> value.
>
> However, if someone adds a new constructor to the sum type later on,
> we'd ideally like the compiler to remind us to check all locations that inspect
> values of this type, especially where conditional evaluation is involved.
>
> Using an equality test is non-ideal because the compiler won't necessarily
> fail if a new constructor is added to the underlying sum type, whereas it
> will **always** fail if a pattern match becomes incomplete.

<details>
  <summary>See examples</summary>

  ```hs
  data SortOrder = Ascending | Descending
      deriving Eq

  -- BAD
  sortWithOrder' :: Ord a => SortOrder -> [a] -> [a]
  sortWithOrder' order = f . sort
    where
      f = if order == Ascending then id else reverse

  -- GOOD
  sortWithOrder :: Ord a => SortOrder -> [a] -> [a]
  sortWithOrder order = f . sort
    where
      f = case order of
          Ascending -> id
          Descending -> reverse
  ```
</details>

### [PROPOSED] Don't spit back malformed values in errors from user inputs.

When failing to parse user inputs, error message should not contain the malformed input. Instead, the error message should contain hints or examples of well-formed values expected by the parser. It is acceptable to show a raw input value if it is known to be within acceptable boundaries (e.g. parsing a `Word32` into a more refined type, there is little chance that the `Word32` will be inadequate to display).

> **Why**
>
> Spitting back what the user has just entered is generally not very helpful. Users can generally easily replay what they've entered and see for themselves. More importantly, an input that didn't parse successfully may be arbitrary long or improper for display; since it failed to parse, we have actually not much control or knowledge about it.

<details>
  <summary>See examples</summary>

```hs
-- BAD
err =
    "Invalid value: " <> show v <> ". Please provide a valid value."

-- GOOD
err =
    "EpochNo value is out of bounds (" <>
    show (minBound @Word31) <>
    ", " <>
    show (maxBound @Word31) <>
    ")."

-- GOOD
err =
    "Unable to decode FeePolicy: \
    \Linear equation not in expected format: a + bx + cy \
    \where 'a', 'b', and 'c' are numbers"
```
</details>

## QuickCheck

### See your property fail

This is a general practice in TDD (**T**est **D**riven **D**evelopment) but
even more important in property-based testing.  You want to see _how_ your
property fails and whether, as a developer, you have enough information to
understand the reason of the failure and debug it.

> **Why**
>
> It is really easy to write all sort of properties which, once they fail, give
> close to no details about the reason why they failed. Yet, as with any tests,
> one wants to understand what happens. It is therefore important to see
> properties failing at least once to see whether the level of details is
> sufficient, as well as the efficiency of the shrinkers.

### Define properties as separate functions

It is often tempting to write properties inlined with `hspec` other combinators
instead of having them as separate functions. However, we recommend writing
properties as separate functions, prefixed with a `prop_` prefix to clearly
identify them.

> **Why**
>
> It makes for more readable test files where the set of properties can be easily
> identified by looking at the top-level exported spec. But more importantly, it
> allows for re-using the property with some regression test cases coming from past
> failures. Having a separate function makes it easy to simply apply counter examples
> yielded by QuickCheck as arguments!

<details>
  <summary>See examples</summary>

```hs
-- GOOD
describe "selectCoinsForMigration properties" $ do
    it "Total input UTxO value >= sum of selection change coins" $
        property $ withMaxSuccess 1000 prop_inputsGreaterThanOutputs

describe "selectCoinsForMigration regressions" $ do
    it "regression #1" $ do
        let feeOpts = FeeOptions
                { dustThreshold = Coin 9
                , estimateFee = \s -> Fee
                    $ fromIntegral
                    $ 5 * (length (inputs s) + length (outputs s))
                }
        let batchSize = 1
        let utxo = UTxO $ Map.fromList
                [ ( TxIn { inputId = Hash "|\243^\SUBg\242\231\&1\213\203", inputIx = 2 }
                  , TxOut { address = Address "ADDR03", coin = Coin 2 }
                  )
                ]
        property $ prop_inputsGreaterThanOutputs feeOpts batchSize utxo


-- | Total input UTxO value >= sum of selection change coins
prop_inputsGreaterThanOutputs
    :: FeeOptions
    -> Word8
    -> UTxO
    -> Property
prop_inputsGreaterThanOutputs feeOpts batchSize utxo = {- ... -}


-- BAD
it "Eventually converge for decreasing functions" $ do
    property $ \coinselOpts -> do
        let batchSize = idealBatchSize coinselOpts
        label (show batchSize) True
```

</details>

### Provide readable counter-examples on failures

Use [counterexample](https://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck.html#v:counterexample) to display human-readable
counter examples when a test fails; in particular, for data-types which have a [Buildable](https://hackage.haskell.org/package/fmt-0.6.1.2/docs/Fmt.html#t:Buildable) instances
that are typically hard to read through their standard `Show` instance. For monadic properties, this can be used via [monitor](https://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck-Monadic.html#v:monitor).

> **Why**
>
> Some property-based tests can use complex combinations of inputs that can be hard to decipher
> when printed out to the console using only the stock `Show` instance. On the other hand, we
> want to keep using the stock `Show` instance in order to be able to easily copy-paste failing
> cases and turn them into regression tests. QuickCheck however provides a good set of tools to
> display counter examples on failures to ease debbugging.

<details>
  <summary>See examples</summary>

```hs
property (Bech32.decode corruptedString `shouldSatisfy` isLeft)
    & counterexample $ unlines
        [ "index of char #1: " <> show index
        , "index of char #2: " <> show (index + 1)
        , "         char #1: " <> show char1
        , "         char #2: " <> show char2
        , " original string: " <> show originalString
        , "corrupted string: " <> show corruptedString
        ]

property (bs' === Just expected)
    & counterexamples $ unlines
        [ "k = " ++ show k
        , "Local chain: " ++ showChain localChain
        , "Node chain:  " ++ showChain nodeChain
        , "Intersects:  " ++ maybe "-" showSlot isect
        , "Expected:    " ++ showBlockHeaders expected
        , "Actual:      " ++ maybe "-" showBlockHeaders bs'
        ]
```
</details>

### Tag interesting cases in complex properties

Quickcheck provides good tooling for labelling (see [label](https://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck.html#v:label) and classifying (see [classify](https://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck.html#v:classify))
inputs or results of a property. These should be used in properties dealing with several classes of values.

> **Why**
>
> It is quite common for properties to deal with different class of values and for us developers to get a false sense of
> coverage. QuickCheck default generators are typically skewed towards certain edge values to favor bug finding but this
> is sometimes counter-intuitive. For example, when testing with lists or maps, it often happens that most of the test cases
> are actually testing on empty values. In order to make sure that some interesting test cases are still covered, it is
> necessary to instrument properties so that they can mesure how often certain cases appear in a particular property.

<details>
  <summary>See Examples</summary>

```hs
prop_sync :: S -> Property
prop_sync s0 = monadicIO $ do
    {- ... -}
    monitor (label (intersectionHitRate consumer))
    monitor (classify (initialChainLength (const (== 1))) "started with an empty chain")
    monitor (classify (initialChainLength (\k -> (> k))) "started with more than k blocks")
    monitor (classify addMoreThanK "advanced more than k blocks")
    monitor (classify rollbackK "rolled back full k")
    monitor (classify (switchChain (<)) "switched to a longer chain")
    monitor (classify (switchChain (>)) "switched to a shorter chain")
    monitor (classify (switchChain (const (== 0))) "rewinded without switch")
    monitor (classify (recoveredFromGenesis s) "recovered from genesis")
    monitor (classify (startedFromScratch c0Cps) "started from scratch")
    {- ... -}

-- Syncs with mock node
--   64.709% started from scratch
--   55.969% advanced more than k blocks
--   53.260% started with an empty chain
--   41.094% started with more than k blocks
--   10.421% switched to a shorter chain
--    7.195% switched to a longer chain
--    6.773% rewinded without switch
--    0.880% rolled back full k
--
--   57.516% Intersection hit rate GREAT (75% - 100%)
--   32.183% Intersection hit rate GOOD  (50% - 75%)
--   10.292% Intersection hit rate POOR  (10% - 50%)
--    0.009% Intersection hit rate BAD   (0%  - 10%)


prop_rollbackPools db pairs = monadicIO $ do
    {- ... -}
    Monitor $ classify (any (> sl) beforeRollback) "something to roll back"
    Monitor $ classify (all (<= sl) beforeRollback) "nothing to roll back"
    {- ... -}

-- Rollback of stake pool production
--   57% nothing to roll back
--   43% something to roll back

prop_accuracy r = withMaxSuccess 1000 $ monadicIO $ do
    {- ... -}
    monitor $ label $ accuracy dust balance balance'
  where
    accuracy :: Coin -> Natural -> Natural -> String
    accuracy (Coin dust) sup real
        | a >= 1.0 =
            "PERFECT  (== 100%)"
        | a > 0.99 || (sup - real) < fromIntegral dust =
            "OKAY     (>   99%)"
        | otherwise =
            "MEDIOCRE (<=  99%)"
      where
        a = double real / double sup

-- Accuracy of selectCoinsForMigration
--   dust=1%
--     +++ OK, passed 1000 tests (100.0% PERFECT  (== 100%)).
--   dust=5%
--     +++ OK, passed 1000 tests (100.0% PERFECT  (== 100%)).
--   dust=10%
--     +++ OK, passed 1000 tests:
--     99.8% PERFECT  (== 100%)
--      0.2% OKAY     (>   99%)
--   dust=25%
--     +++ OK, passed 1000 tests:
--     99.6% PERFECT  (== 100%)
--      0.4% OKAY     (>   99%)
--   dust=50%
--     +++ OK, passed 1000 tests:
--     98.8% PERFECT  (== 100%)
--      1.2% OKAY     (>   99%)
```
</details>

### Write properties to assert the validity of complex generators (and shrinkers)

Arbitrary generators, and in particular complex ones, should be tested independently
to make sure they yield correct values. This also includes shrinkers associated with
the generator which can often break some invariants enforced by the generator itself.

> **Why**
>
> Generators and shrinkers are at the heart of property-based testing. Writing properties
> using clunky generators will lead to poor or wrong results. Above all, it may take an
> important amount of time to debug failures due to an invalid generator. So it's best
> to start by verifying the a given generator is somewhat correct. Often enough, generators
> are obvious, but when they are slightly more engineered, testing them is a must.

<details>
    <summary>See Examples</summary>

```hs
--| Checks that generated mock node test cases are valid
prop_MockNodeGenerator :: S -> Property
prop_MockNodeGenerator (S n0 ops _ _) =
    prop_continuous .&&. prop_uniqueIds
  where
    prop_continuous :: Property
    prop_continuous =
        conjoin (follow <$> scanl (flip applyNodeOp) n0 (concat ops))

    prop_uniqueIds :: Property
    prop_uniqueIds =
        length (nub bids) === length bids
            & counterexample ("Non-unique ID: " ++ show bids)
      where
        bids = concat [map mockBlockId bs | NodeAddBlocks bs <- concat ops]

prop_nonSingletonRangeGenerator :: NonSingletonRange Int -> Property
prop_nonSingletonRangeGenerator = property $ \(nsr :: NonSingletonRange Int) ->
    isValidNonSingleton nsr .&&. all isValidNonSingleton (shrink nsr)
  where
    isValidNonSingleton (NonSingletonRange r) =
        rangeIsValid r && not (rangeIsSingleton r) in
```
</details>

### Use `checkCoverage` to measure coverage requirements

Using [label](https://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck.html#v:label) or [classify](https://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck.html#v:classify)
instruments QuickCheck to gather some metrics about a particular properties and print out results in the console. However,
it also possible to _enforce_ that some collected values stay above a certain threshold using [checkCoverage](https://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck.html#v:checkCoverage).
When used, QuickCheck will run the property as many times as necessary until a particular coverage requirement is satisfied, with a certain confidence.

> **Why**
>
> Labelling and classifying is good but, in an evolving code-base where
> generators are sometimes shared between multiple properties, it is possible
> for someone to accidentally make a generator worse for an existing property
> without noticing it. Therefore, by enforcing clear coverage requirements with
> `checkCoverage`, one can make a property fail if the coverage drops below an
> acceptable threshold. For example, a property can measure the proportion of
> empty lists its generator yield and require that at least 50% of all generated
> list are not empty.

<details>
    <summary>See examples</summary>

```hs
prop_rangeIsValid :: Property
prop_rangeIsValid = property $ \(r :: Range Integer) ->
    rangeIsValid r .&&.  all rangeIsValid (shrink r)
        & cover 10 (rangeIsFinite r) "finite range" $
        & checkCoverage

spec :: Spec
spec = do
    describe "Coin selection properties : shuffle" $ do
        it "every non-empty list can be shuffled, ultimately" $
            checkCoverageWith lowerConfidence prop_shuffleCanShuffle
        it "shuffle is non-deterministic" $
            checkCoverageWith lowerConfidence prop_shuffleNotDeterministic
        it "sort (shuffled xs) == sort xs" $
            checkCoverageWith lowerConfidence prop_shufflePreserveElements
  where
    lowerConfidence :: Confidence
    lowerConfidence = Confidence (10^(6 :: Integer)) 0.75
```
</details>

### Avoid `liftIO` in monadic properties

When running monadic properties in IO, it is often required to lift a
particular IO action.  Unfortunately, the `PropertyM` monad in which the
monadic properties are defined have a `MonadIO` instance so using `liftIO` is
tempting. However, one should use [run](https://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck-Monadic.html#v:run)
in order to lift operation in the property monad.

> **Why**
>
> This is very important if the property also contains calls to `monitor`, `label`, `counterexample`
> and so forth... Using `liftIO` actually breaks the abstraction boundary of the property monad which
> then makes the reporting with these combinator ineffective. Using `run` however correctly inserts
> monadic operations and preserve reporting and measures done during the property.

<details>
    <summary>See examples</summary>

```hs
-- GOOD
setup wid meta = run $ do
    cleanDB db
    unsafeRunExceptT $ createWallet db wid cp0 meta mempty
    unsafeRunExceptT $ putTxHistory db wid txs0

prop wid point = do
    run $ unsafeRunExceptT $ rollbackTo db wid point
    txs <- run $ readTxHistory db wid Descending wholeRange Nothing
    monitor $ counterexample $ "\nTx history after rollback: \n" <> fmt txs
    {- ... -}

-- BAD
prop_createWalletTwice db (key@(PrimaryKey wid), cp, meta) =
    monadicIO (setup >> prop)
  where
    setup = liftIO (cleanDB db)
    prop = liftIO $ do
        let err = ErrWalletAlreadyExists wid
        runExceptT (createWallet db key cp meta mempty) `shouldReturn` Right ()
        runExceptT (createWallet db key cp meta mempty) `shouldReturn` Left err
```
</details>

## Testing

### Test files are separated and self-contained

Test files do not import other test files. Arbitrary instances are not shared
across test files and are defined locally. If we do observe a recurring pattern
in tests (like for instance, testing roundtrips), we may consider making this a
library that test can import.

> **Why**
>
> It is really easy to make the testing code more complex than the actual code
> it's initially testing. Limiting the interaction between test modules helps
> keeping a good maintainability and a rather low overhead when it comes to
> extend, modify, read or comprehend some tests. Also, in many cases, we do
> actually want to have different arbitrary generators for different test cases
> so sharing instances is risky and cumbersome.

### Unit test files names match their corresponding module

Every module from a library has a corresponding test file, within the same
folder architecture, and, sharing a same name prefix. Test files are postfixed
with 'Spec' to distinguish them from their corresponding sources.

> **Why**
>
> It is much easier to find the corresponding test to a module if they share
> a same name. Also, this gives consistency and a clear pattern for naming
> tests in order to avoid chaos.

<details>
  <summary>See examples</summary>

  ```
  src/
  ├── Cardano
  │   ├── Environment.hs
  │   └── Wallet
  │       ├── Binary
  │       │   └── HttpBridge.hs
  │       ├── Compatibility
  │       │   └── HttpBridge.hs
  │       ├── Network
  │       │   ├── HttpBridge
  │       │   │   └── Api.hs
  │       │   └── HttpBridge.hs
  │       └── Transaction
  │           └── HttpBridge.hs
  ├── Data
  │   └── Packfile.hs
  └── Servant
      └── Extra
          └── ContentTypes.hs
  test/unit/
  ├── Cardano
  │   ├── EnvironmentSpec.hs
  │   └── Wallet
  │       ├── Binary
  │       │   └── HttpBridgeSpec.hs
  │       ├── Network
  │       │   ├── HttpBridge
  │       │   │   └── ApiSpec.hs
  │       │   └── HttpBridgeSpec.hs
  │       └── Transaction
  │           └── HttpBridgeSpec.hs
  ├── Data
  │   └── PackfileSpec.hs
  └── Servant
      └── Extra
          └── ContentTypesSpec.hs
  ```
