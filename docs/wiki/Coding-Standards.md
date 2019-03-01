# Forewords

This file contains agreed upon coding standards and best practices as well as, proposals for changes or new standards. Proposals are prefixed with `[PROPOSAL]` and are voted by the wallet backend team through poll on Slack. To be accepted, a practice should be voted with majority + 1, neutral votes counting as positive. 

Each proposal should start with a section justifying for the standard with rational arguments. When it makes sense, we may also add examples of good and bad practices to make the point clearer.


# Code Formatting

## Editor Configuration

> *Why*
>
> This is the kind of details we don't want to be fighting over constantly. The `.editorconfig` are widely used,
> easily written and supported by any decent editor out there. Agreeing on such rules prevent our version control
> system to go crazy because people use different encoding or indentation style. It makes the overall code more consistent.

A `.editorconfig` (see https://editorconfig.org/) at the root of the project specifies for various filetype:

- Line length
- Indentation style (spaces vs tabs)
- Encoding 

This file should be parsed and enforced by any contributor's editor. 

## [PROPOSAL] Limit line length to 80 characters

> *Why*
>
> * **To maximize readability**. Human eyes find it much harder to scan long lines. For people with imperfect vision, it can be even harder. Narrower code can be read quickly without having to scan from side to side. Although monitors have grown in width and resolution in recent years, human eyes haven't changed.
> * **To easily view multiple sources side-by-side**. This is particularly important when working on a laptop. With a readable font size of around 11 pt, 80 characters is about half the horizontal distance across a laptop monitor. Trying to fit 90 or 100 characters into the same width requires a smaller font, raising the level of discomfort for people with poorer vision.
> * **To avoid horizontal scrolling when viewing source code**. When reading most source code, we already have to scroll vertically. Horizontal scrolling means that readers have to move their viewpoint in two dimensions rather than one. This requires more effort and can cause strain for people reading your code.

Source code, including comments, should **not** exceed 80 characters in length.

<details><summary>See Details and Examples</summary>

If you find yourself exceeding 80 characters, there are several strategies you can use.

### 1. Wrap code.

By inserting carriage returns in the right place, we can often reveal the underlying structure of an expression. Haskell allows you to break up long expressions so that they occur over multiple lines. For example:

```Haskell
-- BAD
instance Bi Block where
  encode block = encodeListLen 3 <> encode (blockHeader block) <> encode (blockBody block) <> encode (blockExtraData block)
```
```Haskell
-- GOOD
instance Bi Block where
  encode block =
    encodeListLen 3
      <> encode (blockHeader block)
      <> encode (blockBody block)
      <> encode (blockExtraData block)
```

### 2. Place comments on their own line, instead of attempting to align them vertically:

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

### 3. Break up long string literals.

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

### 4. Reduce nesting.

If your function contains so many levels of nesting that it's hard to keep things within 80 characters (even with careful use of wrapping), consider breaking your function up into smaller parts.

</details>


## [PROPOSAL] Avoid Variable-Length Indentation

> *Why* 
>
> Haskellers have a tendency to over-align everything vertically for the sake of readability. In practice, this is much more of an habit than a real gain in readability. Aligning content based on a function name, variable name or record field tends to create unnecessary long diffs and needless conflicts in version control systems when making a change to add an argument, variable or parameters. Favoring new-line and fixed-length alignment plays nicer with version control.

Variables, arguments, fields and tokens in general shouldn't be aligned based on the length of a previous token. Rather, tokens should go over a new line and be indented one-level extra when it makes sense, or not be aligned at all. 

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


## [PROPOSAL] Stylish-Haskell is used to format grouped imports & language pragmas

> **Why**
> 
> It is rather annoying and time-consuming to align import ligns or statement
> as we code and it's much simpler to leave that to our editor. Yet, we do want 
> to enforce some common formatting such that everyone gets to be aligned (pun
> intended). 
>
> We can use Stylish-Haskell with various set of rules, yet, the same arguments 
> from 'Avoid Variable-Length Indentation' applies when it comes to automatic 
> formatting. Imports are a real pain with git and Haskell when they are vertically
> aligned based on the imported module's name. 

Contributors' editors should pick up and enforce the rules defined by the `.stylish-haskell.yaml`
configuration file at the root of the project. Also, in order to maximise readability, imports
should be grouped into two groups, separated by a blank newline. 

- Explicit imports
- Qualified imports

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


# Haskell Practices

## [PROPOSAL] Favor `newtype` and tagged type over type-aliases

> **Why**
>
> Type-aliases convey a false sense of type-safety. While they usually make things a bit better for the reader, they have a tendency to spread through the code-base transforming those sweet help spot into traps. We can't define proper instances on type aliases, and we treat them as different type whereas they are behind the scene, just another one. 

Instead of writing type aliases, one should favor wrapping up values in newtype when it makes sense, or, have them wrapped into a tagged type with a phantom type to convey some extra meaning while still preserving type safeness. By using newtypes, we actually extend our program vocabulary and increase its robustness.

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

## [PROPOSAL] Language extensions are specified on top of each module

> **Why**
>
> Having a lot of default extensions enabled across the whole project can sometimes lead to cryptic 
> errors where GHC would interpret things differently because of the enabled extensions. Yet, it's 
> sometimes hard to distinguish by simply looking at the module themselves. 
>
> Also, being more explicit on extensions used by a module can help speeding up compile-time of such simple modules
> that don't need to be pull in a lot of extra complexity. 

Haskell's language extension are specified on top of each module. 

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

## [PROPOSAL] We use `hlint` as a linter

> **Why**
> 
> Linters are common practices in software development and help maintaining consistency across a large codebase with
> many developers. Hlint is de de-facto linter in Haskell and comes with a lot of different rules and features that 
> are _most of the time_ rather relevant and convey good practices, agreed upon and shared across the team. 

Contributors' editors should pick up and enforce the rules defined by the .hlint.yaml configuration file at the root of the project. File should be committed without warnings or errors. When it make senses, developer may ignore lints at a function site using a proper annotation:

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


# Git Practices

## [PROPOSAL] PRs are squashed when merged

> **Why**
>
> When working on a task, a best practice is to commit often, and rethink after how we do want to indeed structure 
> the changes. In practice, we don't necessarily take the time to do such re-organization work and we may end up
> merging a lot of intermediate commits into the main trunk, sometimes touching a single file in many different ways.
> This creates noise on the git history as well as unnecessary hard conflict resolutions (because we now have to resolve conflicts for all those commits, some of them being in an intermediate stage of development). 

As we strive to give a clear structure to PR's git history, we should also squash commits from every PR while merging to make sure we do not end up "polluting" the git history by accident with many intermediate commits. When merging, we should also feel free to extend a merge commit message with any outcome of discussions that has occurred during the PR's review and that are worth recording. 
