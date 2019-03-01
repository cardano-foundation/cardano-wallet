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

# Git Practices

## [PROPOSAL] Commits are squashed / re-formulated when merged from a PR to `master`

> **Why**
>
> When working on a task, a best practice is to commit often, and rethink after how we do want to indeed structure 
> the changes. In practice, we don't necessarily take the time to do such re-organization work and we may end up
> merging a lot of intermediate commits into the main trunk, sometimes touching a single file in many different ways.
> This creates noise on the git history as well as unnecessary hard conflict resolutions (because we now have to resolve conflicts for all those commits, some of them being in an intermediate stage of development). 

As we strive to give a clear structure to PR's git history, we should also squash commits from every PR while merging to make sure we do not end up "polluting" the git history by accident with many intermediate commits. 