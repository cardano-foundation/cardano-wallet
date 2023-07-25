# Recovery Phrases

Recovery phrases (also known as _mnemonics_) provide a way for humans to easily read and write down arbitrary large numbers which would otherwise be very difficult to remember.

They use a predefined dictionary of simple words (available in many different languages) which map uniquely back and forth to a binary code.

## Seeds / Entropy

The recovery phrase is normally used to encode a wallet's "seed" - a secret random number which is 28 decimal digits long, or more!

A seed is also called just _entropy[^entropy]_, implying that it is a sequence of bytes which has been generated using high-quality randomness methods.

All keys belonging to a wallet
[address-derivation](address-derivation.md#address-derivation)
from the wallet seed.

[^entropy]: See also the definition of "entropy" from
[information theory](https://en.wikipedia.org/wiki/Information_theory).

## Encoding

The process for encoding recovery phrases is described in
[BIP-0039 § _Generating the mnemonic_](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki#Generating_the_mnemonic).
Below is a reformulation of this specification.

The allowed size of _entropy_ is 96-256 bits and must be multiple of 32 bits (4 bytes).

A checksum is appended to the initial entropy by taking the first $|ent| / 32$ bits of the SHA-256 hash of it, where $|ent|$ designates the _entropy_ size in bits.

Then, the concatenated result is split into groups of 11 bits, each encoding a number from 0 to 2047 serving as an index into a known dictionary (see below).

| Sentence Length | Entropy Size        | Checksum Size |
| ------------    | ------------------- | ------        |
| 9 words         | 96  bits (12 bytes) | 3 bits        |
| 12 words        | 128 bits (16 bytes) | 4 bits        |
| 15 words        | 160 bits (20 bytes) | 5 bits        |
| 18 words        | 192 bits (24 bytes) | 6 bits        |
| 21 words        | 224 bits (28 bytes) | 7 bits        |
| 24 words        | 256 bits (32 bytes) | 8 bits        |

## Dictionaries

Cardano uses the same dictionaries as defined in [BIP-0039](https://github.com/bitcoin/bips/blob/master/bip-0039/bip-0039-wordlists.md).

## Example

This is an English recovery phrase, ordered left-to-right, then top-to-bottom.

```
write       maid        rib
female      drama       awake
release     inhale      weapon
crush       mule        jump
sound       erupt       stereo
```

It is 15 words long, so $15\times11 = 165$ bits of information, which is split into a 160 bit seed and 5 bit checksum.

Using the dictionary, these words resolve to:

```
2036        1072        1479
 679         529         129
1449         925        1986
 424        1162         967
1662         615        1708
```

Which is:
```
Seed:
01111100111 11100100110 01111101011
01010100111 01000010001 00010000001
10110101001 01110011101 11111000010
00110101000 10010001010 01111000111
11001111110 01001100111 110101
Checksum:                     01100

Seed (base16):     fe90c2e3aa7422206d4b9df846a2453c7cfc99f5
Checksum (base16): 0c
```
