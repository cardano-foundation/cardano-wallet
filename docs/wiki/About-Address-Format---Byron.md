# About Address Format - Byron

```
+--------------------------------------------------------------------------------+
|                                                                                |
|                        CBOR-Serialized Object with CRC¹                        |
|                                                                                |
+--------------------------------------------------------------------------------+
                                        |
                                        |
                                        v
+--------------------------------------------------------------------------------+
|     Address Root    |     Address Attributes    |           AddrType           |
|                     |                           |                              |
|   Hash (224 bits)   |  Der. Path² + Stake + NM  |  PubKey | (Script) | Redeem  | 
|                     |    (open for extension)   |     (open for extension)     |
+--------------------------------------------------------------------------------+
             |                 |                                           
             |                 |     +----------------------------------+   
             v                 |     |        Derivation Path           |   
+---------------------------+  |---->|                                  |
| SHA3-256                  |  |     | ChaChaPoly⁴ AccountIx/AddressIx  |
|   |> Blake2b 224          |  |     +----------------------------------+
|   |> CBOR                 |  |        
|                           |  |        
|  -AddrType                |  |     +----------------------------------+
|  -ASD³ (~AddrType+PubKey) |  |     |       Stake Distribution         |   
|  -Address Attributes      |  |     |                                  |   
+---------------------------+  |---->|  BootstrapEra | (Single | Multi) |   
                               |     +----------------------------------+   
                               |
                               |                                          
                               |     +----------------------------------+   
                               |     |          Network Magic           |   
                               |---->|                                  |   
                                     | Addr Discr: MainNet vs TestNet   |   
                                     +----------------------------------+   

```
<p align="center"><small>Fig 1. Byron Address Internals</small></p>


1. CRC: [Cyclic Redundancy Check](https://computer.howstuffworks.com/encryption7.htm);
  sort of checksum, a bit (pun intended) more reliable.

2. ASD: Address Spending Data; Some data that are bound to an address. It's
  an extensible object with payload which identifies one of the three elements:  
    - A Public Key (Payload is thereby a PublicKey)  
    - A Script (Payload is thereby a script and its version)  
    - A Redeem Key (Payload is thereby a RedeemPublicKey)  

3. Derivation Path: Note that there's no derivation path for Redeem nor
  Scripts addresses!

4. ChaChaPoly: Authenticated Encryption with Associated Data; See [RFC
  7539](https://datatracker.ietf.org/doc/rfc7539) We use it as a way to cipher
  the derivation path using a passphrase (the root public key).

## Example 1: Yoroi Address - Byron Mainnet

Let's take an arbitrary Yoroi base58-encoded address of the Byron mainNet:

```
Ae2tdPwUPEZFRbyhz3cpfC2CumGzNkFBN2L42rcUc2yjQpEkxDbkPodpMAi
```

Now, this address could be represented as a raw bytestring by decoding from
base58:

```
0X82 0XD8 0X18 0X58 0X21 0X83 0X58 0X1C 0XBA 0X97 0X0A 0XD3 0X66 0X54
0XD8 0XDD 0X8F 0X74 0X27 0X4B 0X73 0X34 0X52 0XDD 0XEA 0XB9 0XA6 0X2A
0X39 0X77 0X46 0XBE 0X3C 0X42 0XCC 0XDD 0XA0 0X00 0X1A 0X90 0X26 0XDA
0X5B
```

In this representation, bytes are in a structured format called [CBOR](https://tools.ietf.org/html/rfc7049).
Some bytes are actually tags with carry a particular semantic, and some are values.
We can re-shuffle the bytes as follows to make things a bit clearer:

```
82                        # array (2)            
   D8 18                  # tag (24)             [CBOR Metadata]
      58 21 (8358...A000) # bytes (33)           [Address Payload]
   1A 9026DA5B            # unsigned(2418465371) [CRC]
```

So, a Byron address is basically formed of two top-level elements:

- A tagged bytestring; `24` means that the bytes represent another CBOR-encoded structure.
- A CRC of the inner tagged bytestring

Now, if we also interpret the inner bytestring as a CBOR structure, we obtain:

```
83                        # array(3)       
   58 1C (BA97...CCDD)    # bytes(28)      [Address Root]
   A0                     # map(0)         [Address Attributes]
   00                     # unsigned(0)    [Address Type]
```

An address type of `0` refers to a spending address for which the address root
contains a hash of a public spending key. This address payload has no attribute
for the initial address was a Yoroi's address on MainNet which follows a BIP-44
derivation scheme and therefore, does not require any attributes. 

## Example 2: Daedalus Address - Byron TestNet

Let's take an arbitrary Daedalus base58-encoded address of a Byron testNet:

```
37btjrVyb4KEB2STADSsj3MYSAdj52X5FrFWpw2r7Wmj2GDzXjFRsHWuZqrw7zSkwopv8Ci3VWeg6bisU9dgJxW5hb2MZYeduNKbQJrqz3zVBsu9nT
```

Now, this address could be represented as a raw bytestring by decoding from
base58:

```
0x82 0xd8 0x18 0x58 0x49 0x83 0x58 0x1c 0x9c 0x70 0x85 0x38 0xa7 0x63 0xff 0x27 
0x16 0x99 0x87 0xa4 0x89 0xe3 0x50 0x57 0xef 0x3c 0xd3 0x77 0x8c 0x05 0xe9 0x6f 
0x7b 0xa9 0x45 0x0e 0xa2 0x01 0x58 0x1e 0x58 0x1c 0x9c 0x17 0x22 0xf7 0xe4 0x46 
0x68 0x92 0x56 0xe1 0xa3 0x02 0x60 0xf3 0x51 0x0d 0x55 0x8d 0x99 0xd0 0xc3 0x91 
0xf2 0xba 0x89 0xcb 0x69 0x77 0x02 0x45 0x1a 0x41 0x70 0xcb 0x17 0x00 0x1a 0x69 
0x79 0x12 0x6c
```

In this representation, bytes are in a structured format called [CBOR](https://tools.ietf.org/html/rfc7049).
Some bytes are actually tags with carry a particular semantic, and some are values.
We can re-shuffle the bytes as follows to make things a bit clearer:

```
82                         # array(2)              
   D8 18                   # tag(24)               [CBOR Metadata]
      58 49 (8358...1700)  # bytes(73)             [Address Payload]
   1A 6979126C             # unsigned(1769542252)  [CRC]
```

So, a Byron address is basically formed of two top-level elements:

- A tagged bytestring; `24` means that the bytes represent another CBOR-encoded structure.
- A CRC of the inner tagged bytestring

Now, if we also interpret the inner bytestring as a CBOR structure, we obtain:

```
83                                      # array(3)
   58 1C (9C70...450E)                  # bytes(28)    [Address Root]
   A2                                   # map(2)       [Address Attributes]
      01                                # unsigned(1)  [Derivation Path Attribute]
      58 1E (581C...6977)               # bytes(30)    [Derivation Path Value] 
      02                                # unsigned(2)  [Network Magic Attribute]
      45 (1A4170CB17)                   # bytes(5)     [Network Magic Value]
   00                                   # unsigned(0)  [Address Type]
```

An address type of `0` refers to a spending address for which the address root
contains a hash of a public spending key. In addition, we can see that this
address has 2 attributes identified by two tags `01` for the derivation path,
and `02` for the network magic. The derivation path is an encrypted bytestring
which holds two derivation indexes for the account and address paths. 
