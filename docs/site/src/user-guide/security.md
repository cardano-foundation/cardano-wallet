## Wallet security considerations

The cardano-wallet HTTP service is designed to be used by trusted users only. _Any other use is not supported or planned_ .

In order to ensure that only trusted users may access the HTTP service, cardano-wallet uses TLS client certificate authentication. For example, this is how the Daedalus wallet frontend ensures that only this frontend can access the cardano-wallet API. In other words, trust is established through a TLS client certificate. Such certificates need to be placed in the disk storage used by the cardano-wallet process before the HTTP service is started.

It’s worth mentioning that a trusted user can attack the wallet through the HTTP service in many ways, they can also view sensitive information, delete a wallet’s store, etc. Thus, as soon as an attacker is able to become a trusted user.

It’s also worth mentioning that a trusted user that can access the HTTP API is not able to spend funds of the wallet without gaining access to additional information such as the passphrase or the wallet secret key. TLS prevents eavesdropping on the passphrase, and the wallet secret key is encrypted by the passphrase.