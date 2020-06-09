 -           

    wallet -           
    Manage wallets.
        list -           List all known wallets.
        create -           
        Create a new Byron wallet.
            from-mnemonic -           Create a new wallet using amnemonic.
        get -           Fetch the wallet with specifiedid.
        update -           
        Update a wallet.
            name -           Update a wallet's name.
            passphrase -           Update a wallet's passphrase.
        delete -           Deletes wallet with specifiedwallet id.
        utxo -           Get UTxO statistics for thewallet with specified id.
    key -           
    Derive and manipulate keys.
        root -           Extract root extended privatekey from a mnemonic sentence.
        child -           Derive child keys.
        public -           Extract the public key from aprivate key.
        inspect -           Show information about a key.
    mnemonic -           
    Manage mnemonic phrases.
        generate -           Generate English BIP-0039compatible mnemonic words.
        reward-credentials -           Derive reward account privatekey from a given mnemonic.
    network -           
    Manage network.
        information -           View network information.
        parameters -           View network parameters for thecurrent epoch.
        clock -           View NTP offset.
