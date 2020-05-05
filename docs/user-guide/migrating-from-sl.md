---
weight: 5
title: Migrating From SL
--- 

## I am using _explorer_ from _cardano-sl_, what should I do?

The API from the old explorer has been ported identically to _cardano-submit-api_. This component is part of [cardano-rest]. Source code that is dealing with the _explorer_ API from _cardano-sl_ should be straightforward to migrate. See the installation instructions and documentation available on _cardano-rest_ for more details. 

The setup is here a bit different. With _cardano-sl_, the explorer is mounted directly on the core node as one monolith and can be turned on and off. Now, these components have been split off one another and are using an extra middleware to communicate. So the "infrastructure" is slightly more complex but enables greater flexibility and robustness. 

{{<hint info>}}
It is possible to automatically migrate an existing blockchain database from _cardano-sl_ into its new format compatible with _cardano-node_. For this, have a look at the [cardano-cli][cardano-cli] and in particular the `???` command.

```
$ cardano-cli ??? --help
```

This can save you an hour of time downloading the blockchain from the network!
{{</hint>}}


## I am using _wallet V1_ from _cardano-sl_, what should I do?

A new `V2` API is now available on [cardano-wallet][cardano-wallet]. In a similar fashion to _cardano-rest_, this component used to be mounted directly on the core node but is now an independent process. The _cardano-wallet_ is nothing more than a webserver that connects to a local core node through a domain socket. 

There are some variations between the `V2` and `V1` APIs of course, but they follow a very similar approach and are still very resource-centric (a.k.a ReST). _cardano-wallet_ is however a quite large component, that covers multiple networks and node backends. If you're coming from sl, you're most likely interested in _cardano-wallet-byron_ at the moment which is integrated with a Byron-reboot OBFT _cardano-node_. Follow setup instructions on _cardano-wallet_'s README and Wiki.

The API documentation is available in [API References]({{< ref "api-references.md" >}}). Note that only the _Legacy_ sub-part of the API matters at this stage. The _Shelley_ part is not available on the Byron integration but can be looked up as a reference for future integration.

{{<hint warning>}}
_cardano-sl_ had the concept of "accounts" inside wallets. This concepts is now entirely gone. If you need multiple accounts, use multiple wallets.
{{</hint>}}

## I am using _wallet V0_ from _cardano-sl_, what should I do?

You are going to have a harder time migrating to _cardano-wallet_. The gaps between `V0` and `V2` are obviously bigger, but everything from the previous section applies. **Good luck**.

---

<p style="text-align: right">
  More questions? Have a look at the <a href="{{< ref "faq.md" >}}">FAQ</a> or else, reach out on <a href="https://github.com/input-output-hk/adrestia/issues/new/choose">Github</a>!
</p>

[cardano-rest]: https://github.com/input-output-hk/cardano-rest
[cardano-wallet]: https://github.com/input-output-hk/cardano-wallet
[cardano-cli]: https://github.com/input-output-hk/cardano-node/blob/1.11.0/README.md#cardano-cli
