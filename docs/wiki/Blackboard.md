# Forewords

This wiki page is a "blackboard" where anyone in the team can write ideas or suggestions about various things on the team, the project, the code or the development process. Give as many details as you can so that, during release meeting, we can discuss what's on the board and improve. 

# Ideas

* [**Piotr**] Have stylish-haskell, hlint, weeder, and different types of tests (integration, unit) as separate checks on Travis CI for better readability (as on previous repository). Currently they are all done on a single run.

* [**jbgi**] As a wallet user I want an option when sending a transaction to specify the change address. example: I have utxo with 100 Ada. I send 10 Ada to someone. Currently a new address is generated and 90 ADA goes into that address. But I want to be able to specify myself the address that received the 90 ADA change.

* [**KtorZ**] We have hard-coded a couple of times the `k=2160` parameter in the code base. In practice, this one can change through protocol update and this is something we would probably want to track at runtime in some sort of context, might it be a hard-coded context for now -- but at least, we would have the necessary mechanism in place. 

* [**anviking**] Should we have state-machine tests? That seems nice to have sooner rather than later, such that the code we write is designed to be tested. Personally, I have practically no experience writing any, and wonder if I would write code differently if we had state machine tests.

    * I'd be interested to know what happened with the abandoned https://github.com/input-output-hk/cardano-sl/pull/3772, and what lessons were learned

    * We should probably start small

    * This bug https://github.com/input-output-hk/cardano-wallet-legacy/issues/353, which first appeared as mysterious , random failures in the integration tests, would have been much easier to discover if we could generate a sequence of `Wait Time | SendHTTPRequest`, execute, and shrink, with the only conditions that the wallet and node stays running. Maybe this could have been adapted easily from the existing integration tests.

        * A key step of isolating the bug was just to introduce a waiting-time in an integration test. The next test would then fail.

* [**KtorZ**] Our README is quite poor in information and it would be worth adding some basics stuff to it. That's a point Johannes already raised on the legacy repository actually. We could add at least:
    - Basic Overview / Goal 
    - Build & Tests instructions
    - A link to the wiki
    - Link to the generated Haddock documentation
    - Link to the API documentation

* [**KtorZ**] We've completely forgotten about maintaining a CHANGELOG :no_mouth: 