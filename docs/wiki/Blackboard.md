# Forewords

This wiki page is a "blackboard" where anyone in the team can write ideas or suggestions about various things on the team, the project, the code or the development process. Give as much as details as you can such that, during release meeting, we can discuss what's on the board and improve. 

# Ideas

* [**Piotr**] Have stylish-haskell, hlint, weeder, and different types of tests (integration, unit) as separate checks on Travis CI for better readibility (as on previous repository). Currently they are all done on a single run.

* [**jbgi**] As a wallet user I want an option when sending a transaction to specify the change address. example: I have utxo with 100 Ada. I send 10 Ada to someone. Currently a new address is generated and 90 ADA goes into that address. But I want to be able to specify my-self the address that received the 90 ADA change.