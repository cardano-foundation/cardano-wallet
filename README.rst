.. raw:: html

   <p align="center">
     <a href="https://coveralls.io/github/input-output-hk/cardano-wallet?branch=HEAD"><img src="https://img.shields.io/coveralls/github/input-output-hk/cardano-wallet/HEAD?style=for-the-badge" /></a>
     <a href='https://cardano.readthedocs.io/projects/cardano-wallet/en/latest/?badge=latest'><img src='https://readthedocs.org/projects/cardano-wallet/badge/?version=latest' alt='Documentation Status' /></a>
   </p>

Overview
--------

Cardano Wallet helps you manage your Ada. You can use it to send and
receive payments on the `Cardano`_ blockchain.

This project provides an HTTP Application Programming Interface (API)
and command-line interface (CLI) for working with your wallet.

It can be used as a component of a frontend such as `Daedalus`_, which
provides a friendly user interface for wallets. Most users who would
like to use Cardano should start with Daedalus.

.. note::
   This source code repository contains the next
   major version of Cardano Wallet, which has been completely rewritten
   for the `Shelley`_ phase.

.. warning::
   The Byron version of Cardano Wallet is in the `cardano-sl`_ repository.

Getting Started
---------------

.. code-block:: bash
   $ wget https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/docker-compose.yml
   $ NETWORK=testnet docker-compose up

Fantastic! The server is up-and-running, waiting for HTTP requests on localhost:8090/v2 e.g.:

.. code-block:: bash
   $ curl http://localhost:8090/v2/network/information

or to be accessed via CLI, e.g.:

.. code-block:: bash
   $ docker run --network host --rm inputoutput/cardano-wallet network information

See also `Wiki - Docker`_ for more information about using docker.

How to install (Linux / Windows / Mac OS)
-----------------------------------------

See **Installation Instructions** for each available `release`_.

   | cardano-wallet

.. _Cardano: https://www.cardano.org
.. _Daedalus: https://daedaluswallet.io
.. _Shelley: https://cardanoroadmap.com/
.. _cardano-sl: https://github.com/input-output-hk/cardano-sl
.. _Wiki - Docker: https://github.com/input-output-hk/cardano-wallet/wiki/Docker
.. _release: https://github.com/input-output-hk/cardano-wallet/releases