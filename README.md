<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
# Table of Contents

- [Direct Offer](#direct-offer)
  - [Introduction](#introduction)
  - [Documentation](#documentation)
    - [What is P2P trading?](#what-is-peer-to-peer-p2p-trading)
    - [How can this project facilitate P2P trading?](#how-can-this-project-facilitate-p2p-trading)
    - [Details](#details)
      - [Basics of a single offer](#basics-of-a-single-offer)
      - [Multiple offer matching in a single transaction](#multiple-offer-matching-in-a-single-transaction)
  - [Getting Started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Building and Developing](#building-and-developing)
  - [License](#license)

<!-- markdown-toc end -->

# Direct Offer

## Introduction

The Direct Offer project provides a Plutarch-based implementation of a smart contract enabling peer-to-peer trading, in a trustless manner, for the Cardano blockchain. Without the need for a trusted third party or a Decentralized Exchange (DEX), a user can put up any Cardano native asset(s) for sale in exchange for any user-specified native asset(s).

This project is funded by the Cardano Treasury in [Catalyst Fund 10](https://projectcatalyst.io/funds/10/f10-developer-ecosystem-the-evolution/plug-and-play-smart-contract-api-a-game-changing-platform-to-deploy-open-source-contracts-instantly)

## Documentation

### What is Peer-to-Peer (P2P) trading?

P2P trading in the context of this project refers to the direct buying and selling of Cardano Native Tokens (both Fungible & Non-Fungible Tokens) among users, without a third party or an intermediary. This is unlike buying and selling digital assets using a Centralized Exchange (CEX), where you cannot transact directly with counterparties or a DEX where you trade against a fixed Liquidity Pool.

Trading on a CEX requires you to give custody of your tokens to them, so they can execute the trades you enter based on their charts and market order aggregators. A CEX provides access to their order book and facilitates trades and takes fees in exchange.

Depending on the type of order you use, effects such as slippage may mean you don’t get the exact price you want. P2P trading, on the other hand, gives you full control over pricing, settlement time, and whom you choose to sell to and buy from.

### How can this project facilitate P2P trading?

This project fulfills the cornerstone requirement of a trusted Escrow, over seeing the trade in the form of a smart contract. It locks the seller's assets in the contract until a buyer provides the required ask price or the seller wishes to cancel the offer and claim the funds back.

### Details

#### Basics of a single offer

A seller sends the assets he wishes to exchange, to the contract, thus locking the funds therein. This UTxO contains a datum which specifies the "creator" of the offer and the assets wished in return, "toBuy" indicates the price.
```
data PDirectOfferDatum (s :: S)
  = PDirectOfferDatum
      ( Term
          s
          ( PDataRecord
              '[ "creator" ':= PAddress
               , "toBuy" ':= PValue 'Sorted 'Positive
               ]
          )
      )
```

A buyer interested in the offered assets initiates a transaction (with a "PExecuteOffer" redeemer) spending the locked UTxO, along with sending the "toBuy" assets to the "creator". This condition is validated by the contract first before allowing the transaction to proceed further. However, if the seller decides to cancel the offer, he can do so by initiating a transaction (with a "PReclaim" redeemer) and claim all the locked assets back.
```
data PDirectOfferRedeemer (s :: S)
  = PExecuteOffer (Term s (PDataRecord '[]))
  | PReclaim (Term s (PDataRecord '[]))
```

One UTxO at the smart contract address translates to one sell order.

#### Multiple offer matching in a single transaction

A single transaction can fulfill multiple sell orders by matching it with valid buy orders. Whether all the buy orders in the tx are correct or not is validated only once at the tx level using the [Zero ADA Withdrawal Trick](https://github.com/cardano-foundation/CIPs/pull/418#issuecomment-1366605115) from the Staking validator.
```
directOfferGlobalLogic :: Term s PStakeValidator
```

This Staking validator's credential is used as a parameter to a Spending Validator (the smart contract which locks the seller UTxOs). Spending validator ensures that the Staking validator is executed in the tx. A successful validation from both spending and staking validator is essentail for the spending of seller UTxOs.
```
directOfferValidator :: Term s (PStakingCredential :--> PValidator)
```

For carrying out this validation, the Staking Validator requires a redeemer containing one-to-one correlation between script input UTxOs (seller UTxOs) and buy output UTxOs (sent to seller from buyer). This is provided via ordered lists of input/output indices of inputs/ouputs present in the Script Context.
```
data PGlobalRedeemer (s :: S)
  = PGlobalRedeemer
      ( Term
          s
          ( PDataRecord
              '[ "inputIdxs" ':= PBuiltinList (PAsData PInteger)
               , "outputIdxs" ':= PBuiltinList (PAsData PInteger)
               ]
          )
      )
```
For e.g.
```
Inputs     :  [saleOfferA, saleOfferC, buyerInput3, saleOfferB, buyerInput1, buyerInput2]
Outputs    :  [buyOfferA, buyOfferB, buyOfferC, buyerOuput1, buyerOutput2, buyerOutput3]
InputIdxs  :  [0, 1, 3]
OutputIdxs :  [0, 2, 1]
```

While its easy to understand and declare indices of outputs (the order in which outputs appear in the tx builder), we cannot control the order of inputs as seen by the script. As inputs are sorted lexicographically based on their output reference, first by Tx#Id and then by Tx#Idx.

Note: Staking validator is not needed to be invoked if the seller wishes to cancel the offer and reclaim his funds.

## Getting Started

### Prerequisites

Before you begin, ensure you have [Nix](https://nixos.org) installed on your system. Nix is used for package management and to provide a consistent development environment. If you don't have Nix installed, you can do so by running the following command:

#### Official option
[Nix](https://nixos.org/download.html)
```
sh <(curl -L https://nixos.org/nix/install) --daemon
```
#### Preferred option
[Determinate Systems](https://zero-to-nix.com/concepts/nix-installer)
```
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

### Building and developing

Once Nix is installed, you should be able to seamlessly use the repository to
develop, build and run packages.

Download the Git repository:

```sh
git clone https://github.com/Anastasia-Labs/direct-order-contracts.git
```

Navigate to the repository directory:

```sh
cd direct-order-contracts
```

Activate the development environment with Nix:

```sh
nix develop --accept-flake-config
```

Additionally, when you run `nix run .#help` you'll get a list of scripts you can run, the Github CI (nix flake check) is setup in a way where it checks the project builds successfully, haskell format is done correctly, and commit message follows conventional commits. Before pushing you should run `cabal run` , `nix run .#haskellFormat` (automatically formats all haskell files, including cabal), if you want to commit a correct format message you can run `cz commit`

Build:

```sh
cabal build
```

Execute the test suite:

```sh
cabal test
```

![direct-offer](/assets/gifs/direct-offer.gif)

## License

© 2023 Anastasia Labs.

All code is licensed under MIT License. See [LICENSE](./LICENSE) file
for details.