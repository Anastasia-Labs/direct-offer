module Spec.StakingValidatorSpec (
  creatorPKH,
  sampleTest,
  goodCtx1,
  sampleTestEval,
)
where

import Order (
  PDirectOfferDatum (..),
  PGlobalRedeemer (..),
  PSmartHandleRedeemer (..),
  directOrderGlobalLogic,
 )
import Plutarch.Api.V1.Value (padaSymbol, padaToken, pconstantPositiveSingleton)
import Plutarch.Builtin (pdataImpl)
import Plutarch.Prelude
import Plutarch.Test.Precompiled (
  Expectation (..),
  testEvalCase,
  tryFromPTerm,
 )
import PlutusLedgerApi.V1.Address (Address, pubKeyHashAddress)
import PlutusLedgerApi.V2 (
  Credential (..),
  CurrencySymbol (..),
  PubKeyHash,
  ScriptContext (..),
  ScriptHash (..),
  StakingCredential (..),
  TokenName (..),
  adaSymbol,
  adaToken,
  singleton,
 )
import PlutusTx qualified
import Test.Tasty (TestTree)
import "plutarch-context-builder" Plutarch.Context (
  Builder,
  RewardingBuilder,
  -- address,
  buildRewarding',
  input,
  output,
  pubKey,
  script,
  signedWith,
  txId,
  withDatum,
  withRedeemer,
  withRefIndex,
  withRefTxId,
  withRewarding,
  withValue,
  withdrawal,
 )

sampleScriptHash1 :: ScriptHash
sampleScriptHash1 = "395e0b6c308dbdfd6e41354b68f833b96990ecd93721699ed90a2113"

sampleScriptHash2 :: ScriptHash
sampleScriptHash2 = "22e380f1157b688ac08f26a64e046b8b85632ba47c664c8f924b777e"

sampleStakingCredential1 :: StakingCredential
sampleStakingCredential1 = StakingHash $ ScriptCredential sampleScriptHash1

sellTokenName1 :: TokenName
sellTokenName1 = "usd"

sellCurrencySymbol1 :: CurrencySymbol
sellCurrencySymbol1 = "746fa3ba2daded6ab9ccc1e39d3835aa1dfcb9b5a54acc2ebe6b79a4"

sellTokenName2 :: TokenName
sellTokenName2 = "btc"

sellCurrencySymbol2 :: CurrencySymbol
sellCurrencySymbol2 = "776fa3ba2daded6ab9ccc1e39d3835aa1dfcb9b5a54acc2ebe6b79a4"

creatorPKH :: PubKeyHash
creatorPKH = "0d342d962a7aaac57e30d3f8dd2f41907a361860f8889253ebe40bbb"

creatorAddress :: Address
creatorAddress = pubKeyHashAddress creatorPKH

buyerPKH :: PubKeyHash
buyerPKH = "ea2484f839e72f5bd60e004e74b564bb75f79a980b22c55d88f4b8bb"

datum1 :: Term s PDirectOfferDatum
datum1 = pcon $ PDirectOfferDatum $ pdcons @"creator" # pdata (pconstant creatorAddress) #$ pdcons @"toBuy" # pdata (pconstantPositiveSingleton padaSymbol padaToken 10_000_000) # pdnil

executeRdmr :: Term s PSmartHandleRedeemer
executeRdmr = pcon $ PExecuteOrder pdnil

inputIdxs1 :: Term s (PBuiltinList (PAsData PInteger))
inputIdxs1 = pcons # (pdata 0) #$ pcons # (pdata 1) # pnil

outputIdxs1 :: Term s (PBuiltinList (PAsData PInteger))
outputIdxs1 = pcons # (pdata 0) #$ pcons # (pdata 1) # pnil

inputIdxs2 :: Term s (PBuiltinList (PAsData PInteger))
inputIdxs2 = pcons # (pdata 0) #$ pcons # (pdata 1) #$ pcons # (pdata 0) # pnil

outputIdxs2 :: Term s (PBuiltinList (PAsData PInteger))
outputIdxs2 = pcons # (pdata 0) #$ pcons # (pdata 1) #$ pcons # (pdata 0) # pnil

inputIdxs3 :: Term s (PBuiltinList (PAsData PInteger))
inputIdxs3 = pcons # (pdata 0) #$ pcons # (pdata 2) # pnil

globalRdmr1 :: Term s PGlobalRedeemer
globalRdmr1 = pcon $ PGlobalRedeemer $ pdcons @"inputIdxs" # pdata inputIdxs1 #$ pdcons @"outputIdxs" # pdata outputIdxs1 # pdnil

globalRdmr2 :: Term s PGlobalRedeemer
globalRdmr2 = pcon $ PGlobalRedeemer $ pdcons @"inputIdxs" # pdata inputIdxs2 #$ pdcons @"outputIdxs" # pdata outputIdxs2 # pdnil

globalRdmr3 :: Term s PGlobalRedeemer
globalRdmr3 = pcon $ PGlobalRedeemer $ pdcons @"inputIdxs" # pdata inputIdxs3 #$ pdcons @"outputIdxs" # pdata outputIdxs1 # pdnil

inputScript1 :: (Builder a) => a
inputScript1 =
  input $
    mconcat
      [ script sampleScriptHash2
      , withValue (singleton sellCurrencySymbol1 sellTokenName1 1)
      , withRefTxId "759d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
      , withRefIndex 1
      , withRedeemer (plift $ pdataImpl executeRdmr)
      , withDatum (plift $ pdataImpl datum1)
      ]

inputScript2 :: (Builder a) => a
inputScript2 =
  input $
    mconcat
      [ script sampleScriptHash2
      , withValue (singleton sellCurrencySymbol2 sellTokenName2 1)
      , withRefTxId "999d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
      , withRefIndex 1
      , withRedeemer (plift $ pdataImpl executeRdmr)
      , withDatum (plift $ pdataImpl datum1)
      ]

inputScript3 :: (Builder a) => a
inputScript3 =
  input $
    mconcat
      [ script sampleScriptHash2
      , withValue (singleton sellCurrencySymbol2 sellTokenName2 1)
      , withRefTxId "399d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
      , withRefIndex 1
      , withRedeemer (plift $ pdataImpl executeRdmr)
      , withDatum (plift $ pdataImpl datum1)
      ]

inputBuyer :: (Builder a) => a
inputBuyer =
  input $
    mconcat
      [ pubKey buyerPKH
      , withValue (singleton adaSymbol adaToken 20_000_000)
      , withRefTxId "fbd0e00b83475fb00f9a9cf5a0c7015ded973f07e55d8c68ec5ae40663d5e042"
      , withRefIndex 2
      ]

outputCreator :: (Builder a) => a
outputCreator =
  output $
    mconcat
      [ pubKey creatorPKH
      , withValue (singleton adaSymbol adaToken 10_000_000)
      ]

outputCreatorInsufficient :: (Builder a) => a
outputCreatorInsufficient =
  output $
    mconcat
      [ pubKey creatorPKH
      , withValue (singleton adaSymbol adaToken 5_000_000)
      ]

outputBuyer1 :: (Builder a) => a
outputBuyer1 =
  output $
    mconcat
      [ pubKey buyerPKH
      , withValue (singleton sellCurrencySymbol1 sellTokenName1 1)
      ]

outputBuyer2 :: (Builder a) => a
outputBuyer2 =
  output $
    mconcat
      [ pubKey buyerPKH
      , withValue (singleton sellCurrencySymbol2 sellTokenName2 1)
      ]

commonPurpose :: RewardingBuilder
commonPurpose = withRewarding sampleStakingCredential1

-- Execute Order
goodCtx1 :: ScriptContext
goodCtx1 =
  buildRewarding' $
    mconcat
      [ withdrawal sampleStakingCredential1 0
      , inputScript1
      , inputScript2
      , inputBuyer
      , outputCreator
      , outputCreator
      , outputBuyer1
      , outputBuyer2
      , signedWith buyerPKH
      , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
      , commonPurpose
      ]

-- Double Satisfaction
badCtx1 :: ScriptContext
badCtx1 =
  buildRewarding' $
    mconcat
      [ withdrawal sampleStakingCredential1 0
      , inputScript1
      , inputScript2
      , inputScript3
      , inputBuyer
      , outputCreator
      , outputCreator
      , outputBuyer1
      , outputBuyer2
      , outputBuyer2
      , signedWith buyerPKH
      , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
      , commonPurpose
      ]

-- Insufficient ask
badCtx2 :: ScriptContext
badCtx2 =
  buildRewarding' $
    mconcat
      [ withdrawal sampleStakingCredential1 0
      , inputScript1
      , inputScript2
      , inputBuyer
      , outputCreator
      , outputCreatorInsufficient
      , outputBuyer1
      , outputBuyer2
      , signedWith buyerPKH
      , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
      , commonPurpose
      ]

-- Not a script input
badCtx3 :: ScriptContext
badCtx3 =
  buildRewarding' $
    mconcat
      [ withdrawal sampleStakingCredential1 0
      , inputScript1
      , inputScript2
      , inputBuyer
      , outputCreator
      , outputCreatorInsufficient
      , outputBuyer1
      , outputBuyer2
      , signedWith buyerPKH
      , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
      , commonPurpose
      ]

sampleTest :: TestTree
sampleTest = tryFromPTerm "Test Order" directOrderGlobalLogic $ do
  testEvalCase
    "Pass - Execute 2 orders"
    Success
    [ plift $ pdataImpl globalRdmr1 -- Redeemer Unit
    , PlutusTx.toData goodCtx1 -- ScriptContext
    ]
  testEvalCase
    "Fail - Double Satisfaction"
    Failure
    [ plift $ pdataImpl globalRdmr2 -- Redeemer Unit
    , PlutusTx.toData badCtx1 -- ScriptContext
    ]
  testEvalCase
    "Fail - Insufficient ask"
    Failure
    [ plift $ pdataImpl globalRdmr1 -- Redeemer Unit
    , PlutusTx.toData badCtx2 -- ScriptContext
    ]
  testEvalCase
    "Fail - Not a script input"
    Failure
    [ plift $ pdataImpl globalRdmr3 -- Redeemer Unit
    , PlutusTx.toData badCtx3 -- ScriptContext
    ]

sampleTestEval :: Term s POpaque
sampleTestEval =
  directOrderGlobalLogic
    # (pdataImpl globalRdmr1)
    # (pconstant goodCtx1)
