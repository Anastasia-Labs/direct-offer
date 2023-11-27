module Spec.SpendingValidatorSpec (
  sampleTest,
  creatorPKH,
  goodCtx1,
  sampleTestEval,
)
where

import Order (
  directOrderValidator,
  PDirectOfferDatum (..),
  PSmartHandleRedeemer (..),
 )
import Plutarch.Prelude
import Plutarch.Builtin (pdataImpl)
import Plutarch.Test.Precompiled (
  Expectation (..),
  testEvalCase,
  tryFromPTerm,
 )
import Plutarch.Api.V1.Value (pconstantPositiveSingleton, padaSymbol, padaToken)
import PlutusLedgerApi.V1.Address (pubKeyHashAddress, Address)
import PlutusLedgerApi.V2 (
  CurrencySymbol (..),
  PubKeyHash,
  ScriptContext (..),
  ScriptHash (..),
  StakingCredential (..),
  Credential (..),
  TokenName (..),
  adaSymbol,
  adaToken,
  singleton,
 )
import PlutusTx qualified
import Test.Tasty (TestTree)
import "plutarch-context-builder" Plutarch.Context (
  Builder,
  SpendingBuilder,
  buildSpending,
  checkPhase1,
  input,
  output,
  pubKey,
  script,
  signedWith,
  txId,
  withdrawal,
  withDatum,
  withRedeemer,
  withRefIndex,
  withRefTxId,
  withSpendingOutRefIdx,
  withValue,
 )

sampleScriptHash1 :: ScriptHash
sampleScriptHash1 = "395e0b6c308dbdfd6e41354b68f833b96990ecd93721699ed90a2113"

sampleScriptHash2 :: ScriptHash
sampleScriptHash2 = "22e380f1157b688ac08f26a64e046b8b85632ba47c664c8f924b777e"

sampleStakingCredential1 :: StakingCredential
sampleStakingCredential1 = StakingHash $ ScriptCredential sampleScriptHash1

sampleStakingCredential2 :: StakingCredential
sampleStakingCredential2 = StakingHash $ ScriptCredential sampleScriptHash2

sellTokenName :: TokenName
sellTokenName = "usd"

sellCurrencySymbol :: CurrencySymbol
sellCurrencySymbol = "746fa3ba2daded6ab9ccc1e39d3835aa1dfcb9b5a54acc2ebe6b79a4"

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

reclaimRdmr :: Term s PSmartHandleRedeemer
reclaimRdmr = pcon $ PReclaim pdnil

inputScript1 :: (Builder a) => a
inputScript1 =
  input $
    mconcat
      [ script sampleScriptHash2
      , withValue (singleton sellCurrencySymbol sellTokenName 1)
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
      , withValue (singleton sellCurrencySymbol sellTokenName 1)
      , withRefTxId "759d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
      , withRefIndex 1
      , withRedeemer (plift $ pdataImpl reclaimRdmr)
      , withDatum (plift $ pdataImpl datum1)
      ]

inputBuyer :: (Builder a) => a
inputBuyer =
  input $
    mconcat
      [ pubKey buyerPKH
      , withValue (singleton adaSymbol adaToken 10_000_000)
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

outputBuyer :: (Builder a) => a
outputBuyer =
  output $
    mconcat
      [ pubKey buyerPKH
      , withValue (singleton sellCurrencySymbol sellTokenName 1)
      ]

commonPurpose :: SpendingBuilder
commonPurpose = withSpendingOutRefIdx 1

-- Execute Order
goodCtx1 :: ScriptContext
goodCtx1 = 
  buildSpending checkPhase1 $
    mconcat
      [ withdrawal sampleStakingCredential1 0
      , inputScript1
      , inputBuyer
      , outputCreator
      , outputBuyer
      , signedWith buyerPKH
      , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
      , commonPurpose
      ]

-- Reclaim Order
goodCtx2 :: ScriptContext
goodCtx2 = 
  buildSpending checkPhase1 $
    mconcat
      [ inputScript2
      , outputPubKey
      , signedWith creatorPKH
      , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
      , commonPurpose
      ]
  where
    outputPubKey :: (Builder a) => a
    outputPubKey =
      output $
        mconcat
          [ pubKey creatorPKH
          , withValue (singleton sellCurrencySymbol sellTokenName 1)
          ]


-- Incorrect Staking Script Withdrawal
badCtx1 :: ScriptContext
badCtx1 = 
  buildSpending checkPhase1 $
    mconcat
      [ withdrawal sampleStakingCredential2 0
      , inputScript1
      , inputBuyer
      , outputCreator
      , outputBuyer
      , signedWith buyerPKH
      , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
      , commonPurpose
      ]

-- Invalid Reclaim, Missing Creator Signrature 
badCtx2 :: ScriptContext
badCtx2 = 
  buildSpending checkPhase1 $
    mconcat
      [ inputScript2
      , outputBuyer
      , signedWith buyerPKH
      , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
      , commonPurpose
      ]

sampleTest :: TestTree
sampleTest = tryFromPTerm "Test Order" (directOrderValidator # pconstant sampleStakingCredential1) $ do
  testEvalCase
    "Pass - Execute order"
    Success
    [ plift $ pdataImpl datum1 -- Datum Unit
    , plift $ pdataImpl executeRdmr -- Redeemer Unit
    , PlutusTx.toData goodCtx1 -- ScriptContext
    ]
  testEvalCase
    "Pass - Reclaim order"
    Success
    [ plift $ pdataImpl datum1 -- Datum Unit
    , plift $ pdataImpl reclaimRdmr -- Redeemer Unit
    , PlutusTx.toData goodCtx2 -- ScriptContext
    ]
  testEvalCase
    "Fail - Incorrect Staking Script Withdrawal"
    Failure
    [ plift $ pdataImpl datum1 -- Datum Unit
    , plift $ pdataImpl executeRdmr -- Redeemer Unit
    , PlutusTx.toData badCtx1 -- ScriptContext
    ]
  testEvalCase
    "Fail - Invalid Reclaim, Missing Creator Signrature"
    Failure
    [ plift $ pdataImpl datum1 -- Datum Unit
    , plift $ pdataImpl reclaimRdmr -- Redeemer Unit
    , PlutusTx.toData badCtx2 -- ScriptContext
    ]

sampleTestEval :: Term s POpaque
sampleTestEval =
  directOrderValidator
    # (pconstant sampleStakingCredential1)
    # (pdataImpl datum1)
    # (pdataImpl executeRdmr)
    # (pconstant goodCtx1)
