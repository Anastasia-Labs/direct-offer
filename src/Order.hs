{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Order (
  directOrderValidator,
  directOrderGlobalLogic,
  PDirectOfferDatum (..),
  PSmartHandleRedeemer (..),
  PGlobalRedeemer (..),
)
where

import Conversions (pconvert)
import Plutarch
import Plutarch.Api.V1.Address (PCredential (PPubKeyCredential))
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Api.V1.Value (passertPositive, passertSorted)
import Plutarch.Api.V2
import Plutarch.DataRepr (PDataFields)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe
import PlutusLedgerApi.V1
import Utils (pand'List, pcond, pcountScriptInputs, presolveHashByDatum, ptryOwnInput, (#>), (#>=))
import "liqwid-plutarch-extra" Plutarch.Extra.Numeric ((#^))
import "liqwid-plutarch-extra" Plutarch.Extra.Rational ((#%))
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext (pisScriptAddress)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont

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
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PDirectOfferDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PDirectOfferDatum

data PSmartHandleRedeemer (s :: S)
  = PExecuteOrder (Term s (PDataRecord '[]))
  | PReclaim (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PSmartHandleRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PSmartHandleRedeemer

directOrderValidator :: Term s (PStakingCredential :--> PValidator)
directOrderValidator = phoistAcyclic $ plam $ \stakeCred dat redeemer ctx -> P.do
  let redeemer' = pconvert @PSmartHandleRedeemer redeemer
      dat' = pconvert @PDirectOfferDatum dat
  ctxF <- pletFields @'["txInfo"] ctx
  infoF <- pletFields @'["wdrl", "signatories"] ctxF.txInfo
  pmatch redeemer' $ \case
    PExecuteOrder _ ->
      pmatch (plookup # stakeCred # infoF.wdrl) $ \case
        PNothing -> perror
        PJust _ -> popaque $ pconstant ()
    PReclaim _ -> P.do
      datF <- pletFields @'["creator"] dat'
      PPubKeyCredential ((pfield @"_0" #) -> creatorPKH) <- pmatch (pfield @"credential" # datF.creator)
      pif (pelem # creatorPKH # infoF.signatories) (popaque $ pconstant ()) perror

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
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PGlobalRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PGlobalRedeemer

pfoldl2 ::
  (PListLike listA, PListLike listB, PElemConstraint listA a, PElemConstraint listB b) =>
  Term s ((acc :--> a :--> b :--> acc) :--> acc :--> listA a :--> listB b :--> acc)
pfoldl2 =
  phoistAcyclic $ plam $ \func ->
    pfix #$ plam $ \self acc la lb ->
      pelimList
        ( \a as ->
            pelimList
              (\b bs -> self # (func # acc # a # b) # as # bs)
              perror
              lb
        )
        (pif (pnull # lb) acc perror)
        la

pfoldTxUTxOs ::
  Term s (PMap 'Unsorted PDatumHash PDatum) ->
  Term s PInteger ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PBuiltinList PTxOut) ->
  Term s PInteger
pfoldTxUTxOs datums acc la lb =
  pfoldl2
    # plam
      ( \state utxoIn utxoOut ->
          porderSuccessor datums state utxoIn utxoOut
      )
    # acc
    # la
    # lb

porderSuccessor ::
  Term s (PMap 'Unsorted PDatumHash PDatum) ->
  Term s PInteger ->
  Term s PTxOut ->
  Term s PTxOut ->
  Term s PInteger
porderSuccessor datums foldCount orderInput orderOutput = unTermCont $ do
  orderInputF <- pletFieldsC @'["address", "value", "datum"] orderInput
  orderOutputF <- pletFieldsC @'["address", "value", "datum"] orderOutput
  -- orderInput should contain a datum of the type PDirectOfferDatum
  let orderInputDatum =
        orderInputF.datum `pmatch` \case
          POutputDatum r -> (pfield @"outputDatum" # r)
          POutputDatumHash r -> presolveHashByDatum # (pfield @"datumHash" # r) # datums
          PNoOutputDatum _ -> ptraceError "No output datum"
  let inputDatum = pconvert @PDirectOfferDatum (pto orderInputDatum)

  pure $
    pif
      ( pand'List
          [ -- The address of all orderInput's must be a script address
            ptraceIfFalse "Not a script input" (pisScriptAddress # orderInputF.address)
          , -- The address of orderOutput must match orderInput.datum.creator
            ptraceIfFalse "Order not sent to creator" (orderOutputF.address #== pfield @"creator" # inputDatum)
          , -- The value of orderOutput must be equal to or greater than orderInput.datum.toBuy
            ptraceIfFalse "Order value less than expected" (passertPositive # (passertSorted # orderOutputF.value) #>= pfield @"toBuy" # inputDatum)
          ]
      )
      (foldCount + 1)
      perror

puniqueOrderedTxOuts :: Term s ((PInteger :--> PTxOut) :--> PInteger :--> (PBuiltinList (PAsData PInteger)) :--> (PBuiltinList PTxOut))
puniqueOrderedTxOuts =
  phoistAcyclic $
    let go :: Term s ((PInteger :--> PTxOut) :--> PInteger :--> (PBuiltinList (PAsData PInteger)) :--> (PBuiltinList PTxOut))
        go = plam $ \elemAt ->
          ( pfix #$ plam $ \self uniquenessLabel order ->
              pelimList
                ( \x xs ->
                    let n = 2 #^ (pfromData x)
                        n' = 2 * n
                        y = uniquenessLabel + n
                        output = elemAt # pfromData x
                     in pif
                          ((pmod # uniquenessLabel # n') #< (pmod # y # n'))
                          (pcons # output #$ self # y # xs)
                          (ptraceError "duplicate index detected")
                )
                (pcon PNil)
                order
          )
     in go

directOrderGlobalLogic :: Term s PStakeValidator
directOrderGlobalLogic = phoistAcyclic $ plam $ \_red ctx -> P.do
  let red = pconvert @PGlobalRedeemer _red
  redF <- pletFields @'["inputIdxs", "outputIdxs"] red
  ctxF <- pletFields @'["txInfo"] ctx
  infoF <- pletFields @'["inputs", "outputs", "signatories", "datums"] ctxF.txInfo

  let scInputs = puniqueOrderedTxOuts # plam (\idx -> pfield @"resolved" #$ pelemAt @PBuiltinList # idx # infoF.inputs) # 0 # redF.inputIdxs
      scOutputs = puniqueOrderedTxOuts # plam (\idx -> pelemAt @PBuiltinList # idx # infoF.outputs) # 0 # redF.outputIdxs

  let checks = pfoldTxUTxOs infoF.datums 0 scInputs scOutputs #== pcountScriptInputs # infoF.inputs

  pif
    checks
    (popaque (pconstant ()))
    perror
