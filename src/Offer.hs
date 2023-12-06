{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Offer (
  directOfferValidator,
  directOfferGlobalLogic,
  puniqueOrdered,
  PDirectOfferDatum (..),
  PDirectOfferRedeemer (..),
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
import Plutarch.Unsafe ()
import PlutusLedgerApi.V1 ()
import Utils (pand'List, pcountScriptInputs, presolveHashByDatum, (#>=))
import "liqwid-plutarch-extra" Plutarch.Extra.Numeric ((#^))
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

data PDirectOfferRedeemer (s :: S)
  = PExecuteOffer (Term s (PDataRecord '[]))
  | PReclaim (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PDirectOfferRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PDirectOfferRedeemer

directOfferValidator :: Term s (PStakingCredential :--> PValidator)
directOfferValidator = phoistAcyclic $ plam $ \stakeCred dat redeemer ctx -> P.do
  let redeemer' = pconvert @PDirectOfferRedeemer redeemer
      dat' = pconvert @PDirectOfferDatum dat
  ctxF <- pletFields @'["txInfo"] ctx
  infoF <- pletFields @'["wdrl", "signatories"] ctxF.txInfo
  pmatch redeemer' $ \case
    PExecuteOffer _ ->
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
          pofferSuccessor datums state utxoIn utxoOut
      )
    # acc
    # la
    # lb

pofferSuccessor ::
  Term s (PMap 'Unsorted PDatumHash PDatum) ->
  Term s PInteger ->
  Term s PTxOut ->
  Term s PTxOut ->
  Term s PInteger
pofferSuccessor datums foldCount offerInput offerOutput = unTermCont $ do
  offerInputF <- pletFieldsC @'["address", "value", "datum"] offerInput
  offerOutputF <- pletFieldsC @'["address", "value", "datum"] offerOutput
  -- offerInput should contain a datum of the type PDirectOfferDatum
  let offerInputDatum =
        offerInputF.datum `pmatch` \case
          POutputDatum r -> (pfield @"outputDatum" # r)
          POutputDatumHash r -> presolveHashByDatum # (pfield @"datumHash" # r) # datums
          PNoOutputDatum _ -> ptraceError "No output datum"
  let inputDatum = pconvert @PDirectOfferDatum (pto offerInputDatum)

  pure $
    pif
      ( pand'List
          [ -- The address of all offerInput's must be a script address
            ptraceIfFalse "Not a script input" (pisScriptAddress # offerInputF.address)
          , -- The address of offerOutput must match offerInput.datum.creator
            ptraceIfFalse "Offer not sent to creator" (offerOutputF.address #== pfield @"creator" # inputDatum)
          , -- The value of offerOutput must be equal to or greater than offerInput.datum.toBuy
            ptraceIfFalse "Offer value less than expected" (passertPositive # (passertSorted # offerOutputF.value) #>= pfield @"toBuy" # inputDatum)
          ]
      )
      (foldCount + 1)
      perror

puniqueOrdered :: (PElemConstraint PBuiltinList a) => Term s ((PInteger :--> a) :--> PInteger :--> (PBuiltinList (PAsData PInteger)) :--> (PBuiltinList a))
puniqueOrdered =
  phoistAcyclic $
    let go :: (PElemConstraint PBuiltinList a) => Term s ((PInteger :--> a) :--> PInteger :--> (PBuiltinList (PAsData PInteger)) :--> (PBuiltinList a))
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

directOfferGlobalLogic :: Term s PStakeValidator
directOfferGlobalLogic = phoistAcyclic $ plam $ \_red ctx -> P.do
  let red = pconvert @PGlobalRedeemer _red
  redF <- pletFields @'["inputIdxs", "outputIdxs"] red
  ctxF <- pletFields @'["txInfo"] ctx
  infoF <- pletFields @'["inputs", "outputs", "signatories", "datums"] ctxF.txInfo

  let scInputs = puniqueOrdered # plam (\idx -> pfield @"resolved" #$ pelemAt @PBuiltinList # idx # infoF.inputs) # 0 # redF.inputIdxs
      scOutputs = puniqueOrdered # plam (\idx -> pelemAt @PBuiltinList # idx # infoF.outputs) # 0 # redF.outputIdxs

  let checks = pfoldTxUTxOs infoF.datums 0 scInputs scOutputs #== pcountScriptInputs # infoF.inputs

  pif
    checks
    (popaque (pconstant ()))
    perror
