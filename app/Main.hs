{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Offer
import Utils (writePlutusScript)

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)

main :: IO ()
main = do
  exist <- doesDirectoryExist "compiled"
  createDirectoryIfMissing exist "compiled"
  writePlutusScript "Direct Offer Spending Validator" "./compiled/directOfferSpending.json" $ Offer.directOfferValidator
  writePlutusScript "Direct Offer Staking Validator" "./compiled/directOfferStaking.json" $ Offer.directOfferGlobalLogic
