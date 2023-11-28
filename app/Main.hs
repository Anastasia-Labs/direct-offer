{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Order
import Utils (writePlutusScript)

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)

main :: IO ()
main = do
  exist <- doesDirectoryExist "compiled"
  createDirectoryIfMissing exist "compiled"
  writePlutusScript "Direct Order Spending Validator" "./compiled/directOrderSpending.json" $ Order.directOrderValidator
  writePlutusScript "Direct Order Staking Validator" "./compiled/directOrderStaking.json" $ Order.directOrderGlobalLogic
