module Main (main, mainEval1, mainEval2) where

import Test.Tasty (
  defaultMain,
  testGroup,
 )

import Data.Text.IO qualified as TIO
import Prettyprinter qualified as P
import Spec.SpendingValidatorSpec qualified as SpendingValidatorSpec
import Spec.StakingValidatorSpec qualified as StakingValidatorSpec
import Utils (evalT)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Unit Test"
      [ SpendingValidatorSpec.sampleTest
      , StakingValidatorSpec.sampleTest
      ]
  mainEval1
  mainEval2

mainEval1 :: IO ()
mainEval1 = do
  case evalT SpendingValidatorSpec.sampleTestEval of
    Left e -> TIO.putStrLn e
    Right r -> putStrLn (show r)

mainEval2 :: IO ()
mainEval2 = do
  case evalT StakingValidatorSpec.sampleTestEval of
    Left e -> TIO.putStrLn e
    Right r -> putStrLn (show r)
