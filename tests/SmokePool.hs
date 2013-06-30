{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Monad
import Test.FuzzCheck
import Test.FuzzCheck.Pool
import Test.Hspec
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    itio "works with a test with only one value pool" $
        fuzzPool 100 10
            [ PoolAction ((\x -> assertBool "greater" (x > 0)) :: Int -> IO ())
            , PoolAction (return 2 :: IO Int)
            , PoolAction (return . (*2) :: Int -> IO Int)
            ]

    itio "works with a test with multiple pools" $
        fuzzPool 100 10
            [ PoolAction (\i -> return . concat . replicate i :: String -> IO String)
            , PoolAction (return 3 :: IO Int)
            , PoolAction (return "hello! " :: IO String)
            ]

    itio "doesn't execute unreachable tests" $
        fuzzPool 100 10
            [ PoolAction (const $ assertFailure "failure" :: Bool -> IO ())
            , PoolAction (return 3 :: IO Int)
            ]

    itio "fails for a failing test" $
        fuzzPoolFAIL ""
            [ PoolAction ("fail" ?> pure (ioError $ userError "test") :: IO ())
            , PoolAction (return 0 :: IO Int)
            ]

  where
    itio msg f = it msg (f :: IO ())

    fuzzPoolFAIL msg fs = fuzzPool 100 10 fs `shouldThrow`
        \(e :: FuzzException) -> True -- show e == msg
