{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.FuzzCheck where

import Control.Applicative
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product
import Data.List
import Data.Typeable
import Prelude hiding (catch, ioError)
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen(..))

newtype Fuzz a = Fuzz (Compose Gen (Product (Const [String]) Identity) a)
               deriving Functor

instance Applicative Fuzz where
    pure x = Fuzz (Compose (pure (Pair (Const ["<arg>"]) (Identity x))))
    Fuzz f <*> Fuzz x = Fuzz (f <*> x)

arg :: Show a => a -> Fuzz a
arg x = Fuzz (Compose (pure (Pair (Const [show x]) (Identity x))))

gen :: Show a => Gen a -> Fuzz a
gen (MkGen m) = Fuzz (Compose (MkGen g))
  where g r n = let x = m r n in Pair (Const [show x]) (Identity x)

data FuzzException = FuzzException String deriving (Eq, Show, Typeable)
instance Exception FuzzException

runFuzz :: (MonadIO m, MonadBaseControl IO m)
        => String -> [String] -> m a -> m a
runFuzz lbl args m = m `catch` report
  where report e = throwIO (FuzzException $
                            lbl ++ " " ++ unwords (map show args)
                                ++ ": " ++ show (e :: SomeException))

infixr 1 ?>
(?>) :: (MonadIO m, MonadBaseControl IO m)
     => String -> Fuzz (m a) -> m a
lbl ?> Fuzz (Compose (MkGen g)) = do
    rnd <- liftIO newStdGen
    let Pair (Const args) (Identity x) = g rnd 100
    runFuzz lbl args x

fuzzCheck' :: (MonadIO m, MonadBaseControl IO m)
           => m a -> Int -> m () -> m ()
fuzzCheck' f runs cleanup = replicateM_ runs f `finally` cleanup

fuzzCheck :: (MonadIO m, MonadBaseControl IO m)
          => m a -> m ()
fuzzCheck f = fuzzCheck' f 100 $
              liftIO $ putStrLn "+++ OK, passed 100 tests."
