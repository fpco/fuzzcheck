{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.FuzzCheck where

import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Applicative
import Data.Functor.Identity
import Data.Functor.Product
import Data.List
import Data.Monoid hiding (Product, Sum)
import Data.Typeable
import Debug.Trace
import Test.QuickCheck
import Prelude hiding (catch, ioError)
import System.IO.Unsafe

data Sum f g a = InL (f a) | InR (g a) deriving Show

instance (Functor f, Functor g) => Functor (Sum f g) where
    fmap f (InL x) = InL (fmap f x)
    fmap f (InR y) = InR (fmap f y)

class Natural f g where
    eta :: f a -> g a

instance (Applicative f, Applicative g, Natural g f)
         => Applicative (Sum f g) where
    pure x = InR (pure x)
    InL f <*> InL x = InL (f <*> x)
    InR g <*> InR y = InR (g <*> y)
    InL f <*> InR x = InL (f <*> eta x)
    InR g <*> InL x = InL (eta g <*> x)

instance Natural Gen Identity where
    eta g = Identity (head (unsafePerformIO (sample' g)))

newtype Fuzz m a = Fuzz (Product (Const [a -> m String]) (Sum Identity Gen) a)

instance Functor (Fuzz m) where
    fmap f (Fuzz x) = Fuzz (fmap f x)

instance Applicative (Fuzz m) where
    pure x = Fuzz (Pair (Const ["<arg>"]) (InL (Identity x)))
    Fuzz (Pair (Const l) f) <*> Fuzz (Pair (Const r) x) =
        Fuzz (Pair (Const (l <> r)) (f <*> x))

arg :: Show a => a -> Fuzz m a
arg x = Fuzz (Pair (Const [return . show]) (InL (Identity x)))

gen :: Show a => Gen a -> Fuzz m a
gen g = Fuzz (Pair (Const [return . show]) (InR g))

data FuzzException = FuzzException String deriving (Eq, Show, Typeable)
instance Exception FuzzException

runFuzz :: (MonadIO m, MonadBaseControl IO m)
        => String -> [a -> m String] -> m a -> m a
runFuzz lbl args m =
    m `catch` \e ->
        throwIO (FuzzException $
                 lbl ++ " " ++ unwords (map show args)
                     ++ ": " ++ show (e :: SomeException))

infixr 1 ?>
(?>) :: (MonadIO m, MonadBaseControl IO m) => String -> Fuzz m a -> m a
lbl ?> Fuzz (Pair (Const args) (InR g)) = do
    runFuzz lbl args $ head <$> liftIO (sample' g)
lbl ?> Fuzz (Pair (Const args) (InL (Identity x))) =
    runFuzz lbl args (return x)

fuzzCheck :: String -> Fuzz IO a -> IO a
fuzzCheck msg f = msg ?> f

fuzzCheck_ :: String -> Fuzz IO a -> IO ()
fuzzCheck_ msg f = void $ msg ?> f

{------------------------------------------------------------------------}

example :: Int -> IO Int
example x = if trace ("x :" ++ show x) x `mod` 3 == 0
            then ioError (userError "x divisible by three!")
            else return x

main :: IO ()
main = do
    -- void $ "example" ?> example <$> arg 10 -- this will fail
    -- x <- "example" ?> example <$> arg 9  -- this is ok, and returns 9
    -- void $ "next" ?> example <$> arg x -- this will pass too
    void $ "qc1" ?> example <$> gen (choose (1,9))
    void $ "qc2" ?> example <$> gen (choose (1,10))
    return ()
