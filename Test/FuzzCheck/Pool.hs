{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TupleSections #-}

module Test.FuzzCheck.Pool where

import Control.Applicative
import Control.Monad (unless, void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.IORef
import Data.Traversable (forM)
import Data.Typeable
import GHC.Prim (Any)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Test.QuickCheck
import Unsafe.Coerce

import Test.FuzzCheck

data PoolAction (m :: * -> *) = forall a. PoolTaggable m a => PoolAction a

data PoolTag (m :: * -> *) = PoolTag [TypeRep] TypeRep

class PoolTaggable m a where
    poolTag :: a -> PoolTag m

instance (Typeable a) => PoolTaggable m (m a) where
    poolTag _ = PoolTag [] $ typeOf (undefined :: a)

instance (Typeable a, PoolTaggable m b) => PoolTaggable m (a -> b) where
    poolTag _ = PoolTag (typeOf (undefined :: a) : xs) r
      where
        PoolTag xs r = poolTag (undefined :: b) :: PoolTag m

{- NOTE: could instead use this + runtime exceptions
splitTyArrows :: TypeRep -> [TypeRep]
splitTyArrows t =
    case splitTyConApp t of
        (tc, [tl, tr]) | tc == funTc -> tl : splitTyArrows tr
        _ -> [t]
-}

fuzzPool :: forall m. (MonadIO m, MonadBaseControl IO m)
         => Int -> Int -> [PoolAction m] -> m ()
fuzzPool runs poolSize actions = do
    let tagged = map (\x@(PoolAction a) -> (poolTag a :: PoolTag m, x)) actions
        inputs = S.fromList $ concatMap (\(PoolTag inTys _, _) -> inTys) tagged
        outputs = S.fromList $ map (\(PoolTag _ outTy, _) -> outTy) tagged

    -- Warn about inputs and outputs that won't be used.
    -- (NOTE: doesn't compute actual reachability)
    let unreachableInputs = S.difference inputs outputs
        unreachableOutputs = S.difference outputs (S.insert (typeOf ()) inputs)
    unless (S.null unreachableInputs) $ liftIO $ do
        putStrLn "Warning: The following FuzzPool input types are unreachable:"
        print $ S.toList unreachableInputs
    unless (S.null unreachableOutputs) $ liftIO $ do
        putStrLn "Warning: The following FuzzPool output types are unused:"
        print $ S.toList unreachableOutputs

    -- Fuzzify!
    pools <- liftIO . mapM initPool . S.toList
           $ S.difference inputs unreachableInputs

    go (V.fromList tagged) (M.fromList pools) 0
  where
    initPool k = do
      pool <- liftIO $ MV.new poolSize
      size <- newIORef 0
      return (k, (size, pool))

    go :: V.Vector (PoolTag m, PoolAction m)
       -> M.Map TypeRep (IORef Int, MV.IOVector Any)
       -> Int
       -> m ()
    go tagged pools i
        | i >= runs = return ()
        | otherwise = do
            let len = V.length tagged
            n <- "pick a random action"
              ?> return <$> gen (choose (0,len-1) :: Gen Int)
            case V.unsafeIndex tagged n of
                (PoolTag inTys outTy, PoolAction f) -> do
                    -- Pick values from pools that match the argument types.
                    mvals <- sequence <$> mapM pickValue inTys
                    -- If we got values for all of the arguments, apply them.
                    outVal <- forM mvals $ unsafeCoerce . foldl unsafeCoerce f
                    -- Store the result value.
                    _ <- forM outVal (storeValue outTy)
                    return ()
            go tagged pools (i + 1)
      where
        pickValue :: TypeRep -> m (Maybe Any)
        pickValue ty =
            case M.lookup ty pools of
                Nothing -> return Nothing
                Just (sizeRef, pool) -> do
                    size <- liftIO $ readIORef sizeRef
                    if size == 0
                        then return Nothing
                        else do
                            n <- ("pick from pool a value of type " ++ show ty)
                              ?> return <$> gen (choose (0,size-1) :: Gen Int)
                            Just <$> liftIO (MV.unsafeRead pool n)
        storeValue :: TypeRep -> Any -> m ()
        storeValue ty val =
            void $ forM (M.lookup ty pools) $ \(sizeRef, pool) -> do
                size <- liftIO $ readIORef sizeRef
                if size >= poolSize
                    then liftIO $ do
                        n <- ("replace a random value in pool for " ++ show ty)
                          ?> return <$> gen (choose (0,size-1) :: Gen Int)
                        MV.write pool n val
                    else liftIO $ do
                        MV.write pool size val
                        writeIORef sizeRef (size + 1)
