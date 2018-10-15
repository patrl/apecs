{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Apecs.Reactive where

import Data.IORef
import Data.Proxy
import qualified Data.IntSet as S
import qualified Data.IntMap.Strict as M
import Control.Monad.Reader

import Apecs.Core

type family ReactElem r

class Monad m => Reacts m r where
  rempty :: m r
  react  :: Entity -> Maybe (ReactElem r) -> Maybe (ReactElem r) -> r -> m ()

type instance ReactElem (a,b) = ReactElem a
instance (ReactElem a ~ ReactElem b, Reacts m a, Reacts m b) => Reacts m (a, b) where
  {-# INLINE rempty #-}
  rempty = liftM2 (,) rempty rempty
  {-# INLINE react #-}
  react ety old new (a,b) = react ety old new a >> react ety old new b

data Reactive r s = Reactive r s

type instance Elem (Reactive r s) = Elem s

rget :: forall w r s. 
  ( Component (ReactElem r)
  , Has w IO (ReactElem r)
  , Storage (ReactElem r) ~ Reactive r s
  ) => SystemT w IO r
rget = do
  Reactive r (_ :: s) <- getStore
  return r

instance (Reacts IO r, ExplInit IO s) => ExplInit IO (Reactive r s) where
  explInit = liftM2 Reactive rempty explInit

instance (Reacts IO r, ExplSet IO s, ExplGet IO s, Elem s ~ ReactElem r)
  => ExplSet IO (Reactive r s) where
  {-# INLINE explSet #-}
  explSet (Reactive r s) ety c = do
    old <- explGet (MaybeStore s) ety
    react (Entity ety) old (Just c) r
    explSet s ety c

instance (Reacts IO r, ExplDestroy IO s, ExplGet IO s, Elem s ~ ReactElem r)
  => ExplDestroy IO (Reactive r s) where
  {-# INLINE explDestroy #-}
  explDestroy (Reactive r s) ety = do
    old <- explGet (MaybeStore s) ety
    react (Entity ety) old Nothing r
    explDestroy s ety

instance ExplGet IO s => ExplGet IO (Reactive r s) where
  {-# INLINE explExists #-}
  explExists (Reactive _ s) = explExists s
  {-# INLINE explGet    #-}
  explGet    (Reactive _ s) = explGet    s

instance ExplMembers IO s => ExplMembers IO (Reactive r s) where
  {-# INLINE explMembers #-}
  explMembers (Reactive _ s) = explMembers s

data Printer c = Printer
type instance ReactElem (Printer c) = c

instance Show c => Reacts IO (Printer c) where
  {-# INLINE rempty #-}
  rempty = return Printer
  {-# INLINE react #-}
  react (Entity ety) (Just c) Nothing _ =
    putStrLn $ "Entity " ++ show ety ++ ": destroyed component " ++ show c
  react (Entity ety) Nothing (Just c) _ =
    putStrLn $ "Entity " ++ show ety ++ ": created component " ++ show c
  react (Entity ety) (Just old) (Just new) _ =
    putStrLn $ "Entity " ++ show ety ++ ": update component " ++ show old ++ " to " ++ show new
  react _ _ _ _ = return ()

newtype EnumMap c = EnumMap (IORef (M.IntMap S.IntSet))

type instance ReactElem (EnumMap c) = c
instance Enum c => Reacts IO (EnumMap c) where
  {-# INLINE rempty #-}
  rempty = EnumMap <$> newIORef mempty
  {-# INLINE react #-}
  react _ Nothing Nothing _ = return ()
  react (Entity ety) (Just c) Nothing (EnumMap ref) = modifyIORef' ref (M.adjust (S.delete ety) (fromEnum c))
  react (Entity ety) Nothing (Just c) (EnumMap ref) = modifyIORef' ref (M.insertWith mappend (fromEnum c) (S.singleton ety))
  react (Entity ety) (Just old) (Just new) (EnumMap ref) = do
    modifyIORef' ref (M.adjust (S.delete ety) (fromEnum old))
    modifyIORef' ref (M.insertWith mappend (fromEnum new) (S.singleton ety))


{-# INLINE mapLookup #-}
mapLookup :: Enum c => EnumMap c -> c -> System w [Entity]
mapLookup (EnumMap ref) c = do
  emap <- liftIO $ readIORef ref
  return $ maybe [] (fmap Entity . S.toList) (M.lookup (fromEnum c) emap)
