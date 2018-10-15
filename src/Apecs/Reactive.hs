{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Apecs.Reactive where

import Data.IORef
import Data.Proxy
import Control.Monad.Reader

import Apecs.Core

type family ReactElem r

class Monad m => Reacts m r where
  rempty :: m r
  react  :: Entity -> Maybe (ReactElem r) -> Maybe (ReactElem r) -> r -> m r

rget :: forall w r s. 
  ( Component (ReactElem r)
  , Has w IO (ReactElem r)
  , Storage (ReactElem r) ~ Reactive r s
  ) => SystemT w IO r
rget = do
  Reactive ref (_ :: s) <- getStore
  liftIO $ readIORef ref

rset :: forall w r s. 
  ( Component (ReactElem r)
  , Has w IO (ReactElem r)
  , Storage (ReactElem r) ~ Reactive r s
  ) => r -> SystemT w IO ()
rset r = do
  Reactive ref (_ :: s) <- getStore
  liftIO $ writeIORef ref r

rmodify :: forall w r s. 
  ( Component (ReactElem r)
  , Has w IO (ReactElem r)
  , Storage (ReactElem r) ~ Reactive r s
  ) => (r -> r) -> SystemT w IO ()
rmodify f = rget >>= rset . f

type instance ReactElem (a,b) = ReactElem a
instance (ReactElem a ~ ReactElem b, Reacts m a, Reacts m b) => Reacts m (a, b) where
  {-# INLINE rempty #-}
  rempty = liftM2 (,) rempty rempty
  {-# INLINE react #-}
  react ety old new (a,b) = liftM2 (,) (react ety old new a) (react ety old new b)

data Reactive r s = Reactive (IORef r) s

type instance Elem (Reactive r s) = Elem s

instance (Reacts IO r, ExplInit IO s) => ExplInit IO (Reactive r s) where
  explInit = liftM2 Reactive (rempty >>= newIORef) explInit

instance (Reacts IO r, ExplSet IO s, ExplGet IO s, Elem s ~ ReactElem r)
  => ExplSet IO (Reactive r s) where
  {-# INLINE explSet #-}
  explSet (Reactive ref s) ety c = do
    cOld <- explGet (MaybeStore s) ety
    r  <- readIORef ref
    r' <- react (Entity ety) cOld (Just c) r
    writeIORef ref r'
    explSet s ety c

instance (Reacts IO r, ExplDestroy IO s, ExplGet IO s, Elem s ~ ReactElem r)
  => ExplDestroy IO (Reactive r s) where
  {-# INLINE explDestroy #-}
  explDestroy (Reactive ref s) ety = do
    cOld <- explGet (MaybeStore s) ety
    r  <- readIORef ref
    r' <- react (Entity ety) cOld Nothing r
    writeIORef ref r'
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
  react (Entity ety) (Just c) Nothing _ = do
    putStrLn $ "Entity " ++ show ety ++ ": destroyed component " ++ show c
    return Printer
  react (Entity ety) Nothing (Just c) _ = do
    putStrLn $ "Entity " ++ show ety ++ ": created component " ++ show c
    return Printer
  react (Entity ety) (Just old) (Just new) _ = do
    putStrLn $ "Entity " ++ show ety ++ ": update component " ++ show old ++ " to " ++ show new
    return Printer
  react _ _ _ _ = return Printer
