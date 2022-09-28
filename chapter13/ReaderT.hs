-- Building Out The Monad Transformer Library

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Reader as Reader
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Trans.State (StateT (StateT), runStateT)

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance (Functor m) => Functor (ReaderT r m) where
    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap f (ReaderT rma) = ReaderT $ fmap f . rma

instance (Applicative m) => Applicative (ReaderT r m) where
    pure :: a -> ReaderT r m a
    pure a = ReaderT $ \r -> pure a
    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    (ReaderT f) <*> (ReaderT a) = ReaderT $ \r -> f r <*> a r

instance (Monad m) => Monad (ReaderT r m) where
    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    (ReaderT rma) >>= f = ReaderT $ \r ->
        do
            a <- rma r
            let rmb = runReaderT (f a)
            let ma = rmb r
            ma

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

instance MonadTrans (ReaderT r) where
    lift m = ReaderT $ const m

ask' :: Monad m => ReaderT r m r
ask' = ReaderT pure

local' :: Monad m => (r -> r) -> ReaderT r m a -> ReaderT r m a
local' f (ReaderT rma) = ReaderT $ rma . f

class Monad m => MonadReader r m | m -> r where
    ask :: m r
    local :: (r -> r) -> m a -> m a

instance Monad m => MonadReader r (Reader.ReaderT r m) where
    ask = Reader.ask
    local = Reader.local

instance MonadReader r m => MonadReader r (ExceptT e m) where
    ask = lift ask
    local f ma = ExceptT $ local f (runExceptT ma)

instance MonadReader r m => MonadReader r (StateT s m) where
    ask = lift ask
    local f ma = StateT $ \s -> local f (runStateT ma s)
