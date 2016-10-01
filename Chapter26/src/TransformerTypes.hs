{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

module TransformerTypes where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }

-- Remember the Functor for Compose?

-- instance (Functor, Functor g) => Functor (Compose f g) where
--     fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Functor m) => Functor (MaybeT m) where
    fmap f ( MaybeT ma ) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
    pure x = MaybeT (pure (pure x))

    (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
    return = pure

    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (MaybeT ma) >>= f = MaybeT $ do
            v <- ma
            case v of
              Nothing -> return Nothing
              Just y -> runMaybeT (f y)

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO ioa = MaybeT $ Just <$> (liftIO ioa)

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance Applicative m => Applicative (EitherT e m) where
    pure x = EitherT (pure( pure x))

    (EitherT fab) <*> (EitherT a) = EitherT $ (<*>) <$> fab <*> a

instance Monad m => Monad (EitherT e m) where
    return = pure

    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (EitherT me) >>= f = EitherT $ do
        v <- me
        case v of
          (Left e) -> return $ Left e 
          (Right a) -> runEitherT (f a)

instance MonadTrans (EitherT e) where
    lift ma = EitherT (Right <$> ma)

swapEither :: Either a b -> Either b a
swapEither (Left a) = Right a
swapEither (Right b) = Left b

--Hint: write swapEither first, then swapEitherT in terms of the former.
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ swapEither <$> mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f f' (EitherT mab) = do
    v <- mab
    case v of
      Left a -> (f a)
      Right b -> (f' b)


newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
    pure a = ReaderT (pure (pure a))

    (ReaderT fmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
    return = pure

    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    (ReaderT rma) >>= f = ReaderT $ \r -> do
        a <- rma r
        runReaderT (f a) r

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO ios = ReaderT $ \s -> do
        a <- liftIO ios
        return a

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

swap :: (a,b) -> (b,a)
swap (a,b) = (b, a)

-- instance (Functor m) => Functor (StateT s m) where
--     fmap f (StateT sma) = 
--         let x = (fmap . fmap ) swap sma
--             y = (fmap . fmap . fmap ) f x
--             z = (fmap . fmap ) swap y
--         in StateT $ z

instance (Functor m) => Functor (StateT s m) where
    fmap f (StateT sma) = StateT $ (fmap . fmap) (swap . fmap f . swap) sma

instance forall m s . (Monad m) => Applicative (StateT s m) where
    pure :: forall a . a -> StateT s m a
    -- pure a = StateT $ pure $ pure (a,mempty)
    pure a = StateT $ \s -> pure (a, s) 
    -- pure = pure

    (<*>) :: forall a b . StateT s m (a -> b) -> StateT s m a -> StateT s m b
    (StateT smf) <*> (StateT sma) = 
        StateT $ \s -> 

            let goal :: (m (b,s)) 
                goal = second third
                smf :: (s -> m ((a -> b), s))
                smf = smf
                sma :: (s -> m(a,s))
                sma = sma
                first :: m ((a -> b), s)
                first = smf s
                second :: ((a -> b, s) -> m (b,s)) -> m (b, s)
                second = (>>=) first
                third :: (a -> b, s) -> m (b,s)
                third (f , s) = 
                    let sma' :: m(a,s)
                        sma' = sma s
                        foo :: m(b,s)
                        foo = fmap bar sma' 
                        bar :: (a,s) -> (b, s)
                        bar (a,s) = (f a, s)
                     in foo
            in goal       

instance forall s m . (Monad m) => Monad (StateT s m) where

    return :: a -> StateT s m a
    return = pure

    (>>=) :: forall a b . StateT s m a -> (a -> StateT s m b) -> StateT s m b
    (StateT sma) >>= f = 
        StateT $ \s -> 
            let goal :: m (b,s)
                goal = first >>= second
                first :: m (a,s)
                first = sma s
                second :: (a,s) -> m (b,s) 
                second (a, s) = runStateT (f a) s
            in goal

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        a <- m
        return (a,s)

-- newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO ios = StateT $ \s -> do
        a <- liftIO ios
        return (a, s)


