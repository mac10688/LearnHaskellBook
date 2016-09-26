{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

module TransformerTypes where

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

newtype EitherT m e a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT m e) where
    fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance Applicative m => Applicative (EitherT m e) where
    pure x = EitherT (pure( pure x))

    (EitherT fab) <*> (EitherT a) = EitherT $ (<*>) <$> fab <*> a

instance Monad m => Monad (EitherT m e) where
    return = pure

    (>>=) :: EitherT m e a -> (a -> EitherT m e b) -> EitherT m e b
    (EitherT me) >>= f = EitherT $ do
        v <- me
        case v of
          (Left e) -> return $ Left e 
          (Right a) -> runEitherT (f a)

swapEither :: Either a b -> Either b a
swapEither (Left a) = Right a
swapEither (Right b) = Left b

--Hint: write swapEither first, then swapEitherT in terms of the former.
swapEitherT :: (Functor m) => EitherT m e a -> EitherT m a e
swapEitherT (EitherT mea) = EitherT $ swapEither <$> mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT m a b -> m c
eitherT f f' (EitherT mab) = do
    v <- mab
    case v of
      Left a -> (f a)
      Right b -> (f' b)

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
