module TransHelpers where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

newtype EitherT l t a = EitherT {runEitherT :: t (Either l a)}

instance MonadTrans (EitherT l) where
    lift x =
        EitherT (Right <$> x)

instance (Monad t) => Monad (EitherT l t) where
    (>>=) (EitherT val) fn =
        EitherT $
        val >>= \a ->
        case a of
            Right x ->
                runEitherT (fn x)
            Left err ->
                pure (Left err)

instance (Monad t) => Applicative (EitherT l t) where
    pure = EitherT . pure . pure

    ma <*> mb = do
        a <- ma
        a <$> mb

instance Monad t => Functor (EitherT l t) where
    fmap fn arg = do
        a <- arg
        pure (fn a)