module EitherIO where

import Control.Applicative

newtype EitherIO e a = EitherIO { runEitherIO :: IO (Either e a) }

instance Functor (EitherIO e) where
  fmap f = EitherIO . fmap (fmap f) . runEitherIO

instance Applicative (EitherIO e) where
  pure = EitherIO . return . Right
  f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)

instance Monad (EitherIO e) where
  return = pure
  x >>= f = EitherIO $ runEitherIO x >>=
            either (return . Left) (runEitherIO . f)
