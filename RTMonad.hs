{-# LANGUAGE Rank2Types, ExistentialQuantification #-}

module RTMonad where

import Control.Monad.Random
import Control.Monad.Identity
import Control.Applicative

import Types

data Settings = Settings World

data RT a = RT (forall g . RandomGen g => Settings -> Rand g (a, Settings))

instance Functor RT where
  fmap f (RT c) = RT $ \s -> fmap (\(v, s1) -> (f v, s1)) (c s)

instance Applicative RT where
  pure a = RT $ \s -> return (a, s)
  (RT a1) <*> (RT a2) = RT $ \s ->
    do (f, s1) <- a1 s
       (v, s2) <- a2 s1
       return (f v, s2)

instance Monad RT where
  (RT a1) >>= f = RT $ \s ->
    do (v1, s1) <- a1 s
       let (RT a2) = f v1
       a2 s1

setSettings :: Settings -> RT ()
setSettings s = RT $ \_ -> return ((), s)

-- runRand :: RandomGen g => Rand g a -> RT a
-- runRand m = RT Nothing m

getRand :: RT Double
getRand = RT $ \s -> getRandomR (0, 1) >>= \v -> return (v, s)

getRands :: RT [Double]
getRands =
  do
    a <- getRand
    bs <- getRands
    return (a:bs)

getSettings :: RT Settings
getSettings = RT $ \s -> return (s, s)
--
--
getWorld :: RT World
getWorld = getSettings >>= (\(Settings w) -> return w)

runRT :: Settings -> RT a -> IO a
runRT s (RT a) = fst <$> runIdentity <$> evalRandT (a s) <$> newStdGen
