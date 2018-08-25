{-# LANGUAGE BangPatterns #-}

module Progress where

import System.TimeIt
import Control.DeepSeq
import Control.Parallel.Strategies
import Control.Concurrent
import Control.Concurrent.MVar
-- import Control.Concurrent.STM.Counter
import Data.IORef
import Codec.Picture
import Data.List.Split
import Data.Time.Clock.POSIX
import Data.Time.Clock
import System.IO

--
-- chunksOf :: Int -> [a] -> [[a]]
-- chunksOf _ [] = []
-- chunksOf n xs = h : (chunksOf n t)
--   where (h, t) = splitAt n xs

-- parMapM :: NFData b => (a -> IO b) -> [a] -> IO [b]
-- parMapM f xs = (\rs -> rs `using` parBuffer 2 rdeepseq) <$> mapM f xs
--
--
-- parMapM' :: NFData b => (a -> IO b) -> [a] -> IO [b]
-- parMapM' f xs = (\rs -> rs `using` parBuffer 8 rdeepseq) <$> mapM f xs
-- parMapM' = mapM

andByTheWay :: IO a -> IO () -> IO a
andByTheWay a b = a >>= \r -> b >> return r

-- parMapM'r :: NFData b => (a -> IO b) -> [a] -> IO [b]
-- parMapM'r cnt f xs = (\rs -> rs `using` parBuffer 8 rdeepseq) <$> mapM ((`andByTheWay` (modifyMVarPure cnt (+1))) . f) xs

-- parMapM'r cnt f xs = (\rs -> rs `using` parList rdeepseq) <$> mapM f xs

chunkCount :: Int
chunkCount = 256

parMapM'r :: (NFData b) => IORef Int -> (a -> IO b) -> [a] -> IO [b]
parMapM'r cnt f xs =
  do
    let perThread = 1 + (floor $ fromIntegral (length xs) / fromIntegral chunkCount)

    let chunks = (chunksOf perThread xs) -- :: [[a]]

    resvars <- (mapM (const $ newEmptyMVar) chunks) -- :: IO (MVar [b])

    let act xs mv = mapM f xs >>= (\rs -> rs `deepseq` modifyIORef' cnt (+1) >> putMVar mv rs)

    let actions = map (uncurry act) (zip chunks resvars)

    threads <- mapM forkOS actions

    concat <$> sequence (map readMVar resvars)

    -- let (xz, rs) = splitAt thr xs



-- modifyMVarPure :: IORef a -> (a -> a) -> IO ()
-- modifyMVarPure v f = modifyMVar_ v (return . f)

reportProgress :: NominalDiffTime -> IORef Int -> IO ()
reportProgress starTime v =
  do
    !c <- readIORef v
    -- print ""

    now <- getPOSIXTime

    let totalTime = toRational (now - starTime)

     -- print (c, chunkCount)
    let left = chunkCount - c
    let speed = if c > 0 then totalTime / fromIntegral c else 0
    let timeLeft = speed * fromIntegral left
    let pos = fromIntegral c / fromIntegral chunkCount
    let perc = floor $ 40 * pos
    -- print (totalTime, left, cnt)
    putStr $ (
      "\r["
      ++ (replicate perc '#')
      ++ (replicate (40 - perc) ' ')
      ++ "] " ++ (show $ floor timeLeft)) ++ "s"

    threadDelay 1000000
    reportProgress starTime v

mapWithProgressBar :: NFData b => (a -> IO b) -> [a] -> IO [b]
-- mapWithProgressBar f xs =
  -- mapM f xs
-- mapWithProgressBar f xs = (id $!!) <$> mapM f xs
  -- concat <$> mapWithProgressBar' (parMapM' f) (chunksOf 16 xs)

mapWithProgressBar f xs =
  do -- let total = length xs

    hSetBuffering stdout NoBuffering

    cnt <- newIORef 0
    startTime <- getPOSIXTime
    repThread <- forkIO $ reportProgress startTime cnt

    -- let reportingF xs = mapM f xs >>= \r -> modifyMVarPure cnt (+1) >> return r

    -- concat <$>
    parMapM'r cnt f xs --(mapM f) (chunksOf 8 xs)
    -- concat <$> parMapM' (mapM (\x -> f x >>= \r -> r `deepseq` modifyMVarPure cnt (+1) >> return r)) (chunksOf 32 xs)

-- mapWithProgressBar = mapWithProgressBar'

mapWithProgressBar' :: NFData b => (a -> IO b) -> [a] -> IO [b]
mapWithProgressBar' f xs = iter 0 xs
  where
    cnt = fromIntegral $ length xs

    -- iter :: Double -> [a] -> IO [b]
    iter _         []     = return []
    iter totalTime (x:xs) =
      do let left = fromIntegral (length xs)
         let speed = totalTime / (cnt - left)
         let timeLeft = floor $ 1000 * speed * left
         let pos = 1 - (left / cnt)
         let perc = floor $ 40 * pos
         -- print (totalTime, left, cnt)
         putStr $ (
           "\r["
           ++ (replicate perc '#')
           ++ (replicate (40 - perc) ' ')
           ++ "] " ++ (show timeLeft))
         (t, !r) <- timeItT (f x)
         -- print t
         rs <- iter (totalTime + t) xs
         return (r:rs)
