module Config where

-- 4 * 4 slowdown
quality :: Bool
quality = False

res :: (Int, Int)
res = if quality then (600, 300) else (300, 150)

aaGenCount :: Int
aaGenCount = if quality then 16 else 4

---

kRes :: Double
kRes = fromIntegral $ uncurry min $ res
