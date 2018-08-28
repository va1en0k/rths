module Util where

takeBy :: Int -> [a] -> [[a]]
takeBy cnt [] = []
takeBy cnt xs = (take cnt xs : takeBy cnt (drop cnt xs))

maxFloat :: Double
maxFloat = fromIntegral $ snd $ floatRange (0.5 :: Double)
