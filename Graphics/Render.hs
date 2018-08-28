module Graphics.Render where

type Renderer = Geometry -> RT (Int -> Int -> PixelRGB8)
