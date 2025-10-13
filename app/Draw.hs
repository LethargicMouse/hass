{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Draw (Draw (..)) where

import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy

class (Default r, ToRenderable r) => Draw r a | a -> r where
  drawToFile :: FilePath -> a -> IO ()
  drawToFile p = toFile def p . draw

  draw :: a -> EC r ()

instance (PlotValue x, PlotValue y) => Draw (Layout x y) (String, [(x, y)]) where
  draw (n, v) = plot (line n [v])

instance (PlotValue x, PlotValue y) => Draw (Layout x y) (Plot x y) where
  draw = plot . pure
