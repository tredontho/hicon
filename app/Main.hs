module Main where

import PPM
import Image

main :: IO ()
main = do
  writePPM "out/circle.ppm" $ PPM.createPPM $ blackCircle 500
  writePPM "out/square.ppm" $ PPM.createPPM $ blackSquare 500
