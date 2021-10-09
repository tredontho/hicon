{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module PPM where

import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Word (Word8)
import Image

data PPM = PPM
  { ppmHeader :: ByteString,
    ppmBody :: ByteString
  }
  deriving (Show, Eq)

createPPM :: Image a -> PPM
createPPM img =
  PPM
    { ppmHeader = makeHeader img,
      ppmBody = makeBody img
    }

magicNumber :: ByteString
magicNumber = BS8.pack "P6"

writePPM :: FilePath -> PPM -> IO ()
writePPM filePath PPM {ppmHeader, ppmBody} =
  BS.writeFile
    filePath
    ( Builder.toLazyByteString $
        Builder.lazyByteString ppmHeader <> Builder.charUtf8 '\n' <> Builder.lazyByteString ppmBody
    )

makeHeader :: Image a -> ByteString
makeHeader Image {imgHeight, imgWidth, imgPixels} =
  Builder.toLazyByteString $
    Builder.lazyByteString magicNumber
      <> Builder.stringUtf8 "\n"
      <> Builder.wordDec imgWidth
      <> Builder.charUtf8 ' '
      <> Builder.wordDec imgHeight
      <> Builder.charUtf8 '\n'
      <> Builder.word8Dec maxColor
  where
    maxColor = maximum . colorVals $ imgPixels

colorVals :: [Pixel a] -> [Word8]
colorVals [] = []
colorVals ps = concatMap extractColors ps

extractColors :: Pixel a -> [Word8]
extractColors (BW w) = [w]
extractColors (RGB r g b) = [r, g, b]
extractColors (RGBA r g b _) = [r, g, b]

makeBody :: Image a -> ByteString
makeBody Image {imgPixels} = rows
  where
    rows = BS.concat $ map toPPMPixel imgPixels

toPPMPixel :: Pixel a -> ByteString
toPPMPixel (BW w) =
  Builder.toLazyByteString $
    Builder.word8 w <> Builder.word8 w <> Builder.word8 w
toPPMPixel (RGB r g b) =
  Builder.toLazyByteString $
    Builder.word8 r <> Builder.word8 g <> Builder.word8 b
toPPMPixel (RGBA r g b _) =
  Builder.toLazyByteString $
    Builder.word8 r <> Builder.word8 g <> Builder.word8 b
