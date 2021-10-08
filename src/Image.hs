{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Image (Image (..), Pixel (..), blackCircle) where

import Data.Word (Word8)

data BW deriving (Show, Eq)

data RGB deriving (Show, Eq)

data RGBA deriving (Show, Eq)

data Image a = Image
  { imgWidth :: Word,
    imgHeight :: Word,
    imgPixels :: [Pixel a]
  }
  deriving (Show, Eq)

data Pixel a where
  BW :: Word8 -> Pixel BW
  RGB :: Word8 -> Word8 -> Word8 -> Pixel RGB
  RGBA :: Word8 -> Word8 -> Word8 -> Word8 -> Pixel RGBA

deriving instance Eq a => Eq (Pixel a)

deriving instance Show a => Show (Pixel a)

blackCircle :: Word -> Image BW
blackCircle radius =
  Image
    { imgWidth = 2 * radius,
      imgHeight = 2 * radius,
      imgPixels = drawBlackCircle radius
    }

drawBlackCircle :: Word -> [Pixel BW]
drawBlackCircle radius =
  map
    (\coord -> let (y, x) = quotRem coord (2 * radius) in if (fromEnum x - fromEnum radius) ^ 2 + (fromEnum y - fromEnum radius) ^ 2 > fromEnum radius ^ 2 then BW 255 else BW 0)
    [0 .. (2 * radius) ^ 2 - 1]
