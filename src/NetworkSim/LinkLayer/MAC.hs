{-# LANGUAGE ViewPatterns #-}

module NetworkSim.LinkLayer.MAC
  ( MAC ()
  , freshMAC
  , broadcastAddr
  ) where

import Data.Int
import Data.Bits
import Data.Unique
import Data.List (intercalate)

-- | 48-bit medium access control (MAC) address.
newtype MAC = MAC
  { mac :: Int64
  } deriving (Eq)

instance Show MAC where
  show (mac -> mac')
    = intercalate sep . map show $
        [ shiftR mac' 40 .&. mask
        , shiftR mac' 32 .&. mask
        , shiftR mac' 24 .&. mask
        , shiftR mac' 16 .&. mask
        , shiftR mac' 16 .&. mask
        , mac' .&. mask
        ]
    where
      mask
        = 0xffff
      sep
        = ":"

freshMAC :: IO MAC
freshMAC
  = MAC . (0xffffffffffff .&.) . fromIntegral . hashUnique <$> newUnique

broadcastAddr :: MAC
broadcastAddr
  = MAC 0xffffffffffff

  
