{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NetworkSim.LinkLayer.MAC
  ( MAC ()
  , freshMAC
  , broadcastAddr
  ) where

import Data.Int
import Data.Bits
import Data.Unique
import Data.List (intercalate)
import GHC.Generics
import Data.Hashable
import Text.Printf

-- | 48-bit medium access control (MAC) address.
newtype MAC = MAC
  { mac :: Int64
  } deriving (Eq, Generic, Hashable)

instance Show MAC where
  show (mac -> mac')
    = intercalate sep . map (printf "%x") $
        [ shiftR mac' 40 .&. mask
        , shiftR mac' 32 .&. mask
        , shiftR mac' 24 .&. mask
        , shiftR mac' 16 .&. mask
        , shiftR mac' 8 .&. mask
        , mac' .&. mask
        ]
    where
      mask
        = 0xff
      sep
        = ":"

-- | A fresh MAC address for NICs. As only the second 3 bytes are
-- NIC-specific, the number of unique machine-assignable addresses is
-- 2^24.
freshMAC :: IO MAC
freshMAC
  = MAC . (0x020000000000 .|.) . (0xffffff .&.) . fromIntegral . hashUnique <$> newUnique

broadcastAddr :: MAC
broadcastAddr
  = MAC 0xffffffffffff

  
