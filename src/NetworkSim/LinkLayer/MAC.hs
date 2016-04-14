{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NetworkSim.LinkLayer.MAC
  ( MAC ()
  , freshMAC
  , broadcastAddr
  , stpAddr
  ) where

import Data.Word
import Data.Bits
import Data.Unique
import Data.List (intercalate)
import GHC.Generics
import Data.Hashable
import Text.Printf
import Data.Binary

-- | 48-bit medium access control (MAC) address.
newtype MAC = MAC
  { mac :: Word64
  } deriving (Eq, Ord, Generic, Hashable, Binary)

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

-- | Multicast address used in the spanning tree protocol.
stpAddr :: MAC
stpAddr
  = MAC 0x0180c2000000
