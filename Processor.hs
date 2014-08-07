module Processor where

import Data.Binary

data Register = Register {
    r_a :: Word8,
    r_b :: Word8,
    r_c :: Word8,
    r_d :: Word8,
    r_e :: Word8,
    r_h :: Word8,
    r_l :: Word8
} deriving (Show)