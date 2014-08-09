module MMU (
    rb
,   rw
,   wb
,   ww
) where

import Data.Binary (Word8, Word16)
import qualified Data.ByteString as BS

import Data


-- Reads a byte (8bit)
rb :: Word16 -> Z80State Word8
rb i = return 2

-- Writes a byte (8bit)
rw :: ()
rw = ()

-- Reads a word (16bit)
wb :: ()
wb = ()

-- Writes a word (16bit)
ww :: ()
ww = ()
