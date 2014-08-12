{-# LANGUAGE TemplateHaskell #-}
module Data (
    Clock
,   clock_m
,   clock_t
,   Registers
,   MMU
,   bios
,   rom
,   vram
,   eram
,   wram
,   oam
,   zram
,   romOffs
,   ramOffs
,   ie
,   iflag
,   Flags
,   halt_flag
,   stop_flag
,   Z80
,   clock
,   flags
,   mmu
,   regs
,   a
,   b
,   c
,   d
,   e
,   h
,   l
,   f
,   pc
,   sp
,   m
,   t
,   Z80State
,   resetClock
,   resetRegisters
,   resetMMU
,   resetFlags
,   resetZ80
,   resetBIOS
) where

import Control.Lens (makeLenses)
import Control.Monad.State.Lazy (State, StateT)
import Data.Binary (Word8, Word16)
import qualified Data.ByteString as BS

import ROM (resetBIOS)

data Clock = Clock {
    _clock_m  :: Double
,   _clock_t  :: Double
} deriving (Show)
makeLenses ''Clock

data Registers = Registers {
    _a  :: Word8
,   _b  :: Word8
,   _c  :: Word8
,   _d  :: Word8
,   _e  :: Word8
,   _h  :: Word8
,   _l  :: Word8
,   _f  :: Word8   -- Flag
,   _pc :: Word16  -- Program counter
,   _sp :: Word16  -- Stack pointer
,   _m  :: Word8   -- Clock incrementer
,   _t  :: Word8   -- Clock incrementer
} deriving (Show)
makeLenses ''Registers

data MMU = MMU {
    _bios    :: BS.ByteString
,   _rom     :: BS.ByteString
,   _vram    :: BS.ByteString
,   _eram    :: BS.ByteString
,   _wram    :: BS.ByteString
,   _oam     :: BS.ByteString
,   _zram    :: BS.ByteString
,   _romOffs :: Word16
,   _ramOffs :: Word16
,   _ie      :: Word8
,   _iflag   :: Word8
}
makeLenses ''MMU

instance Show MMU where
    show (MMU bios rom vram eram wram oam zram romOffs ramOffs ie iflag) = "MMU: {\n\
\    bios: "       ++ (show $ BS.length bios) ++ " Bytes\n\
\    rom: "        ++ (show $ BS.length rom) ++ " Bytes\n\
\    vram: "       ++ (show $ BS.length vram) ++ " Bytes\n\
\    eram: "       ++ (show $ BS.length eram) ++ " Bytes\n\
\    wram: "       ++ (show $ BS.length wram) ++ " Bytes\n\
\    oam: "        ++ (show $ BS.length oam) ++ " Bytes\n\
\    zram: "       ++ (show $ BS.length zram) ++ " Bytes\n\
\    ROM Offset: " ++ (show $ romOffs) ++ "\n\
\    RAM Offset: " ++ (show $ ramOffs) ++ "\n\
\    IE: "         ++ (show $ ie) ++ "\n\
\    IFlag: "      ++ (show $ iflag) ++ "\n\
\}"

data Flags = Flags {
    _halt_flag :: Word8
,   _stop_flag :: Word8
} deriving Show
makeLenses ''Flags

data Z80 = Z80 {
     _clock :: Clock
,    _regs  :: Registers
,    _mmu   :: MMU
,    _flags :: Flags
} deriving (Show)
makeLenses ''Z80

type Z80State m a = StateT Z80 m a

resetClock :: Clock
resetClock = Clock 0 0

resetRegisters :: Registers
resetRegisters = Registers 0 0 0 0 0 0 0 0 0 0 0 0

resetMMU :: MMU
resetMMU = MMU {
    _bios    = resetBIOS
,   _rom     = resetRange 0x0000 0x7FFF
,   _vram    = resetRange 0x8000 0x9FFF
,   _eram    = resetRange 0xA000 0xBFFF
,   _wram    = resetRange 0xC000 0xEFFF
,   _oam     = resetRange 0xFE00 0xFEA0
,   _zram    = resetRange 0xFF80 0xFFFF
,   _romOffs = 0x4000
,   _ramOffs = 0x0000
,   _ie      = 0x00
,   _iflag   = 0x0000
}
  where resetRange f l = BS.pack [0 | x <- [f..l]]

resetFlags :: Flags
resetFlags = Flags 0 0

resetZ80 :: Z80
resetZ80 = Z80 resetClock resetRegisters resetMMU resetFlags
