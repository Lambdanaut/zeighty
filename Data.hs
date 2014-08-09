{-# LANGUAGE TemplateHaskell #-}
module Data (
    Clock
,   c_m
,   c_t
,   Registers
,   r_a
,   r_b
,   r_c
,   r_d
,   r_e
,   r_h
,   r_l
,   r_f
,   r_pc
,   r_sp
,   r_m
,   r_t
,   MMU
,   Flags
,   f_halt
,   f_stop
,   Z80State
,   z80_c
,   z80_r
,   z80_mmu
,   z80_f
,   resetClock
,   resetRegisters
,   resetMMU
,   resetFlags
,   resetZ80
,   resetBIOS
) where

import Control.Lens ()
import Control.Monad.State.Lazy (State)
import Data.Binary (Word8, Word16)
import qualified Data.ByteString as BS

import ROM (resetBIOS)

data Clock = Clock {
    c_m  :: Double
,   c_t  :: Double
} deriving (Show)

data Registers = Registers {
    r_a  :: Word8
,   r_b  :: Word8
,   r_c  :: Word8
,   r_d  :: Word8
,   r_e  :: Word8
,   r_h  :: Word8
,   r_l  :: Word8
,   r_f  :: Word8   -- Flag
,   r_pc :: Word16  -- Program counter
,   r_sp :: Word16  -- Stack pointer
,   r_m  :: Word8   -- Clock incrementer
,   r_t  :: Word8   -- Clock incrementer
} deriving (Show)

data MMU = MMU {
    inBios :: Bool
,   mmu_bios   :: BS.ByteString
,   mmu_rom    :: BS.ByteString
,   mmu_vram   :: BS.ByteString
,   mmu_eram   :: BS.ByteString
,   mmu_wram   :: BS.ByteString
,   mmu_oam    :: BS.ByteString
,   mmu_zram   :: BS.ByteString
}

data Flags = Flags {
    f_halt :: Word8
,   f_stop :: Word8
} deriving Show

data Z80 = Z80 {
     z80_c   :: Clock
,    z80_r   :: Registers
,    z80_mmu :: MMU
,    z80_f   :: Flags
} deriving (Show)

type Z80State a = State Z80 a

instance Show MMU where
    show (MMU inBios bios rom vram eram wram oam zram) = "MMU: {\n\
\    Currently In BIOS: " ++ show inBios ++ "\n\
\    Bios: "   ++ (show $ BS.length bios) ++ " Bytes\n\
\    rom: "    ++ (show $ BS.length rom) ++ " Bytes\n\
\    vram: "   ++ (show $ BS.length vram) ++ " Bytes\n\
\    eram: "   ++ (show $ BS.length eram) ++ " Bytes\n\
\    wram: "   ++ (show $ BS.length wram) ++ " Bytes\n\
\    oam: "    ++ (show $ BS.length oam) ++ " Bytes\n\
\    zram: "   ++ (show $ BS.length zram) ++ " Bytes\n\
\}"

resetClock :: Clock
resetClock = Clock 0 0

resetRegisters :: Registers
resetRegisters = Registers 0 0 0 0 0 0 0 0 0 0 0 0

resetMMU :: MMU
resetMMU = MMU {
    inBios = True 
,   mmu_bios   = resetBIOS
,   mmu_rom    = resetRange 0 0x7FFF
,   mmu_vram   = resetRange 8000 0x9FFF
,   mmu_eram   = resetRange 0xA000 0xBFFF
,   mmu_wram   = resetRange 0xC000 0xEFFF
,   mmu_oam    = resetRange 0xFE00 0xFEA0
,   mmu_zram   = resetRange 0xFF80 0xFFFF
}
  where resetRange f l = BS.pack [0 | x <- [f..l]]

resetFlags :: Flags
resetFlags = Flags 0 0

resetZ80 :: Z80
resetZ80 = Z80 resetClock resetRegisters resetMMU resetFlags
