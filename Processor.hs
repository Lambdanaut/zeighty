module Processor (
    Clock
,   resetClock
,   c_m
,   c_t
,   Registers
,   resetRegisters
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
) where

import Control.Monad.State
import Data.Binary (Word8, Word16)

import MMU


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

data Flags = Flags {
  halts :: Int,
  stops :: Int
} deriving Show

data Z80 = Z80 {
	z80_c   :: Clock
,	z80_r   :: Registers
,	z80_mmu :: MMU
} deriving (Show)

type Z80State a = State Z80 a

resetClock :: Clock
resetClock = Clock 0 0

resetRegisters :: Registers
resetRegisters = Registers 0 0 0 0 0 0 0 0 0 0 0 0

resetZ80 :: Z80
resetZ80 = Z80 resetClock resetRegisters resetMMU


dispatcher :: Z80State ()
dispatcher = do
    dispatcher

{- OPCODES START HERE -}
nop :: Z80State ()
nop = do
	z80 <- get
	put z80 {z80_r = (z80_r z80) {r_m = 1, r_t = 4} }
{- OPCODES END HERE -}


-- Mapping from opcode value to corresponding function
opcodes :: [Z80State ()]
opcodes = [nop | x <- [0..255]]
