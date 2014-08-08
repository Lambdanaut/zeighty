module Processor (
    Clock
,   clock
,   c_m
,   c_t
,   Registers
,   registers
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

import Data.Binary

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

clock :: Clock
clock = Clock 0 0

registers :: Registers
registers = Registers 0 0 0 0 0 0 0 0 0 0 0 0
