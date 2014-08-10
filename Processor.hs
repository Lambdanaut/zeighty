module Processor (
) where

import Control.Lens ((^.), set, over)
import Control.Monad.State.Lazy (State, put, get, modify, runState)

import Data
import qualified MMU


{- OPCODE HELPERS START -}

-- Register time passing
tickTock :: Z80State ()
tickTock = do
    modify $ \z80 -> set (regs.m) 1 z80
    modify $ \z80 -> set (regs.t) 4 z80

{- OPCODE HELPERS END -}

{- OPCODES START -}

-- 00
nop = tickTock
-- 10
-- 20
-- 30
-- 40
-- 50
-- 60
-- 70
halt :: Z80State ()
halt = tickTock >> (modify $ \state -> set (flags.halt_flag) 1 state)
-- 80
-- 90
-- A0
-- B0
-- C0
-- D0
-- E0
-- F0

{- OPCODES END -}


-- Mapping from opcode value to corresponding function
opcodes :: [Z80State ()]
opcodes = [nop | x <- [0..255]]

dispatcher :: Z80State ()
dispatcher = do
    z80 <- get
    -- Increment program counter
    let inc_pc = 1 + (z80^.regs.pc)
    put $ set (regs.pc) inc_pc z80

    opcode <- MMU.rb inc_pc
    opcodes !! (fromIntegral $ opcode :: Int)

    -- Update system clock
    modify $ \z80 -> over (clock.clock_m) ((+) $ fromIntegral $ z80^.regs.m) z80
    modify $ \z80 -> over (clock.clock_t) ((+) $ fromIntegral $ z80^.regs.t) z80

    if inc_pc == 50000 then return () else dispatcher
