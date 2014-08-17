module Processor (
) where

import Control.Lens ((^.), set, over)
import Control.Monad.State.Lazy (put, get, modify, runStateT, lift)

import Data
import qualified MMU


{- OPCODE HELPERS START -}

-- Register time passing
tickTock :: Monad m => Z80State m ()
tickTock = do
    modify $ \z80 -> set (regs.m) 1 z80
    modify $ \z80 -> set (regs.t) 4 z80

{- OPCODE HELPERS END -}

{- OPCODES START -}

-- 00
nop :: Monad m => Z80State m ()
nop = tickTock
-- 10
-- 20
-- 30
-- 40
-- 50
-- 60
-- 70
halt :: Monad m => Z80State m ()
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
opcodes :: Monad m => [Z80State m ()]
opcodes = [nop | x <- [0..255]]

updateClock :: Z80State IO ()
updateClock = do
    modify $ \z80 -> over (clock.clock_m) ((+) $ fromIntegral $ z80^.regs.m) z80
    modify $ \z80 -> over (clock.clock_t) ((+) $ fromIntegral $ z80^.regs.t) z80

dispatcher :: Z80State IO ()
dispatcher = do
    z80 <- get
    -- Increment program counter
    let incPC = 1 + (z80^.regs.pc)
    put $ set (regs.pc) incPC z80

    opcode <- MMU.readByte incPC
    case opcode of
        Left err -> error err
        Right opcode -> do
            opcodes !! (fromIntegral $ opcode :: Int)
            updateClock
            if incPC == 65535 then return () else dispatcher
