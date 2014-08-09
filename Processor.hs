{-# LANGUAGE TemplateHaskell #-}
module Processor (
) where

import Control.Lens ()
import Control.Monad.State.Lazy (State, put, get, modify, runState)

import Data
import qualified MMU

dispatcher :: Z80State ()
dispatcher = do
    state <- get
    let inc_pc = 1 + (r_pc $ z80_r state)
    put state {z80_r = (z80_r state) {r_pc = inc_pc}}
    opcode <- MMU.rb inc_pc
    opcodes !! (fromIntegral $ opcode :: Int)

    --dispatcher

{- OPCODE HELPERS START -}

-- Register time passing
tickTock :: Z80State ()
tickTock = modify (\state -> state {z80_r = (z80_r state) {r_m = 1, r_t = 4}})

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
halt = tickTock >> modify (\state -> state {z80_f = (z80_f state) {f_halt = 1}})
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
