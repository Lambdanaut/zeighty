module Z80 where

import Control.Monad.State

import Processor


data Z80 = Z80 {
	z80_c   :: Clock
,	z80_r   :: Registers
,	z80_mmu :: MMU
}

main :: IO ()
main = do
    return ()