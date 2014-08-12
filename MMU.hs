module MMU (
    rb
,   rw
,   wb
,   ww
) where

import Prelude hiding ((!!))

import Data.Binary (Word8, Word16)
import Data.Bits ((.&.))
import Data.ByteString (index)
import Control.Lens ((^.), set)
import Control.Monad.State.Lazy (gets)

import Data


-- Reads a byte (8bit)
rb :: Monad m => Word16 -> Z80State m Word8
rb addr = 
    case addr .&. 0xF000 of
    roundAddr
     | roundAddr == 0x0000 ->
        if addr < 0x0100
        then gets $ \z80 -> index (z80^.mmu.bios) $ fromIntegral addr
        else gets $ \z80 -> index (z80^.mmu.rom) $ fromIntegral addr
     | any (==roundAddr) [0x1000,0x2000,0x3000]  -> gets $ \z80 -> index (z80^.mmu.rom)  $ fromIntegral addr                                      {- ROM BANK 0-}
     | any (==roundAddr) [0x4000,0x5000..0x7000] -> gets $ \z80 -> index (z80^.mmu.rom)  $ fromIntegral $ (z80^.mmu.romOffs) + (addr .&. 0x3FFF)  {- ROM BANK 1-}
     | any (==roundAddr) [0x8000,0x9000]         -> gets $ \z80 -> index (z80^.mmu.vram) $ fromIntegral $ addr .&. 0x1FFF                         {- VRAM -}
     | any (==roundAddr) [0xA000,0xB000]         -> gets $ \z80 -> index (z80^.mmu.eram) $ fromIntegral $ (z80^.mmu.ramOffs) + (addr .&. 0x1FFF)  {- External RAM -}
     | any (==roundAddr) [0xC000,0xD000,0xE000]  -> gets $ \z80 -> index (z80^.mmu.wram) $ fromIntegral $ addr .&. 0x1FFF                         {- Work RAM and echo -}
     | otherwise                                 -> error $ "Read byte index out of range: " ++ show addr
     | roundAddr == 0xF000                       ->
        case addr .&. 0x0F00 of                                                                                                                   {- WRAM Shadow, I/O, ZRAM -}
        roundAddr
         | any (==roundAddr) [0x000,0x100..0xD00] -> gets $ \z80 -> index (z80^.mmu.wram) $ fromIntegral $ addr .&. 0x1FFF                        {- WRAM Shadow -}
         | roundAddr == 0xE00                     -> if addr .&. 0xFF < 0xA0 then gets $ \z80 -> index (z80^.mmu.oam) $ fromIntegral $ addr .&. 0xFF else return 0  {- OAM Sprite data -}
         | roundAddr == 0xF00                     ->
            case addr .&. 0xF0 of 
            roundAddr
             | addr == 0xFFFF                      -> gets $ \z80 -> z80^.mmu.ie  {- TODO: Need to figure out what this does -}
             | addr > 0xFF7F                       -> gets $ \z80 -> index (z80^.mmu.zram) $ fromIntegral $ addr .&. 0x7F
             | roundAddr == 0x00 ->
                case addr .&. 0xF of
                roundAddr
                 | roundAddr == 0              -> error $ "Read byte: " ++ show addr ++ " is undefined. Need to fix. "  {- TODO -}
                 | any (==roundAddr) [4,5,6,7] -> error $ "Read byte: " ++ show addr ++ " is undefined. Need to fix. "  {- TODO -}
                 | roundAddr == 15             -> gets $ \z80 -> z80^.mmu.iflag
                 | otherwise                   -> return 0
             | any (==roundAddr) [0x10,0x20,0x30]  -> return 0
             | any (==roundAddr) [0x40,0x50..0x70] -> return 0 {- TODO READ FROM GPU -}
        -- | roundAddr == 0xF00 && (addr >= 0xFF80) = ((zram mem) !! (addr .&. 0x7F),(inBios mem))                             {- ZRAM -}
        -- | roundAddr == 0xF00 && (addr < 0xFF80)  = (31337,False)                                                            {- I/O control handling -}


--rb :: (MMUState) -> Int -> Int -> (Int, Bool)
--rb mem addr npc 
--  | roundAddr == 0x0000 = if (inBios mem) && (addr < 0x0100) then ((bios mem) !! addr,chBios) else ((rom mem) !! addr,chBios) {- BIOS -}
--  | any (==roundAddr) [0x1000,0x2000..0x7000] = ((rom mem) !! addr,(inBios mem))                                              {- ROM -}
--  | any (==roundAddr) [0x8000,0x9000]         = ((vram mem) !! (addr .&. 0x1FFF),(inBios mem))                                {- VRAM -}
--  | any (==roundAddr) [0xA000,0xB000]         = ((eram mem) !! (addr .&. 0x1FFF),(inBios mem))                                {- ERAM -}
--  | any (==roundAddr) [0xC000,0xD000,0xE000]  = ((wram mem) !! (addr .&. 0x1FFF),(inBios mem))                                {- WRAM -}
--  | roundAddr == 0xF000                       = f000Check                                                                     {- WRAM Shadow, I/O, ZRAM -}
--  | otherwise                                 = (31337,False)                                                                 {- BREAKS SHIT -}
--  where roundAddr = addr .&. 0xF000
--        chBios = if (npc == 0x0100) then False else True
--        f000Check 
--          | any (==roundAddr) [0x000,0x100..0xD00] = ((wram mem) !! (addr .&. 0x1FFF),(inBios mem))                           {- WRAM Shadow -}
--          | roundAddr == 0xE00 && (addr < 0xFEA0)  = ((oam mem) !! (addr .&. 0xFF),(inBios mem))                              {- OAM -}
--          | roundAddr == 0xF00 && (addr >= 0xFF80) = ((zram mem) !! (addr .&. 0x7F),(inBios mem))                             {- ZRAM -}
--          | roundAddr == 0xF00 && (addr < 0xFF80)  = (31337,False)                                                            {- I/O control handling -}
--          | otherwise                              = (31337,False)                                                            {- BREAKS SHIT -}
--          where roundAddr = addr .&. 0x0F00

-- Writes a word (16bit)
rw :: ()
rw = ()

-- Writes a byte (8bit)
wb :: Monad m => Word16 -> Word8 -> Z80State m Word8
wb addr val = return 0

-- Writes a word (16bit)
ww :: ()
ww = ()
