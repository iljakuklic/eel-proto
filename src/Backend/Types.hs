
module Backend.Types where

import Foreign.C.Types
import Foreign.Storable

inBits x = sizeOf x * 8
u = undefined

sizeBits    = inBits (u :: CSize)
ptrBits     = inBits (u :: CIntPtr)
ptrDiffBits = inBits (u :: CPtrdiff)
intBits     = inBits (u :: CInt)
charBits    = inBits (u :: CChar)

