module OnSpotPermut where

import System.Random
import Data.Map
import SubSegTree.h

data Permut =   Permut  {size    :: Integer
                        direct   :: Node,     
                        reversed :: Node, 
                        dirmap   :: Map Ineger Integer, 
                        revmap   :: Map Integer Integer}

make :: Integer -> Permut
make s = Permut s (make s) (make s) empty empty

new_pair :: Random g => g -> Integer -> Permut -> (Permut, Maybe Integer, g)
new_pair g a (size direct reversed dirmap) = 
    if (a < 0) && (a >= size)





