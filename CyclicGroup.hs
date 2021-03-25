module CyclicGroup where

import System.Random
import Data.Map
import OnSpotPermut

data CyclicGroup g = CyclicGroup {size :: Integer, perm :: Permut, g :: g} deriving Show

make :: RandomGen g => Integer -> g -> CyclicGroup g
make s g = CyclicGroup s (OnSpotPermut.make s) g

add :: RandomGen g => CyclicGroup g -> Integer -> Integer -> (CyclicGroup g, Integer) 
add (CyclicGroup sz perm g) a b = 
    let a1 = mod a sz  in
    let b1 = mod b sz  in
    let (perm1, Just a2, g1) = ind_perm perm a1 g in 
    let (perm2, Just b2, g2) = ind_perm perm1 b1 g1 in
    let c2 = mod (a2 + b2) sz in
    let (perm3, Just c1, g3) = rev_perm perm2 c2 g2 in 
        ((CyclicGroup sz perm3 g3), c1)


invert :: RandomGen g => CyclicGroup g -> Integer -> (CyclicGroup g, Integer) 
invert (CyclicGroup sz perm g) a = 
    let a1 = mod a sz  in
    let (perm1, Just a2, g1) = ind_perm perm a1 g in 
    let c2 = mod (-a2) sz in
    let (perm2, Just c1, g2) = rev_perm perm1 c2 g1 in 
        ((CyclicGroup sz perm2 g2), c1)

generative :: RandomGen g => CyclicGroup g -> (CyclicGroup g, Integer) 
generative (CyclicGroup sz perm g) = 
    let (perm2, Just c1, g1) = rev_perm perm 1 g in 
        ((CyclicGroup sz perm2 g1), c1)

neutral :: RandomGen g => CyclicGroup g -> (CyclicGroup g, Integer) 
neutral (CyclicGroup sz perm g) = 
    let (perm2, Just c1, g1) = rev_perm perm 0 g in 
        ((CyclicGroup sz perm2 g1), c1)