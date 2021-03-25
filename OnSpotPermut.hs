module OnSpotPermut where

import System.Random
import Data.Map
import SubSegTree

data Permut =   Permut  {size    :: Integer,
                        direct   :: Node,     
                        reversed :: Node, 
                        dirmap   :: Map Integer Integer, 
                        revmap   :: Map Integer Integer}  deriving Show

make :: Integer -> Permut
make s = Permut s (SubSegTree.make s) (SubSegTree.make s) empty empty

ind_perm :: RandomGen g => g -> Integer -> Permut -> (Permut, Maybe Integer, g)
ind_perm g a all@(Permut size direct reversed dirmap revmap) = 
    if (a < 0) || (a >= size) then
        (all, Nothing, g)
    else
        case Data.Map.lookup a dirmap of
        Just p  -> (all, Just p, g)
        Nothing ->
            let fr = free0 direct in
            let (num, g1) = randomR (0, fr - 1) g in 
            let (Just b, dir2) = push num direct in 
            let (_, rev2)      = push2 a reversed in 
            let dirmap2        = insert a b dirmap in
            let revmap2        = insert b a revmap in

            ((Permut size dir2 rev2 dirmap2 revmap2), (Just b), g1)


rev_perm :: RandomGen g => g -> Integer -> Permut -> (Permut, Maybe Integer, g)
rev_perm g a all@(Permut size direct reversed dirmap revmap) = 
    if (a < 0) || (a >= size) then
        (all, Nothing, g)
    else
        case Data.Map.lookup a revmap of
        Just p  -> (all, Just p, g)
        Nothing ->
            let fr = free0 reversed in
            let (num, g1) = randomR (0, fr - 1) g in 
            let (Just b, rev2) = push num reversed in 
            let (_, dir2)      = push2 a direct in 
            let dirmap2        = insert b a dirmap in
            let revmap2        = insert a b revmap in

            ((Permut size dir2 rev2 dirmap2 revmap2), (Just b), g1)




