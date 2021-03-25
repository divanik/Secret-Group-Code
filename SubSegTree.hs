module SubSegTree where

{-import Data-}

data Node  = Empty | Leaf {size :: Integer} | Mid {free :: Integer, size :: Integer, lef :: Node, rig :: Node} deriving Show

make :: Integer -> Node
make s  	| s == 0  	= Empty
			| otherwise = Leaf s

size0 :: Node -> Integer
size0 Empty = 0
size0 all = size all

free0 :: Node -> Integer
free0 Empty = 0
free0 (Leaf sz) = sz
free0 all = free all


push :: Integer -> Node -> (Maybe Integer, Node)
push k Empty = (Nothing, Empty)

push k all@(Leaf sz) = 
	if ((k < 0) || (k >= sz)) then
		(Nothing, all)
	else 
		if (sz == 1) then
			(Just 0, Empty)
		else	
			let a = div sz 2 in 
			let b = div (sz + 1) 2 in 
			if (k < a) then
				let (Just num, tr) = (push k (make a)) in 
					(Just num, Mid (sz - 1) sz tr (make b))
			else
				let (Just num, tr) = (push (k - a) (make b)) in 
					(Just (num + a), Mid (sz - 1) sz (make a) tr)

push k all@(Mid fr sz lf rg) =
	if ((k < 0) || (k >= fr)) then
		(Nothing, all)
	else
		let lfr = free0 lf in
		let lsz = size0 lf in
		if (k < lfr) then
			let (Just num, tr) = (push k lf) in
			(Just num, Mid (fr - 1) sz tr rg)
		else	
			let (Just num, tr) = (push (k - lfr) rg) in
			(Just (num + lsz), Mid (fr - 1) sz lf tr)


push2 :: Integer -> Node -> (Maybe Integer, Node)
push2 k Empty = push k Empty

push2 k all@(Leaf sz) = push k all

push2 k all@(Mid fr sz lf rg) =
	if ((k < 0) || (k >= sz)) then
		(Nothing, all)
	else
		let lfr = free0 lf in
		let lsz = size0 lf in
		if (k < lsz) then
			let (lget, tr) = (push2 k lf) in
			case lget of
			Nothing  -> (Nothing, all)
			Just num -> (Just num, Mid (fr - 1) sz tr rg)
		else	
			let (rget, tr) = (push2 (k - lsz) rg) in
			case rget of
			Nothing  -> (Nothing, all)
			Just num -> (Just (num + lsz), Mid (fr - 1) sz lf tr)

