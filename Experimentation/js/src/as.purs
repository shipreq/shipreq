-- vim:ft=haskell:
module Golly where

import Data.Array
import Data.Maybe

sum :: [Number] -> Number
sum [] = 0
sum (x:xs) = x + sum(xs)

-----------------------------------------------------------------------------------

type Id = Number
data AllocSegment = AllocSegment String Id
data Alloc = Alloc (Maybe AllocSegment) Number
data Grouping = Grouping String [Alloc]

sampleData = [
  Grouping "Priority" [
    Alloc Nothing 57 ,
    Alloc (Just $ AllocSegment "High"   1) 20 ,
    Alloc (Just $ AllocSegment "Medium" 2) 17 ,
    Alloc (Just $ AllocSegment "Low"    3)  4 ]
  ,
  Grouping "Version" [ Alloc Nothing $ 57+20+17+4-63 + 1
                     , Alloc (Just $ AllocSegment "v2.x"  20) 60
                     , Alloc (Just $ AllocSegment "v3.x"  21)  0
                     , Alloc (Just $ AllocSegment "Defer" 22)  3
                     ]
  ]

segName (AllocSegment n _) = n
allocQty (Alloc _ q) = q
isUnalloc (Alloc Nothing _) = true
isUnalloc (Alloc (Just _) _) = false

countUnAp (Grouping _ as) = sum $ map allocQty $ filter isUnalloc $ as

{-
data AllocSegmentX = AllocSegmentX {allocName :: String, allocId :: Id}
data AllocX = AllocX {seg :: (Maybe AllocSegmentX), qty :: Number}
data GroupingX = GroupingX {gname :: String, allocs :: [AllocX]}
sampleDataX = [
  GroupingX { gname: "Priority"
            , allocs: [ AllocX {seg: Nothing                          , qty: 57}
                      , AllocX {seg: (Just $ AllocSegmentX {allocName: "High"  , id: 1}), qty: 20}
                      , AllocX {seg: (Just $ AllocSegmentX {allocName: "Medium", id: 2}), qty: 17}
                      , AllocX {seg: (Just $ AllocSegmentX {allocName: "Low"   , id: 3}), qty:  4}
  ]} ,
  GroupingX { gname: "Version"
            , allocs: [ AllocX {seg: Nothing, qty: 57+20+17+4-63 + 1}
                      , AllocX {seg: (Just $ AllocSegmentX {allocName: "v2.x" , id:20}), qty: 60}
                      , AllocX {seg: (Just $ AllocSegmentX {allocName: "v3.x" , id:21}), qty:  0}
                      , AllocX {seg: (Just $ AllocSegmentX {allocName: "Defer", id:22}), qty:  3}
  ]} ]
-}
