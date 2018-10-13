{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
    ( TD(..),
      start,start',
      advances,advances',
      retreats,retreats',
      stepRandom,stepRandom',
      rhombus,
      diaTD
    ) where

import qualified Data.Map.Strict as M
import Data.Foldable
import Control.Monad
import Control.Monad.Random.Class
import Diagrams.Prelude hiding (conjugate,start)
import Data.Complex
import Data.Foldable


i = 0:+1
omega = (-1/2) :+ (sqrt 3/2)
omega' = conjugate omega

vec (x :+ y) = V2 x y
pt (x :+ y) = p2 (x,y)


data TD = TD {
  volume :: !Int,
  hXYtoZ :: M.Map (Int,Int) Int,
  hYZtoX :: M.Map (Int,Int) Int,
  hZXtoY :: M.Map (Int,Int) Int} deriving (Show,Eq)

start = TD 0 M.empty M.empty M.empty

startR :: Int -> Int -> M.Map (Int,Int) Int
startR m n = M.fromList [((i,j),0)|i<-[0..m-1],j<-[0..n-1]]

start' l m n = TD 0 (startR l m) (startR m n) (startR n l)


findC :: Int -> Int -> M.Map (Int,Int) Int -> Int
findC u v = M.findWithDefault 0 (u,v)

bound :: M.Map (Int,Int) Int -> Int
bound x = case (M.toDescList x) of
  [] -> 0
  ((a,_),_):_ -> a+1

adv :: Maybe Int -> Maybe Int
adv Nothing = Just 1
adv (Just x) = Just (x+1)

advance :: TD -> Int -> Int -> Int -> TD
advance (TD v xyZ yzX zxY) x y z =
  TD (v+1) (M.alter adv (x,y) xyZ) (M.alter adv (y,z) yzX) (M.alter adv (z,x) zxY)

advances :: TD -> [TD]
advances d@(TD v xyZ yzX zxY) = do
  x <- [0..bound xyZ]
  y <- [0..bound yzX]
  let z = findC x y xyZ
  guard $ x == findC y z yzX
  guard $ y == findC z x zxY
  return $ advance d x y z

advances' :: TD -> [TD]
advances' d@(TD v xyZ yzX zxY) = do
  ((x,y),z) <- M.toList xyZ
  guard $ Just x == M.lookup (y,z) yzX
  guard $ Just y == M.lookup (z,x) zxY
  return $ advance d x y z

rtr :: Maybe Int -> Maybe Int
rtr Nothing = Nothing -- shouldn't happen though
rtr (Just 1) = Nothing
rtr (Just x) = Just (x-1)

retreat :: TD -> Int -> Int -> Int -> TD
retreat (TD v xyZ yzX zxY) x y z = 
  TD (v-1) (M.alter rtr (x,y) xyZ) (M.alter rtr (y,z) yzX) (M.alter rtr (z,x) zxY)

retreats :: TD -> [TD]
retreats d@(TD v xyZ yzX zxY) = do
  x <- [0..bound xyZ]
  y <- [0..bound yzX]
  let z = (findC x y xyZ) - 1
  guard $ z>=0
  guard $ x + 1 == findC y z yzX
  guard $ y + 1 == findC z x zxY
  return $ retreat d x y z

retreat' :: TD -> Int -> Int -> Int -> TD
retreat' (TD v xyZ yzX zxY) x y z =
  TD (v-1) (M.adjust s (x,y) xyZ) (M.adjust s (y,z) yzX) (M.adjust s (z,x) zxY)
  where s = subtract 1

retreats' :: TD -> [TD]
retreats' d@(TD v xyZ yzX zxY) = do
  ((x,y),z') <- M.toList xyZ
  let z = z'-1
  guard $ Just (x+1) == M.lookup (y,z) yzX
  guard $ Just (y+1) == M.lookup (z,x) zxY
  return $ retreat' d x y z

stepRandom d = uniform $ (advances d ++ retreats d)
stepRandom' d = uniform $ (advances' d ++ retreats' d)


rhombus :: Trail' Loop V2 Double
rhombus = fromVertices . map pt $ [0, omega, omega+omega', omega',0]

sh :: Int -> Int -> Int -> Complex Double
sh x y z = fromIntegral x*omega+fromIntegral y*omega'+fromIntegral z

rhombi = foldMap w . M.toList
  where w ((x,y),z) = strokeLoop rhombus # translate (vec $ sh x y z)

diaTD (TD _ xyZ yzX zxY) = (fc lightblue (rhombi  xyZ) <>
                            (rotateBy (1/3) . fc darkblue) (rhombi yzX) <>
                            (rotateBy (2/3) . fc blue) (rhombi zxY)) # lw thin # rotateBy (1/4) # pad 1.1

someFunc :: IO ()
someFunc = putStrLn "someFunc"
