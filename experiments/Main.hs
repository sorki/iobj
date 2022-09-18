module Main where

import Prelude
import Control.Applicative (pure)
import Graphics.Implicit
import Graphics.Implicit.Definitions
import Graphics.Implicit.Primitives
import Graphics.Implicit.MathUtil

roundbox:: SymbolicObj3
roundbox =
  implicit
    (\(V3 x y z) -> x^4 + y^4 + z^4 - 15000)
    (pure (-20), pure 20)

torus :: Double -> Double -> SymbolicObj3
torus distanceR radius =
  implicit
    (\(V3 x y z) -> (sqrt (x^2 + y^2) - distanceR)^2 + z^2 - radius^2 )
    --- XXX: fixme
    ( pure (-10), pure 10)

torusSample = torus 5 3

genus2 :: SymbolicObj3
genus2 =
  implicit
    (\(V3 x y z) -> 2*y*(y^2 - 3*x^2)*(1 - z^2) + (x^2 + y^2)^2 - (9*z^2 - 1)*(1 - z^2) )
    (pure (-30), pure 30)

mixed
  :: (Object obj f a, Semigroup obj)
  => a
  -> obj
  -> obj
  -> obj
mixed s a b = implicit
  (\i ->
    (1 - s) * getImplicit a i
  +      s  * getImplicit b i
  )
  (getBox (a <> b))

loft :: Double -> Double -> SymbolicObj2 -> SymbolicObj2 -> SymbolicObj3
loft z2 bulgeA a b = implicit
  (\(V3 x y z) ->
    let
      t =  z / z2-z1
      bulge = ((-1) * bulgeA * (sin (pi * t)))
    in
      rmax 0 (bulge + (getImplicit (mixed t a b) $ (V2 x y)))
        (abs (z - (z2-z1)/2) - (z2-z1)/2)
  )
  ((V3 x1 y1 z1), (V3 x2 y2 z2))
  where
    (V2 x1 y1, V2 x2 y2) = getBox (a <> b)
    z1 = 0

obj = loft 5 0 (square True (pure 10)) (circle 2)

fMap
  :: (Object obj f a, Semigroup obj)
  => (f a -> f a)
  -> obj
  -> obj
fMap f obj = implicit
  (\i -> getImplicit obj (f i))
  (f <$> getBox obj)

displaced
  :: (Object obj f a)
  => (f a -> a)
  -> obj
  -> obj
displaced dispFn obj = implicit
  (\i -> 
    let prim = getImplicit obj i
        disp = dispFn i
    in prim + disp
    )
  ((*2) <$> getBox obj)

mul (V3 a b c) = a * b * c

-- segfaults hah
displacedSphere =
  displaced (\v@(V3 x y z) -> mul (
    V3 (sin . (*(2)) $ x)
       (cos . (*(1/2)) $ y)
       (sin z)
    ) ) $ sphere 10

displacedSphere1 = displaced (\v@(V3 x y z) -> mul ( (cos . (*(2))) <$> v ) ) $ sphere 10

main = writeSTL 1 "displaced.stl"  $ displacedSphere
