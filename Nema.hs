{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module SomeModule where

import Linear -- (V2(..), V3(..))
import Graphics.Implicit
import Graphics.Implicit.Primitives

import SomeOtherModule

res = 2.3


objA = withRounding 1 $ difference (union [
    cube True (V3 5 10 3)
  , translate (V3 2 3 0) $ cube True (V3 5 5 6)
  ])
  [
    translate (V3 0 0 (-10)) $ cylinder 2 20
  ]

data Nema = Nema {
    nemaStackLengths     :: [Double]
  , nemaStacks           :: Int
  , nemaSide             :: Double
  , nemaMountingHoleDia  :: Double
  , nemaMountingHoleDist :: Double
  , nemaMountingHoleDepth :: Double
  , nemaAxleDia          :: Double
  , nemaAxleLength       :: Double
  , nemaAxleFlatDepth    :: Double
  , nemaAxleDouble       :: Bool
 }

nema17 = Nema {
    nemaStackLengths      = [33, 39, 47]
  , nemaStacks            = 2
  , nemaSide              = 42.2
  , nemaMountingHoleDia   = 4
  , nemaMountingHoleDist  = 31
  , nemaMountingHoleDepth = 4.6
  , nemaAxleDia           = 5
  , nemaAxleLength        = 24
  , nemaAxleFlatDepth     = 0.5
  , nemaAxleDouble        = False
  }

side = 42.3

obj = nema nema17

nema Nema{..} =
  difference nemaBody
  [
    -- mounting holes
    atCorners
      (V3 nemaMountingHoleDist nemaMountingHoleDist (len/2 - nemaMountingHoleDepth))
      $ cylinder (nemaMountingHoleDia / 2) nemaMountingHoleDepth
  ]
  where
    len = nemaStackLengths !! nemaStacks
    shaft = cylinder (nemaAxleDia / 2) nemaAxleLength
    shaftWithFlat = difference shaft [
      translate (V3 (nemaAxleDia / 2 - nemaAxleFlatDepth) (-5) 0)
        $ cube False (V3 10 10 100)
      ]
    nemaBody = intersect [
        union [
          cube True (V3 nemaSide nemaSide len)
        , translate (V3 0 0 (len/2)) $
            difference (cylinder 11 2) [ cylinder 4 3 ]
        -- shaft
        , translate (V3 0 0 (len/2)) $ shaftWithFlat
        ]
      , rotate3 (V3 0 0 (pi/4)) $ cube True (V3 cornerSide cornerSide 100)
      ]
      where
        cornerSide = nemaSide * 1.25
