module ILogo where

obj = translate (V3 (-35) 0 0) $ unionR 0.5 [
     translate (V3 (-10) 0 0) demoLetterI
   , translate (V3 (-2) 0 0) demoLetterM
   , translate (V3 6 0 0) demoLetterP
   , translate (V3 17 0 2) $ withRounding 1 $ cube True (V3 4 4 14)
   , translate (V3 (22.5) 0 0)
      $ differenceR 0.1 demoLetterI [
    --      translate (V3 0 0 (-2)) $ cylinder 2.1 5
    --      ,
          translate (V3 (-5) 0 (8)) $ rotate3 (V3 0 (pi/2) 0) $ cylinder 1.5 10
        , rotate3 (V3 0 0 (pi/2)) $ translate (V3 (-5) 0 (8)) $ rotate3 (V3 0 (pi/2) 0) $ cylinder 1.2 10
        ]
   , translate (V3 (30.5) 0 0) demoLetterC
   , translate (V3 (36) 0 0) $ demoLetterIt (-0.1)
   , translate (V3 (42) 0 0) $ demoLetterT
   , translate (V3 (52.9) 0 4) $ scale (V3 1.2 1 1.8) $ demoLetterC
   , translate (V3 (65) 0 0) $ demoLetterA
   , translate (V3 (75) 0 0) $ demoLetterD
  ]

demoLetterI :: SymbolicObj3
demoLetterI = demoLetterIt 0

demoLetterIt :: Double -> SymbolicObj3
demoLetterIt t =
    translate (V3 0 0 (-5))
  $ unionR 1
  [ translate (V3 0 0 5) $ withRounding 1 $ cube True (V3 4 4 10)
  , translate (V3 0 0 (13 - 13 * t)) $ sphere (3 + t)
  ]

demoLetterT :: SymbolicObj3
demoLetterT =
    translate (V3 0 0 (-5))
  $ unionR 1
  [ translate (V3 0 0 8) $ withRounding 1 $ cube True (V3 4 4 16)
  , translate (V3 0 0 11) $ withRounding 1 $ cube True (V3 8 3 2)
  ]

demoLetterA :: SymbolicObj3
demoLetterA =
  translate (V3 0 0 0)
  -- $ scale 1.2
  $ unionR 0 [
    mirror (V3 1 0 0) side
  , side
  ]
  where
    side = translate (V3 (-6) 0 0) $ withRounding 0.1 $ union
      [ translate (V3 5 0 4) $ withRounding 1 $ cube True (V3 6 4 4)
      , translate (V3 5 0 10) $ withRounding 1 $ cube True (V3 6 4 4)
      , translate (V3 0 0 (-2)) $ withRounding 1 $ cube True (V3 3 4 4)
      , translate (V3 2 0 4) $  rotate3 (V3 0 (-pi/2.3) 0) $ withRounding 1 $ cube True (V3 16 4 4)
      ]

demoLetterD :: SymbolicObj3
demoLetterD =
  translate (V3 2 0 0)
  $ unionR 0.2 [
      translate (V3 0 2 4)
    $ rotate3 (V3 (pi/2) 0 0)
    $ differenceR 2 dshape [ translate (V3 (-0) 0 0) $ scale (V3 0.6 0.6 1.1) $ dshape ]
   , translate (V3 (-1) 0 4) $ withRounding 1 $ cube True (V3 4 4.4 16.1)
   ]
  where dshape = differenceR 0 ( cylinder 8 4 ) [ translate (V3 (-10) 0 0) $ cube True (pure 20) ]

demoLetterM :: SymbolicObj3
demoLetterM =
  translate (V3 0 0 0)
  $ scale 1.2
  $ unionR 0 [
    mirror (V3 1 0 0) side
  , side
  ]
  where
    side = translate (V3 (-3) 0 0) $ union
      [ translate (V3 0 0 0) $ withRounding 1 $ cube True (V3 3 3 8)
      , translate (V3 2 0 1) $ rotate3 (V3 0 (pi/4) 0) $ withRounding 1 $ cube True (V3 5 3 2)
      ]

demoLetterP :: SymbolicObj3
demoLetterP = unionR 0.5 [
    translate (V3 0 0 (-8)) $ cylinder 2.1 12
  , translate (V3 3 0 0) $ differenceR 1 c [ scale (V3 0.4 1.1 0.4) c ]
  ]
  where c = withRounding 2 $ cube True (V3 10 4 10)

demoLetterC :: SymbolicObj3
demoLetterC = differenceR 1
    (translate (V3 0 0 0) $ differenceR 1 c [ scale (V3 0.4 1.1 0.4) c ])
    [ translate (V3 7.9 0 0) $ cube True (pure 11) ]
  where c = withRounding 2 $ cube True (V3 10 4 10)
