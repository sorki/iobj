
atCorners :: V3 Double -> SymbolicObj3 -> SymbolicObj3
atCorners (V3 w h z) o = union [
    translate (V3 (i * w/2) (j * h/2) z) o | i <- [-1, 1], j <- [-1, 1]
  ]

upVector :: V3 Double
upVector = unit _z

downVector :: V3 Double
downVector = -1 *^ upVector

rightVector :: V3 Double
rightVector = unit _x

leftVector :: V3 Double
leftVector = -1 *^ rightVector

forwardVector :: V3 Double
forwardVector = unit _y

backwardVector :: V3 Double
backwardVector = -1 *^ forwardVector

up by = translate $ by *^ upVector
down by = translate $ by *^ downVector
right by = translate $ by *^ rightVector
left by = translate $ by *^ leftVector
fwd by = translate $ by *^ forwardVector
back by = translate $ by *^ backwardVector

d2r = (*(pi / 180))

rotX by = rotate3V by rightVector
rotXd = rotX . d2r

rotY by = rotate3V by forwardVector
rotYd = rotY . d2r

rotZ by = rotate3V by upVector
rotZd = rotZ . d2r

unitCube = cube True (pure 1)
centerCube s = cube True (pure s)

grid (V2 w h) (V2 cx cy) o = union [
  translate (V3 (i * (w / cx)) (j * (h / cy)) 0) o
  | i <- [0..cx]
  , j <- [0..cy]
  ]
