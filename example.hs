{-# LANGUAGE Arrows #-}
import Control.Arrow
import Data.Monoid
import Data.VectorSpace

import Graphics.Rendering.OpenGL
import Graphics.FieldTrip hiding (frustum)

import FRP.Yampa.FieldTrip.Adapter
import FRP.Yampa (SF, integral, delay, edge, edgeTag, hold)
import FRP.Yampa.Event
import FRP.Yampa.GLUT.Adapter

main = anim2 (mconcat [ball >>> ballG])

ballG :: SF (Vector2 GLfloat) Geometry2
ballG = proc pos -> do
    let geom = uscale2 (0.05 :: GLfloat) *% udisk
    returnA -< (translate2 pos *% geom)

ball :: SF (Event UI) (Vector2 GLfloat)
ball = proc ev -> do
    rec
        mouse <- simpleMousePosition -< ev
        let distance = magnitude (position ^-^ mouse)
            dashAwaySpeed = normalized (position ^-^ mouse) ^* (0.6 - distance)
            tooClose = if distance < 0.5 then Event () else NoEvent

        speed <- hold zeroV -< tooClose `tag` dashAwaySpeed

        position <- integral <<< delay 0.01 zeroV -< speed

    returnA -< position
