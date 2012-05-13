{-# LANGUAGE Arrows #-}
import Control.Arrow

import Graphics.Rendering.OpenGL
import Graphics.FieldTrip hiding (frustum)

import FRP.Yampa.FieldTrip.Adapter
import FRP.Yampa (SF, integral)
import FRP.Yampa.Event
import FRP.Yampa.GLUT.Adapter

main = anim2 (ball >>> ballG)

ballG :: SF (GLfloat, GLfloat) Geometry2
ballG = proc (x, y) -> do
    let geom = uscale2 (0.05 :: GLfloat) *% udisk
    returnA -< (translate2 (Vector2 x y) *% geom)


ball :: SF (Event UI) (GLfloat, GLfloat)
ball = proc ev -> do
    rec
        let speed = (0.05, 0.05)
        position <- integral -< speed
    returnA -< position

{-
ball' :: SF (Event UI) (Vector2 GLfloat)
ball' = proc ev -> do
    rec
        let speed = (0.05, 0.05)
        position <- integral -< speed
    returnA -< position
-}
