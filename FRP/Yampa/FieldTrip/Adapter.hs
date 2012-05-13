
-- Copyright   :  (c) Nikolay Orlyuk 2012
-- License     :  GNU GPLv3 (see COPYING)

module FRP.Yampa.FieldTrip.Adapter
    ( Anim, Anim2, Anim3
    , anim2, anim3, liftAction
    ) where

import Control.Arrow
import Data.Monoid

import Graphics.FieldTrip hiding (frustum)

import Graphics.UI.GLUT

import FRP.Yampa (SF)
import FRP.Yampa.Event
import FRP.Yampa.GLUT.Adapter

-- | Interactive animation
type Anim a = SF (Event UI) a

-- | Interactive 2D animation
type Anim2 = Anim Geometry2

-- | Interactive 3D animation
type Anim3 = Anim Geometry3

-- | Present a 2D animation.
anim2 :: Anim2 -> IO ()
anim2 anim = adaptSimple "Yampa + FieldTrip 2D" leaveMainLoop sf
    where
        sf = mconcat [(redisplay &&& anim) >>> arr (uncurry tag) >>> liftAction render
                     , reshaped >>> liftAction reshape2
                     ]
        render = glwrap . renderWith2 gc
        gc = defaultGC { gcErr = 0.005 }

-- | Present a 3D animation.
anim3 :: Anim3 -> IO ()
anim3 anim = adaptSimple "Yampa + FieldTrip 3D" leaveMainLoop sf
    where
        sf = mconcat [(redisplay &&& anim) >>> arr (uncurry tag) >>> liftAction render
                     , reshaped >>> liftAction reshape3
                     ]

        render = glwrap . renderWith3 gc
        gc = defaultGC { gcErr = 0.005 }


-- | Wrap an OpenGL rendering action, to clear the frame-buffer before
-- swap buffers afterward.
glwrap :: IO () -> IO ()
glwrap f = do
    clear [ ColorBuffer, DepthBuffer ]
    f
    swapBuffers

-- | Attach IO function to Event
liftAction :: (a -> IO ()) -> SF (Event a) (Event Action)
liftAction f = arr (fmap (actionIO . f))

-- | Handles reshape event for 2D scene
reshape2 :: Size -> IO ()
reshape2 (Size w 0) = reshape3 (Size w 1) -- prevent divide by zero
reshape2 s@(Size w h) = do
    let b = fromIntegral (w `min` h)
        w' = fromIntegral w / b
        h' = fromIntegral h / b

    viewport $= (Position 0 0, s)

    matrixMode $= Projection
    loadIdentity
    ortho (-w') w' (-h') h' (-1) 1

    matrixMode $= Modelview 0
    -- loadIdentity

-- | Handles reshape event for 3D scene
reshape3 :: Size -> IO ()
reshape3 (Size w 0) = reshape3 (Size w 1) -- prevent divide by zero
reshape3 s@(Size w h) = do
    {-
    let aspect = realToFrac w / realToFrac h

    viewport   $= (Position 0 0, s)

    matrixMode $= Projection
    loadIdentity
    perspective 75 aspect 0.001 100
    
    matrixMode $= Modelview 0
    loadIdentity
    -}
    let b = fromIntegral (w `min` h) * 2
        w' = fromIntegral w / b
        h' = fromIntegral h / b

    viewport $= (Position 0 0, s)

    matrixMode $= Projection
    loadIdentity
    frustum (-w') w' (-h') h' 2 100

    matrixMode $= Modelview 0
    loadIdentity

    translate (Vector3 0 0 (-4 :: GLfloat))
