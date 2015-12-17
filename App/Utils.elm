module App.Utils where

import List exposing (map)
import Transform2D exposing (..)

import App.Const exposing (..)
import App.Vec exposing (..)

type alias MousePosition = (Int, Int)
relativeMouse : MousePosition -> MousePosition -> MousePosition
relativeMouse (ox,oy) (x,y) = (x - ox, oy - y)

center : (Int,Int) -> (Int,Int)
center (x,y) = (x//2,y//2)

--scaleToViewPort l = map (\(x,y)->(x*hWidth,y*hHeight)) l
--scaleVecToViewPort (x,y) = (x*hWidth,y*hHeight)


toList : a -> List a
toList x = [x]

--transformWorld (vw,vh) (ww,wh) = multiply (translation 0 (-vh)) (scale (2*((max vw vh)/(max ww wh)))) 
transformWorld (vw,vh) (ww,wh) =  (translation 0 (-vh)) `multiply` ((scaleX (2*vw/ww)) `multiply` (scaleY (2*vh/wh)))


integrateEuler dt ({pos,vel,acc} as obj) =
  let acc' = gravity `vecAdd` acc
      vel' = vecIntegrate vel acc' dt
      pos' = vecIntegrate pos vel' dt
   in { obj | pos <- pos', vel <- vel' }

integrateRK4 dt obj =
  let accel dt obj = gravity `vecAdd` obj.acc -- could be extended to fit the needs of the specific simulation
      deriv dt dxdt dvdt o = 
        let vel' = vecIntegrate o.vel dvdt dt
            pos' = vecIntegrate o.pos dxdt dt
            new_dxdt = vel'
            new_dvdt = accel dt o
        in ({ o | pos <- pos', vel <- vel' }, new_dxdt, new_dvdt)
      (state1, dxdt1, dvdt1) = deriv 0.0 vecNull vecNull obj
      (state2 ,dxdt2, dvdt2) = deriv (dt*0.5) dxdt1 dvdt1 state1
      (state3 ,dxdt3, dvdt3) = deriv (dt*0.5) dxdt2 dvdt2 state2
      (state4 ,dxdt4, dvdt4) = deriv (dt)     dxdt3 dvdt3 state3
      dxdt = ((dxdt1 `vecAdd`  ((dxdt2 `vecAdd` dxdt3)`vecMulS` 2.0)) `vecAdd` dxdt4) `vecMulS` (1.0/6.0)
      dvdt = ((dvdt1 `vecAdd`  ((dvdt2 `vecAdd` dvdt3)`vecMulS` 2.0)) `vecAdd` dvdt4) `vecMulS` (1.0/6.0)
  in { obj | pos <- vecIntegrate obj.pos dxdt dt, vel <- vecIntegrate obj.vel dvdt dt}
