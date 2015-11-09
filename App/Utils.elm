module App.Utils where

import List exposing (map)

import App.Const exposing (..)

type alias MousePosition = (Int, Int)
relativeMouse : MousePosition -> MousePosition -> MousePosition
relativeMouse (ox,oy) (x,y) = (x - ox, oy - y)

center : (Int,Int) -> (Int,Int)
center (x,y) = (x//2,y//2)

scaleToViewPort l = map (\(x,y)->(x*hWidth,y*hHeight)) l
scaleVecToViewPort (x,y) = (x*hWidth,y*hHeight)
