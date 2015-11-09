module App.View where

import Html as H
import Graphics.Element as E exposing (Element)
import Graphics.Collage as GC
import Transform2D as T
import Color as C exposing (Color)
import Text

import ConsoleLog exposing (log) 

import App.Collision exposing (..)

import App.Const exposing (..)
import App.Utils exposing (..)
import App.Model exposing (..)
import App.Vec exposing (..)

testIt = let poly1 = [(-15,-10),(0,15),(12,-5)]
             poly2 = [(-9,13),(6,13),(-2,22)]
         in  collision 10 (poly1, polySupport) (poly2, polySupport)

-- VIEW --
textForm : Float -> Float -> Float -> String -> GC.Form
textForm x y scl str = Text.fromString str |> Text.color C.grey
                                  |> E.centered
                                  |> GC.toForm
                                  |> GC.scale scl
                                  |> GC.move (x,y)

hudFuel : number -> GC.Form
hudFuel v = let txtE = v |> toString |> Text.fromString |> Text.color C.grey |> E.centered
                (barOutlineWidth, barOutlineHeight) = ((maxFuel |> toString |> Text.fromString |> Text.color C.grey |> E.centered |> E.widthOf |> toFloat)/2 ,txtEHeight/2)
                (txtEWidth,txtEHeight) = ( txtE |> E.widthOf |> toFloat, txtE |> E.heightOf |> toFloat )
                (txtXPos,txtYPos) = (-hWidth+(hWidth/100)+(barOutlineWidth),hHeight-(hHeight/100)-(txtEHeight/2)) 
                txtF = txtE |> GC.toForm |> GC.scale 1
                (barWidth,barHeight) = ((2*barOutlineWidth * (toFloat v)/maxFuel)-barOutlineWidth, barOutlineHeight)
                barOutlineF = GC.polygon [ (-barOutlineWidth,barOutlineHeight),(barOutlineWidth,barOutlineHeight)
                                         , (barOutlineWidth,-barOutlineHeight),(-barOutlineWidth,-barOutlineHeight)] 
                                         |> GC.outlined GC.defaultLine
                barF = GC.polygon [(-barOutlineWidth,barHeight),(barWidth,barHeight)
                                  ,(barWidth,-barHeight),(-barOutlineWidth,-barHeight)] 
                                  |> GC.filled C.blue
             in GC.move (txtXPos,txtYPos) <| (GC.group [barF,barOutlineF,txtF])

hudVel : Vec -> GC.Form
hudVel (vx,vy) = let txtE = (vx |> ((*) 100) |> truncate, vy |> ((*) 100) |> truncate) |> toString |> Text.fromString |> Text.color C.grey |> E.centered
                     (txtEWidth,txtEHeight) = ( txtE |> E.widthOf |> toFloat, txtE |> E.heightOf |> toFloat )
                     txtF = txtE |> GC.toForm |> GC.scale 1
                  in GC.move (-hWidth+(hWidth/100)+(txtEWidth/2),hHeight-3*((hHeight/100)+(txtEHeight/2))) <| (GC.group [txtF])

render : (Int, Int) -> GameState -> Element
render (w,h) gameState = 
  case gameState of
    (GameOver game) -> let formText = textForm 0 0 3 ("Game Over " ++ (toString (log "game" game).totalScore))
                              in  GC.collage width height [formText] |> E.color C.white |> E.container w h E.middle |> E.color C.lightGray

    (LevelCompleted game) -> let formText = textForm 0 0 3 ("Level Completed " ++ (toString game.level))
                             in  GC.collage width height [formText] |> E.color C.white |> E.container w h E.middle |> E.color C.lightGray

    (Paused game)  -> let formText = textForm 0 0 3 "Paused"
                      in  GC.collage width height [formText] |> E.color C.white |> E.container w h E.middle |> E.color C.lightGray
    (NewGame game) -> let formText = textForm 0 0 3 <| "New Game" ++ (toString testIt)
                      in  GC.collage width height [formText] |> E.color C.white |> E.container w h E.middle |> E.color C.lightGray
    (Playing game) -> let formHudFuel = game.rocket.fuel  |> truncate |> hudFuel
                          formHudVel = game.rocket.vel |> hudVel
                          formRocket = GC.polygon (scaleToViewPort game.rocket.hull) |> GC.filled C.lightRed |> GC.rotate game.rocket.alpha |> GC.move (scaleVecToViewPort game.rocket.pos)
                          formBase = GC.polygon (scaleToViewPort game.base.hull) |> GC.filled C.lightBlue |> GC.move game.base.pos
                          forms = [formHudFuel, formHudVel, formRocket, formBase] 
                      in  GC.collage width height forms |> E.color C.white  
                                                   |> E.container w h E.middle 
                                                   |> E.color C.lightGray
