module App.Update (update) where

import Signal as S exposing ((<~),(~))
import Color as C exposing (Color)
import Time as T exposing (Time)
import List exposing (..)
import App.Collision exposing (..)

import App.Vec   exposing (..)
import App.Const exposing (..)
import App.Utils exposing (..)
import App.Model exposing (..)
import App.Action exposing (..)
import App.Signal exposing (..)

import ConsoleLog exposing (log) 

smoothLanded : Rocket -> Bool
smoothLanded r = (fst r.vel < crashSpeed && fst r.vel > -crashSpeed) && (snd r.vel <= 0 && snd r.vel > -crashSpeed) && (abs r.alpha < crashAngle)


-- UPDATE --
update : Action -> GameState -> GameState
update e gameState =
  case (gameState) of
    (GameOver _)  -> if e == StartGame then defaultGameState else gameState
    (LevelCompleted g) -> if e == StartGame then Playing <| nextLevel g else gameState
    (NewGame g) -> if e == StartGame then Playing g else gameState
    (Paused g) -> if e == Resume then Playing g else gameState
    (Playing g) -> if isGameOver g 
                   then gameOver g
                   else 
                    if isLevelCompleted g
                    then LevelCompleted g
                    else
                       case e of
                          Tick (time, (rot,ign)) -> 
                            let ign' = if g.rocket.fuel > 0 then ign else 0
                                gravityOnObject o = { o | acc <- vecAdd o.acc <| vecMulS gravity time }
                                ignition o = { o | acc <- vecAdd o.acc <| flip vecMulS time <| vecRot (0,ign') o.alpha }
                                velocity o = { o | vel <-  vecAdd o.vel <| vecMulS o.acc time }
                                moveObject o = { o | pos<- vecAdd o.pos <| vecMulS o.vel time }
                                rotateObject o = { o | alpha <- o.alpha-rot }
                                consumeFuel o ={ o | fuel <- o.fuel - (consumptionFactor * ign') }
                                rocket' = g.rocket |> gravityOnObject |> ignition |> rotateObject |> velocity |> moveObject |> consumeFuel
                                rocketHullOnPosition = map (\v->vecAdd v rocket'.pos) rocket'.hull
                                landed = (collision 10 (rocketHullOnPosition, polySupport) (g.base.hull,polySupport))
                            in  if landed
                                   then 
                                    if smoothLanded rocket'
                                       then LevelCompleted { g | score <- truncate g.rocket.fuel }
                                       else GameOver g
                                   else Playing <| { g | rocket <- rocket'}
                          Add {- object -} -> gameState -- Add a new object to Game
                          Pause       -> Paused g
                          _           -> Playing g -- DEFAULT NoOp
