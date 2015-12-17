module App.Update (update) where

import Signal as S exposing ((<~),(~))
import Color as C exposing (Color)
import Time as T exposing (Time)
import List as L exposing (..)
import App.Collision exposing (..)

import App.Vec   exposing (..)
import App.Const exposing (..)
import App.Utils exposing (..)
import App.Model exposing (..)
import App.Action exposing (..)
import App.Signal exposing (..)

import ConsoleLog exposing (log) 

smoothLanded : Rocket -> Bool
smoothLanded r = (fst r.vel < crashSpeed && fst r.vel > -crashSpeed) && (snd r.vel <= 0 && snd r.vel > -crashSpeed) && ((log "alpha" (abs r.alpha)) < crashAngle)


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
                       case e of
                          Tick (frameTime, (rot,ign)) -> 
                            let ign' = if g.rocket.fuel > 0 then ign else 0
                                totalFrameTime = g.remainingFrameTime + (frameTime)
                                iterations =  truncate (totalFrameTime/dT)
                                acceleration o = {o | acc <- vecRot (0,ign') o.alpha}
                                ignition o = { o | ignition <- if ign' > 0 then Just Up else Nothing }
                                rotateObject o = { o | alpha <- o.alpha-rot }
                                consumeFuel o ={ o | fuel <- o.fuel - (consumptionFactor * ign') }
                                rocket' = g.rocket |> rotateObject |> ignition |> acceleration |> consumeFuel
                                rocket'' = List.foldl (\i accum->integrateRK4 dT accum) rocket' (List.repeat iterations True)
                                hullOnPosition o = map (\v->vecAdd v o.pos) o.hull
                                rocketHullOnPosition = hullOnPosition rocket''
                                landed = (collision 10 (rocketHullOnPosition, polySupport) (g.base.hull,polySupport))
                                crashed p1 p2 = collision 10 (p1, polySupport) (p2, polySupport)
                                rocketCrashedOnRock = crashed rocketHullOnPosition
                                rocketCrashedOnRocks = L.any (\rock -> rocketCrashedOnRock <| hullOnPosition rock) g.rocks
                                fps = (1/totalFrameTime) 
                            in  if landed -- TODO this test must all happen in the iteration!!!
                                   then 
                                    if smoothLanded rocket''
                                       then LevelCompleted { g | score <- truncate g.rocket.fuel }
                                       else GameOver g
                                   else if rocketCrashedOnRocks 
                                           then GameOver g
                                           else Playing <| { g | remainingFrameTime <-  totalFrameTime - (toFloat iterations)*dT, t<-g.t+frameTime, fps <- fps, rocket <- rocket''}
                          Add {- object -} -> gameState -- Add a new object to Game
                          Pause       -> Paused g
                          _           -> Playing g -- DEFAULT NoOp
