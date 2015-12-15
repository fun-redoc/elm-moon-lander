module App.Signal (event) where

import Window
import Mouse
import Keyboard
import Signal as S exposing ((<~),(~))
import Color as C exposing (Color)
import Time as T exposing (Time)
import List exposing (..)
import AnimationFrame
import Random
import Char
import Set exposing (Set)
import Dict

import App.Vec exposing (..)
import App.Const exposing (..)
import App.Action exposing (..)

import ConsoleLog exposing (log)
-- SIGNALS --
delta = T.fps 360
--spawnSignal : Signal Time
--spawnSignal = (T.every (T.second*0.5))
--delta = AnimationFrame.frame

randomX = (Random.float (-worldWidth) worldWidth) 
randomXSignal : Signal Time -> Signal (Maybe (Float, Random.Seed))
randomXSignal timeSignal = S.foldp (\t m-> case m of
                                  Nothing -> Just (Random.generate randomX (Random.initialSeed (round t))) 
                                  Just (rndx, seed) -> Just (Random.generate randomX seed))
                        Nothing timeSignal

randomRange = (Random.int 0 9)
randomColor' : Signal Time -> Signal ( Maybe ( Int, Random.Seed))
randomColor' timeSignal = S.foldp (\t m-> case m of
                                Nothing -> Just (Random.generate randomRange (Random.initialSeed (round t)))
                                Just (rndn,seed) -> Just (Random.generate randomRange seed)
                             ) Nothing timeSignal
--randomColor : Signal Time -> Signal Color
--randomColor timeSignal = (\m->Maybe.withDefault defaultColor (Maybe.map (\(v,_)->if v >= 2 then defaultColor else C.lightBlue) m ))<~(randomColor' timeSignal)

pauseOrRun : Signal Action
pauseOrRun = Signal.dropRepeats <|  (Signal.map (\b->if b then Resume else Pause) <| (Signal.foldp (\b s-> xor b s) True Keyboard.space))

startGame : Signal Action
startGame = Signal.dropRepeats <|  Signal.map (\b->if b then StartGame else NoOp) (Signal.map (Set.member <| Char.toCode 'S') Keyboard.keysDown)

arrows = Dict.fromList [(37,(-rotationUnit,0)),(38,(0,ignitionVelo)),(39,(rotationUnit,0))]
mapArrow : Int -> (Float,Float)
mapArrow k = Dict.get k arrows |> Maybe.withDefault (0,0)
--keysToActions : Set Int -> Set (Float,Float)
--keysToActions = Set.map mapArrow
ignition : Signal (Float,Float)
ignition = (Signal.map ((Set.foldl (\(x,y) (ax,ay)-> (ax+x,ay+y)) (0,0)) << (Set.map mapArrow)) Keyboard.keysDown)

inputSignal = (,) <~ (T.inSeconds <~ delta)
--                   ~  S.sampleOn delta (S.map2 relativeMouse (S.map center Window.dimensions) Mouse.position) -- the main signal is the time signal, the other signals are sampled on the time signal, when you try without the pill will not fall continuously but according to how fast the mouse is moved
                   ~ S.sampleOn delta ignition


event : Signal Action
event =  S.dropRepeats (S.mergeMany [ startGame
                                                  , pauseOrRun
                                                  , Tick <~ inputSignal
--                     , Add <~ ((\maybeRandomX color-> Maybe.withDefault defaultPill 
--                                                                         (Maybe.map (\randomX->{defaultPill| pos <- (fst randomX,snd defaultPill.pos), col<-color}) maybeRandomX)
--                              )<~(randomXSignal  spawnSignal)~(randomColor spawnSignal))
--                     , (\s p ((Ignition (x,y)) as i)-> if (x,y) == (0,0) then (if s == NoOp then p else s) else i) <~ startGame ~ pauseOrRun ~ ignition
--                       , (\s p->if s == NoOp then p else s) <~ startGame ~ pauseOrRun
                     ])
