module App.Model  (..)  where

import Color as C exposing (Color)
import List exposing (..)
import Maybe as Maybe exposing (..)

import App.Const exposing (..)
import App.Vec exposing (..)
import App.Levels as Levels exposing (..)

import ConsoleLog exposing (log)

--defaultVal = 15

type alias Score = Int
type alias Level = Int

type alias Polygon = List Vec

type Direction = Left | Right | Up | Down

type alias Rocket = { ignition:Maybe Direction, size:Vec, pos:Vec, vel:Vec, acc:Vec, alpha:Float, fuel:Float, hull:Polygon, ignitionHull:Polygon}
(rocketWorldX, rocketWorldY) = (100,100)
scaleRocketHull = List.map (\(x,y) -> (x*rocketWorldX/(worldWidth*0.5),y*rocketWorldY/(worldHeight*0.5))) -- the default rocket hull is defined within a 100x100 world
defaultRocket = { ignition = Nothing
                , size = (0,0)
                , pos = (0,0) --startHight) 
                , vel = (0,0)
                , acc = gravity -- (0,0)
                , alpha = 0
                , fuel = maxFuel
                , hull = [(-1.0,0.0), (0.0, 1.0), (1.0, 0.0), (1.0,-1.0), (-1.0,-1.0)] |> scaleRocketHull
                , ignitionHull = [(-0.5,-1.0),(0.5,-1.0),(0.0,-1.5)] |> scaleRocketHull
--                , hull = [(-4.0,2.0),(4.0,2.0),(4.0,0.0),(-4.0,0.0)]
                }

type alias Base = { pos:Vec, hull:Polygon}
defaultBase = { pos = (0,0)
              , hull = [(-4.0,2.0),(4.0,2.0),(4.0,0.0),(-4.0,0.0)]
              }

type alias Rock = { pos:Vec, hull:Polygon }
defaultRock = { pos = (0,0), hull = [(-3.5,3.5),(3.5,3.5),(3.5,-3.5),(-3.5,-3.5)] }

type alias Game = { t:Float, dt:Float, fps: Float, level:Level, score:Score, totalScore:Score, rocket:Rocket, base:Base, rocks:List Rock}
defaultGame : Game
defaultGame = {  fps = 0
               , t = 0.0
               , dt = 0.0
               , level = 0
               , score = 0
               , totalScore = 0
               , rocket = defaultRocket
               , base = defaultBase
               , rocks = [] 
              }

rocksForLevel k = let level = (Levels.level k)
                      rocks = level.rocks
                      (w,h) = level.size
                      (fw, fh) = (toFloat (w-1), toFloat (h-1))
                      (hw, hh) = (worldWidth*0.5/fw, worldHeight*0.5/fh)
                      hull = [(-hw,hh),(hw,hh), (hw,-hh),(-hw,-hh)]
                   in  rocks |> List.map (\(x,y) -> {pos = (x*(worldWidth),(y+0.5)*(worldHeight)),  hull = hull})

baseForLevel k =    (Levels.level k).base |> log "base"
                 |> List.foldl (\(x,y) ((maxx,maxy),(minx,miny)) -> (((max x maxx),(max y maxy)),((min x minx),(min y miny)))) ((-1,-1),(1,1))
                 |> log "min max"
                 |> (\((maxx,maxy),(minx,miny)) -> (((maxx+minx)/2,(maxy+miny)/2),((maxx,maxy+0.05),(minx,miny-0.05))))
                 |> (\((centerx,centery),((maxx,maxy),(minx,miny))) -> {defaultBase | pos <- log "pos" (centerx*(worldWidth),(centery+0.5)*worldHeight),
                                                                                      hull <- List.map (\(x,y)->(x*worldWidth,(y+0.5)*(worldHeight))) [(minx, maxy),(maxx,maxy),(maxx, miny), (minx,miny)]})

rocketForLevel k = Maybe.withDefault defaultRocket (List.head <| List.map (\(x,y) -> {defaultRocket | pos <- (x*(worldWidth),(y+0.5)*(worldHeight)) }) (Levels.level k).rocket)

type GameState = NewGame Game | Playing Game | Paused Game | GameOver Game | LevelCompleted Game
defaultGameState = NewGame <| nextLevel <| defaultGame

isGameOver : Game -> Bool
isGameOver g = g.score < 0

gameOver : Game -> GameState
gameOver g = GameOver g

levelScoreTreshold = 10

nextLevel : Game -> Game
nextLevel g = { g | level <- g.level + 1, score <- 0, totalScore <- g.totalScore+g.score,
                    rocks <- rocksForLevel (g.level + 1),
                    rocket <- rocketForLevel (g.level + 1),
                    base <- baseForLevel (g.level + 1) }
