module App.Model  (..)  where

import Color as C exposing (Color)
import List exposing (..)
import Maybe exposing (..)

import App.Const exposing (..)
import App.Vec exposing (..)
import App.Levels as Levels exposing (..)

import ConsoleLog exposing (log)

--defaultVal = 15

type alias Score = Int
type alias Level = Int

type alias Polygon = List Vec

type alias Rocket = { pos:Vec, vel:Vec, acc:Vec, alpha:Float, fuel:Float, hull:Polygon}
defaultRocket = { pos = (0,0) --startHight) 
                , vel = (0,0)
                , acc = (0,0)
                , alpha = 0
                , fuel = maxFuel
                , hull = [(-2.0,0.0), (0.0, 2.0), (2.0, 0.0), (2.0,-2.0), (-2.0,-2.0)]
--                , hull = [(-4.0,2.0),(4.0,2.0),(4.0,0.0),(-4.0,0.0)]
                }

type alias Base = { pos:Vec, hull:Polygon}
defaultBase = { pos = (0,0)
              , hull = [(-4.0,2.0),(4.0,2.0),(4.0,0.0),(-4.0,0.0)]
              }

type alias Rock = { pos:Vec, hull:Polygon }
defaultRock = { pos = (0,0), hull = [(-3.5,3.5),(3.5,3.5),(3.5,-3.5),(-3.5,-3.5)] }

type alias Game = { level:Level, score:Score, totalScore:Score, rocket:Rocket, base:Base, rocks:List Rock}
defaultGame : Game
defaultGame = {  level = 0
               , score = 0
               , totalScore = 0
               , rocket = defaultRocket
               , base = defaultBase
               , rocks = [] -- rocksForLevel 1
              }
rocksForLevel k = List.map (\(x,y) -> {defaultRock | pos <- (x*(worldWidth),(y+0.5)*(worldHeight)) }) (Levels.level k).rocks
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

--isLevelCompleted : Game -> Bool
--isLevelCompleted {score} = score == levelScoreTreshold

nextLevel : Game -> Game
nextLevel g = { g | level <- g.level + 1, score <- 0, totalScore <- g.totalScore+g.score,
                    rocks <- rocksForLevel (g.level + 1),
                    rocket <- rocketForLevel (g.level + 1),
                    base <- baseForLevel (g.level + 1) }
