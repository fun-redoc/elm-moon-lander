module App.Model  (..)  where

import Color as C exposing (Color)
import List exposing (..)
import App.Const exposing (..)
import App.Vec exposing (..)

--defaultVal = 15

type alias Score = Int
type alias Level = Int

type alias Polygon = List Vec

type alias Rocket = { pos:Vec, vel:Vec, acc:Vec, alpha:Float, fuel:Float, hull:Polygon}
defaultRocket = { pos = (0,startHight) 
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

type alias Game = { level:Level, score:Score, totalScore:Score, rocket:Rocket, base:Base, rocks:List Rock}
defaultGame : Game
defaultGame = {  level = 1
               , score = 0
               , totalScore = 0
               , rocket = defaultRocket
               , base = defaultBase
               , rocks = []
              }

type GameState = NewGame Game | Playing Game | Paused Game | GameOver Game | LevelCompleted Game
defaultGameState = NewGame defaultGame

isGameOver : Game -> Bool
isGameOver g = g.score < 0

gameOver : Game -> GameState
gameOver g = GameOver g

levelScoreTreshold = 10

isLevelCompleted : Game -> Bool
isLevelCompleted {score} = score == levelScoreTreshold

nextLevel : Game -> Game
nextLevel g = { defaultGame | level <- g.level + 1, score <- 0, totalScore <- g.totalScore+g.score}
