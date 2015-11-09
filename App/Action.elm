module App.Action (Action(..)) where

import Time as T exposing (Time)

import App.Model exposing (..)
import App.Utils exposing (..)

-- ACTIONS -- 
type Action = NoOp | Tick (Time, (Float,Float)) | Add {- some object from model -} | Pause | Resume | StartGame
