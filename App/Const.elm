module App.Const where

-- world
gravity = (0,-9.81) -- m/s^2

maxFuel = 1000
consumptionFactor = 0.1

crashSpeed = 20 -- m/s

crashAngle = 3.81/4 -- 45 deg

startHight = 50
worldHeight = 60 -- m
worldWidth = 100 -- m

ignitionVelo = (snd gravity) * (-1.9)
rotationUnit = 2*3.81/360
