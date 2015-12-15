module App.Const where

-- world
gravity = (0,-1.622) -- m/s^2 moon: -1.622, earth: -9.807

maxFuel = 1000
consumptionFactor = 0.1

crashSpeed = 20 -- m/s

crashAngle = 3.81/4 -- 45 deg

startHight = 50
worldHeight = 60 -- m
worldWidth = 60 -- m

ignitionVelo = (snd gravity) * (-3.0)
rotationUnit = 3.14159265359/360
