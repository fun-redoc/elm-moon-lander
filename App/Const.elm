module App.Const where


dT = 1/120

-- world
gravity = (0,-1.622) -- m/s^2 moon: -1.622, earth: -9.807

maxFuel = 1000
consumptionFactor = 0.1

crashSpeed = 10 -- m/s

crashAngle = 3.1415926535/36 -- 5 deg

startHight = 50
worldHeight = 60 -- m
worldWidth = 60 -- m

ignitionVelo = (snd gravity) * (-3.0)
rotationUnit = 4*3.14159265359/360
