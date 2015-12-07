module App.Levels (Game,level,Hull) where
import List as L
import Array as A exposing (Array)
import String as S
import Graphics.Element as E
import Maybe exposing (Maybe)
import ConsoleLog exposing (log)


type alias AsciiLevel = { maxFuel:Float, playField:List String }
defaultAsciiLevel = {maxFuel=0, playField=[]}
type alias Hull = List (Float,Float)
type alias Game = {maxFuel:Float,rocket:Hull, base:Hull, rocks:Hull}
defaultGame = {maxFuel=0, rocket=[], base=[], rocks=[]}

testLevel : AsciiLevel
testLevel = { maxFuel = 1000
            , playField = [ "X          X"
                          , "XX         X"
                          , "XXX      XXX"
                          , "X    @    XX"
                          , "XX         X"
                          , "X          X"
                          , "X X         "
                          , "XXX.....XXXX"
                          ]
          }

levels : List AsciiLevel
levels = [  { maxFuel = 1000
            , playField = [ "X    @     X"
                          , "XX         X"
                          , "XXX      XXX"
                          , "X         XX"
                          , "XX         X"
                          , "X          X"
                          , "X X         "
                          , "............"
                          ]
            }
         ,  { maxFuel = 2000
            , playField = [ "X   @      X"
                          , "XX         X"
                          , "XXXXXX    XX"
                          , "X         XX"
                          , "XX         X"
                          , "X          X"
                          , "X X         "
                          , "XXX.....XXXX"
                          ]
            }
         ]



fst2 (a,b,c) = a
snd2 (a,b,c) = b
trd2 (a,b,c) = c

mkCoord : List String -> (Int, Int, List (Int, Int, Char))
mkCoord = L.foldl (\row (idx,idy, res) -> 
                      let rowRes = (S.foldl (\c (res, x,y)->((x,y,c)::res, x+1, y)) (res, 0, idy) row)
                       in (max idx (snd2 rowRes), idy+1, fst2 rowRes)) (0,0, [])

toShow : List (Int,Int,Char) -> List String
toShow ls =
  let (maxx, maxy) = List.foldl (\(x,y,c) (mx,my)-> (max x mx, max y my)) (0,0) ls
      initialRow = A.initialize (maxx+1) (always ' ')
      initialLevel : Array (Array Char)
      initialLevel = A.initialize (maxy+1) (always initialRow)
      getRow : Int -> Array (Array Char) -> Array Char
      getRow y a = Maybe.withDefault initialRow (A.get y a)
      level' : Array (Array Char)
      level' = List.foldl (\(x,y,c) res -> A.set y (A.set x c (getRow y res)) res) initialLevel ls
      level = List.map (S.fromList << A.toList) (A.toList level')
   in level


cohesion2 : Char -> List String -> List (List String)
cohesion2 c ls =
  let (_,_, playField) = mkCoord ls
      isRelevant x = c == (trd2 x)
      areNeighbours (x1,y1,c) (x2,y2,k) = c==k && (((abs <| (x2 - x1)) <= 1) && ((abs <| (y2 - y1)) <= 1))
      hasNeighbour x xs = L.any (areNeighbours x) xs
      allNeighbours f fs = L.foldl (\((x,y,c) as f') (neigh, noNeigh) -> if ((areNeighbours f f') && isRelevant f') then (f'::neigh, noNeigh) else (neigh, f'::noNeigh) ) ([],[]) fs
      classify (result, rest, unclassified) = 
        case rest of
          [] -> (result, rest, unclassified)
          (h::t) -> if not <| isRelevant h
                       then classify (result, t, h::unclassified)
                       else if (L.isEmpty result) 
                               || (h `hasNeighbour` result )
                                       then let (neighbours, notNeighbours) = allNeighbours h t
                                            in  classify (neighbours ++ (h::result), t, unclassified)
                                       else classify (result, t, h::unclassified)
      classifyAll (classified, unclassified) =
        case unclassified of
          [] -> (classified, unclassified)
          (((x,y,k) as h)::t) -> let (result, rest, stillUnclassified) = classify ([h], t, [])
                                  in if k /= c then classifyAll (classified, t) else classifyAll ((toShow <| result)::classified, stillUnclassified)
  in fst <| classifyAll ([],L.filter (\(_,_,k) -> k == c) playField)

    

asciiToGame : AsciiLevel -> Game
asciiToGame {maxFuel, playField} = 
    let (maxx,maxy, asciiWithCoord) = mkCoord playField
        (fmaxx,fmaxy) = ((toFloat maxx)-1, (toFloat maxy)-1)
        --normalize (x,y) = (toFloat x, toFloat y) --let (fx,fy) = (toFloat x, toFloat y) in ((2*fx/fmaxx)-1,(2*fy/fmaxy)-1)
        normalize (x,y) = let (fx,fy) = (toFloat x, toFloat y) in ((fx/(fmaxx))-0.5,(0.5-(fy/(fmaxy))))
    in log "--> " <|   L.foldl (\(x,y,c) res-> if | c == 'X' -> { res | rocks <- ((normalize (x,y))::res.rocks)}
                                   | c == '.' -> { res | base <- ((normalize (x,y))::res.base)}
                                   | c == '@' -> { res | rocket <- ((normalize (x,y))::res.rocket) }
                                   | otherwise -> res
                ) {maxFuel=maxFuel, rocket=[], base=[], rocks=[]} asciiWithCoord

asciiLevel : Int -> Maybe AsciiLevel
asciiLevel i = L.head <| L.drop (i-1) <| levels

level : Int -> Game
level i = asciiLevel i |> Maybe.map asciiToGame |> Maybe.withDefault defaultGame


main = level 2  |> E.show
--main = cohesion 'X' testLevel |> E.show
--main = E.flow E.down <| L.map E.show <| L.concat <| cohesion2 'X' testLevel.playField



--toAscii : Game -> List String
--toAscii {rocket, base, rocks} = 
--    let mkCoordAscii : Char -> Hull -> List (Int, Int, Char)
--        mkCoordAscii c hs =  L.map (\(x,y)->(truncate x,truncate y,c)) hs
--        fs : List (Int, Int, Char)
--        fs = (mkCoordAscii '@' rocket)++(mkCoordAscii '.' base)++(mkCoordAscii 'X' rocks)
--        (maxx,maxy) = List.foldl (\(x,y,c) (mx,my)-> (max x mx, max y my)) (0,0) fs
--        initialRow : Array Char
--        initialRow = A.initialize (maxx+1) (always ' ')
--        initialLevel : Array (Array Char)
--        initialLevel = A.initialize (maxy+1) (always initialRow)
--        getRow : Int -> Array (Array Char) -> Array Char
--        getRow y a = Maybe.withDefault initialRow (A.get y a)
--        level' : Array (Array Char)
--        level' = List.foldl (\(x,y,c) res -> A.set y (A.set x c (getRow y res)) res) initialLevel fs
--        level = List.map (S.fromList << A.toList) (A.toList level')
--     in level

