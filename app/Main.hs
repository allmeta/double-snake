{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Main where
import Graphics.Gloss
    ( play,
      Display(InWindow), Point, black, red, Picture (..), white,green )
import Graphics.Gloss.Interface.IO.Interact (Event, KeyState (Down))
import Graphics.Gloss.Interface.IO.Game (Event(..), Key (Char))
import System.Random (Random (randomR), StdGen, initStdGen)
import Data.List ((\\))

main :: IO ()
main = do
  g <- initStdGen
  play (InWindow "Double Snake" (width, height) (0,0))
        black fps (startState g)
        draw eventHandler update


startState :: StdGen -> State
startState g = State
            (Snake [(10,10)] dirUp)
            (Snake [(5,5)] dirDown)
            [ (10,5), (5,10) ]
            g

data State = State
  { snake1 :: Snake
  , snake2 :: Snake
  , food :: [Food]
  , generator :: StdGen
  } deriving (Show, Eq)

data Snake = Snake
  {
    body :: [Point]
  , dir :: Direction
  } deriving (Show, Eq)


type Food = Point
type Direction = Point

dirLeft = (-1,0) :: Direction
dirRight = (1,0) :: Direction
dirUp = (0,1) :: Direction
dirDown = (0,-1) :: Direction

opPoints :: (Float->Float->Float)-> Point -> Point -> Point
opPoints f (a,b) (x,y) = (f a x, f b y)

width :: Num a => a
width = 600
height :: Num a => a
height = 600

scale :: Num a => a
scale = 20
scaleX :: (Num a, Fractional a) => a
scaleX = (width) / scale
scaleY :: (Num a, Fractional a) => a
scaleY = (height) / scale

fps :: Int
fps = 3

-- food got eaten ->
-- move snake ->
-- check self collision ->
-- spawn new food? -> return
update :: Float -> State -> State
update deltaTime state@State{..} = if hasCollided then (startState generator) else newState
  where
    (snakesThatHaveEaten,foodEaten) = eatFood [snake1,snake2] food
    newSnakes = moveSnakes [snake1,snake2] snakesThatHaveEaten
    hasCollided = checkCollision newSnakes
    (newGen,generatedFood) = if null foodEaten then (generator,food) else spawnFood generator foodEaten
    newFood = generatedFood ++ (food \\ foodEaten)
    newState = state{food=newFood,snake1=(newSnakes !! 0),snake2=(newSnakes !! 1),generator=newGen}

spawnFood :: StdGen -> [Food] -> (StdGen,[Food])
spawnFood g = foldr (\_ (a,bs)-> let (newFood,newG) = genRandom a in (newG,newFood:bs)) (g,[])
  where
    genRandom :: StdGen -> (Point,StdGen)
    genRandom = randomR ((0,0), (scale,scale))


checkCollision :: [Snake] -> Bool
checkCollision = any $ \case
  Snake [] _ -> True
  Snake (s:ss) _ -> s `elem` ss


moveSnakes :: [Snake] -> [Snake] -> [Snake] -- snakes -> snakesThatHaveEaten
moveSnakes ss zz = map moveSnake ss
  where moveSnake s@(Snake{..}) = let newHead = opPoints (+) dir (head body) in s{body=newHead:(hasEaten s)}
        hasEaten s = if s `elem` zz then body s else init $ body s

eatFood :: [Snake] -> [Food] -> ([Snake],[Food])
eatFood ss fs = unzip $ filter (\(a,b)->head (body a) == b) perms
  where perms = (,) <$> ss <*> fs



eventHandler :: Event -> State -> State
eventHandler event state@State{..}
  | EventKey (Char 'w') Down _ _ <- event
  , dir snake1 /= dirDown =      state { snake1=snake1{ dir=dirUp } }

  | EventKey (Char 's') Down _ _ <- event
  , dir snake1 /= dirUp =      state { snake1=snake1{ dir=dirDown } }

  | EventKey (Char 'a') Down _ _ <- event
  , dir snake1 /= dirRight =      state { snake1=snake1{ dir=dirLeft } }

  | EventKey (Char 'd') Down _ _ <- event
  , dir snake1 /= dirLeft =      state { snake1=snake1{ dir=dirRight } }

  | EventKey (Char 'i') Down _ _ <- event
  , dir snake2 /= dirDown =      state { snake2=snake2{ dir=dirUp } }

  | EventKey (Char 'k') Down _ _ <- event
  , dir snake2 /= dirRight =      state { snake2=snake2{ dir=dirLeft } }

  | EventKey (Char 'j') Down _ _ <- event
  , dir snake2 /= dirRight =      state { snake2=snake2{ dir=dirLeft } }

  | EventKey (Char 'l') Down _ _ <- event
  , dir snake2 /= dirLeft =      state { snake2=snake2{ dir=dirRight } }

  | otherwise = state

drawSquare :: Point -> Picture
drawSquare (x,y) = Polygon [(x,y),(x+1,y),(x+1,y+1),(x,y+1)]

draw :: State -> Picture
draw State{..} = Pictures $
  map (Scale scaleX scaleY) $
  map (Color white . drawSquare) (body snake1) ++
  map (Color green . drawSquare) (body snake2) ++
  map (Color red . drawSquare) food
