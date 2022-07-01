{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.List ((\\))
import Data.Tuple (swap)
import Graphics.Gloss (
  Display (InWindow),
  Picture (..),
  Point,
  black,
  green,
  play,
  red,
  white,
 )
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (Char))
import Graphics.Gloss.Interface.IO.Interact (KeyState (Down))
import System.Random (Random (randomR), StdGen, initStdGen)

main :: IO ()
main = do
  g <- initStdGen
  play
    (InWindow "Double Snake" (width, height) (0, 0))
    black
    fps
    (startState g)
    draw
    eventHandler
    update

startState :: StdGen -> GameState
startState g =
  GameState
    (Snake [(10, 10)] dirUp)
    (Snake [(5, 5)] dirDown)
    [(10, 5), (5, 10)]
    g

data GameState = GameState
  { snake1 :: Snake
  , snake2 :: Snake
  , food :: [Food]
  , generator :: StdGen
  }
  deriving (Show, Eq)

data Snake = Snake
  { body :: [Point]
  , dir :: Direction
  }
  deriving (Show, Eq)

type Food = Point

type Direction = Point

dirLeft, dirRight, dirUp, dirDown :: Direction
dirLeft = (-1, 0)
dirRight = (1, 0)
dirUp = (0, 1)
dirDown = (0, -1)

opPoints :: (Float -> Float -> Float) -> Point -> Point -> Point
opPoints f (a, b) (x, y) = (f a x, f b y)

width, height, scale :: Num a => a
width = 600
height = 600
scale = 20

scaleX, scaleY :: Fractional a => a
scaleX = width / scale
scaleY = height / scale

fps :: Int
fps = 3

-- food got eaten ->
-- move snake ->
-- check self collision ->
-- spawn new food? -> return
update :: Float -> GameState -> GameState
update _deltaTime gamestate@GameState {..}
  | hasCollided = startState generator
  | otherwise = newState
  where
    (snakesThatHaveEaten, foodEaten) = eatFood [snake1, snake2] food
    newSnakes = moveSnakes [snake1, snake2] snakesThatHaveEaten
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
  Snake (s : ss) _ -> s `elem` ss

moveSnakes :: [Snake] -> [Snake] -> [Snake] -- snakes -> snakesThatHaveEaten
moveSnakes ss zz = map moveSnake ss
  where
    moveSnake s@(Snake {..}) = let newHead = opPoints (+) dir (head body) in s {body = newHead : (hasEaten s)}
    hasEaten s = if s `elem` zz then body s else init $ body s

eatFood :: [Snake] -> [Food] -> ([Snake], [Food])
eatFood ss fs = unzip $ filter (\(a, b) -> head (body a) == b) perms
  where
    perms :: [(Snake, Food)]
    perms = (,) <$> ss <*> fs

eventHandler :: Event -> GameState -> GameState
eventHandler event gameState@GameState {..}
  | EventKey (Char 'w') Down _ _ <- event
  , dir snake1 /= dirDown =
      gameState {snake1 = snake1 {dir = dirUp}}
  | EventKey (Char 's') Down _ _ <- event
  , dir snake1 /= dirUp =
      gameState {snake1 = snake1 {dir = dirDown}}
  | EventKey (Char 'a') Down _ _ <- event
  , dir snake1 /= dirRight =
      gameState {snake1 = snake1 {dir = dirLeft}}
  | EventKey (Char 'd') Down _ _ <- event
  , dir snake1 /= dirLeft =
      gameState {snake1 = snake1 {dir = dirRight}}
  | EventKey (Char 'i') Down _ _ <- event
  , dir snake2 /= dirDown =
      gameState {snake2 = snake2 {dir = dirUp}}
  | EventKey (Char 'k') Down _ _ <- event
  , dir snake2 /= dirRight =
      gameState {snake2 = snake2 {dir = dirLeft}}
  | EventKey (Char 'j') Down _ _ <- event
  , dir snake2 /= dirRight =
      gameState {snake2 = snake2 {dir = dirLeft}}
  | EventKey (Char 'l') Down _ _ <- event
  , dir snake2 /= dirLeft =
      gameState {snake2 = snake2 {dir = dirRight}}
  | otherwise = gameState

drawSquare :: Point -> Picture
drawSquare (x, y) = Polygon [(x, y), (x + 1, y), (x + 1, y + 1), (x, y + 1)]

draw :: GameState -> Picture
draw GameState {..} =
  Pictures $
    map (Scale scaleX scaleY) $
      map (Color white . drawSquare) (body snake1)
        <> map (Color green . drawSquare) (body snake2)
        <> map (Color red . drawSquare) food
