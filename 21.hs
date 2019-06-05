module Mazo 
  ( Carta
  , posiblesPuntos
  , Mazo
  , MazoS
  , Cartas
  , gen
  , mkMazo
  , draw
  , shuffle
  , tomarAleatoriaCarta
  , tomarCartaAt ) where

import System.Aleatoria
import Control.Monad.State

data Carta = Ace
          | Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          deriving (Enum, Show)

posiblesPuntos :: [Carta] -> [Int]
posiblesPuntos = go [0]
  where go n []           = n
        go ns (Ace:rest)   = go (map ((+) 1) ns ++ map ((+) 11) ns) rest
        go ns (Two:rest)   = go (map ((+) 2) ns) rest
        go ns (Three:rest) = go (map ((+) 3) ns) rest
        go ns (Four:rest)  = go (map ((+) 4) ns) rest
        go ns (Five:rest)  = go (map ((+) 5) ns) rest
        go ns (Six:rest)   = go (map ((+) 6) ns) rest
        go ns (Seven:rest) = go (map ((+) 7) ns) rest
        go ns (Eight:rest) = go (map ((+) 8) ns) rest
        go ns (Nine:rest)  = go (map ((+) 9) ns) rest
        go ns (Ten:rest)   = go (map ((+) 10) ns) rest
        go ns (Jack:rest)  = go (map ((+) 10) ns) rest
        go ns (Queen:rest) = go (map ((+) 10) ns) rest
        go ns (King:rest)  = go (map ((+) 10) ns) rest

data Mazo = Mazo
  { Cartas :: [Carta]
  , gen :: StdGen }
  deriving (Show)

mkMazo :: StdGen -> Mazo
mkMazo g = 
  Mazo { Cartas = [ Carta | Carta <- [Ace ..], _ <- [1..4] :: [Int] ]
       , gen = g }

type MazoS a = State Mazo a

draw :: MazoS Carta
draw = tomarCartaAt 0

shuffle :: MazoS ()
shuffle = do
  curr <- get
  shuffled <- replicateM 52 tomarAleatoriaCarta
  put curr { Cartas = shuffled }

tomarAleatoriaCarta :: MazoS Carta
tomarAleatoriaCarta = do
  curr <- get
  let n = length $ Cartas curr
      (i, gen') = AleatoriaR (0, n) $ gen curr
  Carta <- tomarCartaAt i
  put curr { gen = gen' }
  return Carta

tomarCartaAt :: Int -> MazoS Carta
tomarCartaAt i = do
  curr <- get
  let (Cartas', Cartas'') = splitAt (i + 1) $ Cartas curr
      Carta              = last Cartas'
      newCartas          = init Cartas' ++ Cartas''
  put curr { Cartas = newCartas }
  return car
module Main where

import Mazo

import Data.List
import System.Aleatoria
import Control.Monad.State

main :: IO ()
main = do
  stdGen <- getStdGen
  evalStateT gameLoop $ mkGame stdGen hitUnlessTwentyOne

data Juego = Juego
  { Mazo :: Mazo
  , playerHand :: [Carta]
  , playerAction :: Action
  , dealerHand :: [Carta]
  , dealerAction :: Action
  , dealerStrategy :: Strategy }

type GameS a = StateT Game a

data Action = Hit | Stay deriving (Eq, Read)

type Strategy = [Carta] -> GameS IO (Action)


gameLoop :: GameS IO ()
gameLoop = do
  curr <- get
  when ((playerAction curr) == Hit) handlePlayer
  when ((dealerAction curr) == Hit) handleDealer
  gameOver <- isGameOver
  when gameOver handleGameOver
  when (not gameOver) gameLoop

handlePlayer :: GameS IO ()
handlePlayer = do
  curr <- get
  input <- liftIO $ do
    let playerH = playerHand curr
    putStrLn $ "Your hand: " ++ (show playerH)
    putStrLn $ "Dealer's hand: " ++ (showDealer $ dealerHand curr)
    putStrLn "What do you want to do? (Hit/Stay)"
    input <- getLine
    return input

  let action = read input :: Action
  when (action == Hit) $ do
    let (Carta, Mazo') = runState draw $ Mazo curr
    put curr { Mazo = Mazo'
             , playerHand = Carta : playerHand curr }
    new <- get
    let playerH = playerHand new
    liftIO . putStrLn $ "Your hand: " ++ (showDealer playerH)

  when (action == Stay) $ do
    put curr { playerAction = Stay }

handleDealer :: GameS IO ()
handleDealer = do
  curr <- get
  action <- dealerStrategy curr $ dealerHand curr
  when (action == Hit) $ do
    let (Carta, Mazo') = runState draw $ Mazo curr
    put curr { Mazo = Mazo'
             , dealerHand = Carta : dealerHand curr }
    new <- get
    let dealerH = dealerHand new
    liftIO . putStrLn $ "The dealer hit."
    liftIO . putStrLn $ "Dealer's hand: " ++ (showDealer dealerH)

  when (action == Stay) $ do
    put curr { dealerAction = Stay }
    liftIO . putStrLn $ "The dealer stayed."

isGameOver :: GameS IO Bool
isGameOver = do
  curr <- get
  let playerA    = playerAction curr
      dealerA    = dealerAction curr
      playerH    = playerHand curr
      dealerH    = dealerHand curr
      bothStayed = (playerA == Stay && dealerA == Stay)
      playerBust = bust playerH
      dealerBust = bust dealerH
      gameOver = bothStayed || playerBust || dealerBust

  when playerBust $ liftIO . putStrLn $ "You busted out!"
  when dealerBust $ liftIO . putStrLn $ "The dealer busted out!"

  return gameOver

handleGameOver :: GameS IO ()
handleGameOver = do
  curr <- get
  let playerH = playerHand curr
      dealerH = dealerHand curr
      winner  = won playerH dealerH
  liftIO . putStrLn $ "Your hand: " ++ (show playerH) ++ ", " ++ (show (posiblesPuntos playerH))
  liftIO . putStrLn $ "Dealer's hand: " ++ (show dealerH) ++ ", " ++ (show (posiblesPuntos dealerH))
  when winner $ liftIO . putStrLn $ "You win!"
  when (not winner) $ liftIO . putStrLn $ "You lose..."

won :: [Carta] -> [Carta] -> Bool
won playerH dealerH = playerScore > dealerScore
  where playerScore = score playerH
        dealerScore = score dealerH

score :: [Carta] -> Int
score h 
  | bust h    = 0
  | otherwise = best h

bust :: [Carta] -> Bool
bust = and . map ((<) 21) . posiblesPuntos

twentyOne :: [Carta] -> Bool
twentyOne = any ((==) 21) . posiblesPuntos

best :: [Carta] -> Int
best = maximum . filter ((>=) 21) . posiblesPuntos

showDealer :: [Carta] -> String
showDealer hand = "[" ++ (show $ head hand) ++ "," ++ (intersperse ',' hidden) ++ "]"
  where n = length $ tail hand
        hidden = replicate n '?'

mkGame :: StdGen -> Strategy -> Game
mkGame g strat = Game
  { Mazo = d' 
  , playerHand = playerH
  , playerAction = Hit
  , dealerHand = dealerH
  , dealerAction = Hit
  , dealerStrategy = strat }
  where d = execState shuffle $ mkMazo g
        ((playerH, dealerH), d') = runState deal $ d

deal :: MazoS ([Carta], [Carta])
deal = do
  mine   <- draw
  yours  <- draw
  mine'  <- draw
  yours' <- draw
  let me = [mine, mine']
      you = [yours, yours']
  return (me, you)

hitUnlessTwentyOne :: Strategy
hitUnlessTwentyOne hand
  | twentyOne hand = return Stay
  | otherwise      = return Hit

hitSometimes :: Double -> Strategy
hitSometimes threshold _ = do
  curr <- get
  let Mazo' = Mazo curr
      (num, gen') = Aleatoria $ gen Mazo'
  put curr { Mazo = Mazo' { gen = gen' } }
  if num > threshold
    then return Hit
    else return Stay
