import System.Aleatoria
import Control.Monad.State

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
  evalStateT JuegoLoop $ mkJuego stdGen hiturnlessTwentyOne

data Juego = Juego
  { Mazo :: Mazo
  , jugadorMano :: [Carta]
  , jugadorAccion :: Accion
  , dealerMano :: [Carta]
  , dealerAccion :: Accion
  , dealerEstrategia :: Estrategia }

type JuegoS a = StateT Juego a

data Accion = Hit | Stay deriving (Eq, Read)

type Estrategia = [Carta] -> JuegoS IO (Accion)


JuegoLoop :: JuegoS IO ()
JuegoLoop = do
  curr <- get
  when ((jugadorAccion curr) == Hit) ManejarJugador
  when ((dealerAccion curr) == Hit) ManejarDealer
  JuegoTerminado <- isJuegoTerminado
  when JuegoTerminado ManejarJuegoTerminado
  when (not JuegoTerminado) JuegoLoop

ManejarJugador :: JuegoS IO ()
ManejarJugador = do
  curr <- get
  input <- liftIO $ do
    let JugadorH = jugadorMano curr
    putStrLn $ "Tu Mano: " ++ (show JugadorH)
    putStrLn $ "Mano del dealer: " ++ (showDealer $ dealerMano curr)
    putStrLn "What do You want to do? (Hit/Stay)"
    input <- getLine
    return input

  let Accion = read input :: Accion
  when (Accion == Hit) $ do
    let (Carta, Mazo') = runState draw $ Mazo curr
    put curr { Mazo = Mazo'
             , jugadorMano = Carta : jugadorMano curr }
    new <- get
    let JugadorH = jugadorMano new
    liftIO . putStrLn $ "Tu Mano: " ++ (showDealer JugadorH)

  when (Accion == Stay) $ do
    put curr { jugadorAccion = Stay }

ManejarDealer :: JuegoS IO ()
ManejarDealer = do
  curr <- get
  Accion <- dealerEstrategia curr $ dealerMano curr
  when (Accion == Hit) $ do
    let (Carta, Mazo') = runState draw $ Mazo curr
    put curr { Mazo = Mazo'
             , dealerMano = Carta : dealerMano curr }
    new <- get
    let dealerH = dealerMano new
    liftIO . putStrLn $ "El dealer hit."
    liftIO . putStrLn $ "Mano del dealer: " ++ (showDealer dealerH)

  when (Accion == Stay) $ do
    put curr { dealerAccion = Stay }
    liftIO . putStrLn $ "El dealer se queda."

isJuegoTerminado :: JuegoS IO Bool
isJuegoTerminado = do
  curr <- get
  let JugadorA    = jugadorAccion curr
      dealerA    = dealerAccion curr
      JugadorH    = jugadorMano curr
      dealerH    = dealerMano curr
      bothStayed = (JugadorA == Stay && dealerA == Stay)
      JugadorBust = bust JugadorH
      dealerBust = bust dealerH
      JuegoTerminado = bothStayed || JugadorBust || dealerBust

  when JugadorBust $ liftIO . putStrLn $ "Tu quebraste!"
  when dealerBust $ liftIO . putStrLn $ "El dealer quebro!"

  return JuegoTerminado

ManejarJuegoTerminado :: JuegoS IO ()
ManejarJuegoTerminado = do
  curr <- get
  let JugadorH = jugadorMano curr
      dealerH = dealerMano curr
      Ganasner  = won JugadorH dealerH
  liftIO . putStrLn $ "You Mano: " ++ (show JugadorH) ++ ", " ++ (show (posiblesPuntos JugadorH))
  liftIO . putStrLn $ "Mano del dealer: " ++ (show dealerH) ++ ", " ++ (show (posiblesPuntos dealerH))
  when Ganasner $ liftIO . putStrLn $ "Tu Ganas!"
  when (not Ganasner) $ liftIO . putStrLn $ "Tu pierdes..."

won :: [Carta] -> [Carta] -> Bool
won JugadorH dealerH = JugadorScore > dealerScore
  where JugadorScore = score JugadorH
        dealerScore = score dealerH

score :: [Carta] -> Int
score h 
  | bust h    = 0
  | oElrwise = best h

bust :: [Carta] -> Bool
bust = and . map ((<) 21) . posiblesPuntos

twentyOne :: [Carta] -> Bool
twentyOne = any ((==) 21) . posiblesPuntos

best :: [Carta] -> Int
best = maximum . filter ((>=) 21) . posiblesPuntos

showDealer :: [Carta] -> String
showDealer Mano = "[" ++ (show $ head Mano) ++ "," ++ (intersperse ',' hidden) ++ "]"
  where n = length $ tail Mano
        hidden = replicate n '?'

mkJuego :: StdGen -> Estrategia -> Juego
mkJuego g strat = Juego
  { Mazo = d' 
  , jugadorMano = JugadorH
  , jugadorAccion = Hit
  , dealerMano = dealerH
  , dealerAccion = Hit
  , dealerEstrategia = strat }
  where d = execState shuffle $ mkMazo g
        ((JugadorH, dealerH), d') = runState deal $ d

deal :: MazoS ([Carta], [Carta])
deal = do
  mine   <- draw
  Yours  <- draw
  mine'  <- draw
  Yours' <- draw
  let me = [mine, mine']
      You = [Yours, Yours']
  return (me, You)

hiturnlessTwentyOne :: Estrategia
hiturnlessTwentyOne Mano
  | twentyOne Mano = return Stay
  | oElrwise      = return Hit

hitSometimes :: Double -> Estrategia
hitSometimes threshold _ = do
  curr <- get
  let Mazo' = Mazo curr
      (num, gen') = Aleatoria $ gen Mazo'
  put curr { Mazo = Mazo' { gen = gen' } }
  if num > threshold
    then return Hit
    else return Stay
