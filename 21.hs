module BlackJack where

import Cards
import Wrapper
import Test.QuickCheck
import System.Random

-- size hand2
-- = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
-- = 1 + size (Add (Card Jack Spades) Empty)
-- = 2 + size Empty
-- = 2 + 0
-- = 2

empty :: Hand
empty = Empty

-- Use valueRank to determine the value of the current hand.
totalValue :: Hand -> Integer
totalValue Empty                    = 0
totalValue (Add (Card rank _) hand) = valueRank rank + totalValue hand

-- fixes the value of the hand if it is more than 21 and it contains ace/s.
value :: Hand -> Integer
value Empty                    = 0
value hand 
  | totalValue hand <= 21 = totalValue hand
  | totalValue hand > 21  = totalValue hand - (10 * numberOfAces hand)

-- Values each rank, returns the value in numbers
valueRank :: Rank -> Integer
gameOver hand = value hand > 21
valueRank King        = 10
valueRank Queen       = 10
valueRank Jack        = 10
valueRank (Numeric x) = x
valueRank Ace         = 11

-- Values a Card given the rank.
valueCard :: Card -> Integer
valueCard (Card rank _) = valueRank rank 

-- Counts the number of Aces in the current hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand
numberOfAces (Add (Card _ _) hand)   = 0 + numberOfAces hand

-- If the hand is valued more than 21, gameOver is set to True.
gameOver :: Hand -> Bool

-- WHO WILL WIN!
winner :: Hand -> Hand -> Player
winner guest bank
  | gameOver guest == True    = Bank
  | gameOver bank  ==  True   = Guest
  | value guest > value bank  = Guest 
  | value guest <= value bank = Bank

-- Combines 2 different hands. (hand1 <+ hand2 = hand3)
(<+) :: Hand -> Hand -> Hand
hand1 <+ Empty          = hand1
Empty <+ hand2          = hand2
Add card hand1 <+ hand2 = Add card (hand1 <+ hand2)

-- Property, is (<+) associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

-- Property, is the size different
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = size (p1 <+ p2) == size p1 + size p2

-- Combines all the complete suits into one hand creating a deck of
-- 52 cards.
fullDeck :: Hand
fullDeck = ((completeSuit ranks Hearts   <+
           completeSuit ranks Spades)    <+
           (completeSuit ranks Diamonds  <+
           completeSuit ranks Clubs))

-- List of all the existing ranks.
ranks :: [Rank]
ranks =  [Ace, King, Queen, Jack] ++ [Numeric x | x <- [2..10]]

-- Given a suit and a list of ranks (the list of all the ranks above)
-- makes a complete set of cards for the said suit.
completeSuit :: [Rank] -> Suit -> Hand
completeSuit [] suit     = Empty
completeSuit (x:xs) suit = (Add (Card x suit) Empty) <+ completeSuit xs suit

-- Takes the first card in the deck and places it in the hand.
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand            = error "draw: The deck is empty."
draw (Add card deck) hand  = (deck, (Add card hand))

-- Start the bank AI.
playBank :: Hand -> Hand 
playBank deck = playBank' deck Empty

-- The bank will continue to draw until its hand is valued 16 or more.
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand | value bankHand < 16   = playBank' deck' bankHand'
                        | value bankHand >= 16  = bankHand
  where (deck', bankHand') = draw deck bankHand

-- Given a StdGen and a deck (or hand) it will create a random number between
-- 0 and the size of the deck/hand.
randomNum :: StdGen -> Hand -> (Integer, StdGen)
randomNum gen deck = (randomR(0, (size(deck) - 1)) gen)

-- Start the shuffling! :)
shuffle :: StdGen -> Hand -> Hand
shuffle gen deck = shuffle' gen deck Empty -- Empty == shuffled deck

-- Shuffle' is the help function for shuffle. It'll call randomNum and rndCard
-- to get random cards and numbers and continue to add them into the new deck.
shuffle' :: StdGen -> Hand -> Hand -> Hand
shuffle' gen Empty hand = hand <+ Empty
shuffle' gen deck hand  = shuffle' gen' hand' (Add card' hand)
   where (rnd', gen')   = randomNum gen deck
         (hand', card') = rndCard deck Empty rnd'

-- Picks a random card, given from randomNum
rndCard :: Hand -> Hand -> Integer -> (Hand, Card)
rndCard (Add card deck) save rnd | rnd == 0 = ((deck <+ save), card)
                                 | rnd > 0  = rndCard deck (Add card save) (rnd - 1)

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
  c `belongsTo` h == c `belongsTo` shuffle g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty    = False
c `belongsTo` Add c' h = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g Empty = False
prop_size_shuffle g h     = size(h) == size(shuffle g h)

implementation = Interface
  {  iEmpty     = empty
  ,  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

main :: IO ()
main = runGame implementation
