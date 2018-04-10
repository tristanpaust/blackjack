module Blackjack where

-- Data Types
data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Enum, Eq)
data Royalty = King | Queen | Jack
  deriving (Enum,Eq)
data Card = RoyalCard Royalty Suit | IntCard Int Suit | AceCard String Suit
  deriving (Eq)

type Deck = [Card]
type Hand = [Card]

-- Helper function to remove the quotes around the A for Aces
-- https://stackoverflow.com/questions/12102874/haskell-suppress-quotes-around-strings-when-shown
newtype PlainString = PlainString String
instance Show PlainString where
  show (PlainString s) = s

-- New instances for Show
instance Show Suit where 
  show Hearts = "\x2661"
  show Diamonds = "\x2662"
  show Clubs = "\x2663"
  show Spades = "\x2660"

instance Show Royalty where
  show King = "K"
  show Queen = "Q"
  show Jack = "J"

instance Show Card where
  show (RoyalCard v s) = concat [show v, show s] 
  show (IntCard v s) = concat [show v, show s] 
  show (AceCard v s) = concat [show (PlainString "A"), show s] 

-- Building a full deck
suits :: [Suit]
suits = [Hearts .. Spades]
faces :: [Royalty]
faces = [King, Queen, Jack]
numbers :: [Int]
numbers = [2 .. 10]

allNumbers :: [Card]
allNumbers = [ IntCard number suit | number <- numbers, suit <- suits ]
allFaces = [ RoyalCard face suit | face <- faces, suit <- suits]
allAces = [ AceCard "Ace" suit | suit <- suits]

fullDeck :: Deck
fullDeck = allNumbers ++ allFaces ++ allAces
-- [2♡,2♢,2♣,2♠,3♡,3♢,3♣,3♠,4♡,4♢,4♣,4♠,5♡,5♢,5♣,5♠,6♡,6♢,6♣,6♠,7♡,7♢,7♣,7♠,8♡,8♢,8♣,8♠,9♡,9♢,9♣,9♠,10♡,10♢,10♣,10♠,K♡,K♢,K♣,K♠,Q♡,Q♢,Q♣,Q♠,J♡,J♢,J♣,J♠,A♡,A♢,A♣,A♠]

data Score = Score [Int]
  deriving Show

scoreValue :: Score -> Int
scoreValue (Score ([])) = 0
scoreValue (Score (x:xs)) = x + scoreValue (Score xs)

improveScore :: Score -> Score
improveScore (Score (x:xs)) = if (scoreValue (Score (x:xs)) > 21) then (Score [x,0]) else (Score (x:xs))

-- improveScore(Score[1,30])
-- -> Score [1,0]
-- improveScore(Score[12,30])
-- -> Score [12,0]
-- improveScore(Score[21,30])
-- -> Score [21,0]
-- improveScore(Score[11,10])
-- -> Score [11,10]
-- scoreValue(Score[11,10])
-- -> 21

instance Monoid Score where
  Score x `mappend` Score y = Score (zipWith (+) x y)
  mempty = Score []
-- Score [2,13] `mappend` Score [3,3]
-- -> Score [5,16]
-- Score [2,13] `mappend` Score [10,12] `mappend` Score[3,3]
-- -> Score [15,28]
-- TODO: EMPTY WITH NON-EMPTY LIST

-- HANDS FOR TESTING
notBusted :: Hand
notBusted = [IntCard 4 Spades, RoyalCard Queen Clubs]
notBustedIfImproved :: Hand
notBustedIfImproved = [IntCard 7 Diamonds, RoyalCard King Hearts, AceCard "Ace" Spades]
definitelyBusted :: Hand
definitelyBusted = [IntCard 9 Hearts, RoyalCard Jack Clubs, RoyalCard King Spades]

cardScore :: Card -> Score
cardScore (RoyalCard v _) = Score [10,0]
cardScore (AceCard v _) = Score [1,10]
cardScore (IntCard v _) = Score [v,0]
-- cardScore is a list with two elements, where the first element is the hard score and the second element is the soft score that can be removed
-- The suit does not influence the cardScore, hence the underscore

-- cardScore (IntCard 7 Diamonds)
-- -> Score [7,0]
-- cardScore (AceCard "Ace" Hearts)
-- -> Score [1,10]
-- cardScore (RoyalCard Queen Spades)
-- -> Score [10,0]

handScore :: Hand -> Score
handScore [] = Score [0,0]
handScore (x:xs) = cardScore x `mappend` handScore xs
-- handScore newHand = 7 Diamonds + King Hearts + Ace of Spades = [(7 + 10 + 1), 10] = [18,10]
-- -> Score [18,10]

handValue :: Hand -> Int
handValue [] = 0
handValue x = scoreValue(improveScore(handScore x))
-- handValue notBusted 
-- -> 14
-- handValue notBustedIfImproved
-- -> 18
-- handValue definitelyBusted
-- -> 29

--- Ordered Tuples used for shuffling lists, e.g the full deck of cards
data Indexed i a = Indexed (i,a)
  deriving Show

-- Indexed ("a",1)
-- -> Indexed ("a",1)
-- Indexed (12412,1)
-- -> Indexed (12412,1)
-- Indexed ([1,2,3,4,5],1)
-- -> Indexed ([1,2,3,4,5],1)

instance (Eq i) => Eq (Indexed i a) where
  (Indexed (i1,a1)) == (Indexed (i2,a2)) = i1 == i2

instance (Ord i) => Ord (Indexed i a) where
    (Indexed (i1,a1)) > (Indexed (i2,a2)) = i1 > i2
    (Indexed (i1,a1)) <= (Indexed (i2,a2)) = i1 <= i2