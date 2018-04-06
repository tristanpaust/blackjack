module CardGames where

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