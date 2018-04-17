module CardGames where
import Data.List
import System.Random
import System.Exit
import Data.Char

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
  mempty = Score []
  Score x `mappend` Score [] = Score x
  Score [] `mappend` Score x = Score x
  Score x `mappend` Score y = Score (zipWith (+) x y)
-- Score [2,13] `mappend` Score [3,3]
-- -> Score [5,16]
-- Score [2,13] `mappend` Score [10,12] `mappend` Score[3,3]
-- -> Score [15,28]
-- (Score [2,13] `mappend` Score [10,12]) `mappend` Score [3,3]
-- -> Score [15,28]
-- Score [2,13] `mappend` (Score [10,12] `mappend` Score[3,3])
-- -> Score [15,28]
-- mempty `mappend` Score [10,12]
-- -> Score [10,12]
-- Score [2,13] `mappend` mempty
-- -> Score [2,13]
-- Score [2,13] `mappend` mempty `mappend` Score[3,3]
-- -> Score [5,16]
-- Score [2,13] `mappend` Score [] `mappend` Score[3,3]
-- Score [5,16]

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

-- Indexed (1,"a") < Indexed (1,"asdas")    
-- ->False
-- Indexed (1,"a") < Indexed (3,"asdas")
-- ->True
-- Indexed (4,"a") > Indexed (3,"asdas")
-- ->True
-- Indexed (4,"a") == Indexed (4,"a")
-- -> True
-- Indexed (4,"a") == Indexed (3,"a")
-- ->False

backToString [] = []
backToString (Indexed(x,y):xs) = y:backToString xs

makeIndexedList :: [Int] -> [a] -> [(Indexed Int a)]
makeIndexedList [] (y:ys) = []
makeIndexedList (x:xs) [] = []
makeIndexedList [] [] = []
makeIndexedList (x:xs) (y:ys) =
  (Indexed(x,y)) : (makeIndexedList xs ys)

shuffle :: [Int] -> [a] -> [a]
shuffle [] (y:ys) = []
shuffle (x:xs) [] = []
shuffle [] [] = []
shuffle (x:xs) (y:ys) = backToString(sort(makeIndexedList (x:xs)(y:ys)))

-- TESTING
intList1 :: [Int]
intList1 = [4, 2, 7, 3, 6, 9] 
charList1 = "abcdef"

intList2 :: [Int]
intList2 = [12, 3, 7, 5, 1]
charList2 = "vwxyz"

-- shuffle intList1 charList1
-- -> "bdaecf"
-- shuffle intList2 charList2
-- -> "zwyxv"
-- -> shuffle intList2 charList1
-- -> "ebdca"
-- shuffle intList1 charList2
-- -> "wyvzx"

freshDeck :: IO Deck
freshDeck = do
  g <- newStdGen
  return (shuffle (take 52 $ (randoms:: StdGen -> [Int]) g) fullDeck)

dealOneCard :: Deck -> (Card, Deck)
dealOneCard (x:xs) = (x,xs)

getNewDeck = do
  putStrLn "Shuffling a new deck..."
  (y:ys) <- freshDeck  
  return (y:ys)

draw :: Deck -> IO (Card, Deck)
draw (x:xs) = return (x,xs)
draw [] = do
  (x:xs) <- getNewDeck
  return (x,xs)

hitHand :: Hand -> Deck -> IO (Hand, Deck)
hitHand hand deck = do
  (card,rest) <- (draw deck)
  return ((card:hand), rest)

deal :: Deck -> IO (Hand, Deck)
deal deck = do
  (x,xs) <- draw deck
  (y, remainder) <- draw xs
  return ([x,y], remainder)

list_to_string = unwords . map show 
-- https://stackoverflow.com/questions/30352733/outputting-a-list-in-haskell-without-the-brackets-and-over-a-range
-- show returns unicodes like so: "[4\9824,Q\9827]", 
-- putStrLn shows them like so: [4♠,Q♣], 
-- hence, we have to remove the array brackets and the comma with the above function before actually printing it

prettyPrint :: Hand -> String
prettyPrint hand = "Your hand is " ++ list_to_string(hand) ++ " (" ++ (show (handValue hand)) ++ ") What do you do?"

dealerReveals :: Hand -> String
dealerReveals hand = "The dealer reveals his hand: " ++ list_to_string(hand) ++ " (" ++ (show (handValue hand)) ++ ")"

playerBusted :: Hand -> String
playerBusted hand = "You busted! " ++ list_to_string(hand) ++ " (" ++ (show (handValue hand)) ++ ") The house wins."

showWallet :: Wallet -> String
showWallet money = "You have " ++ (show money) ++ "$ in your wallet. How much do you want to bet?"

parseMove :: String -> Maybe Move
parseMove s =
  case s of
  "" -> Just (Play)
  "hit" -> Just (Hit) 
  "stand" -> Just (Stand)
  otherwise -> Nothing

safeReadInt :: String -> Maybe Int
safeReadInt s
  | isSafe s = Just (read s) 
  | otherwise = Nothing
  where isSafe s = all isDigit s && not (null s)

prompt :: String -- query message
  -> String -- help message
  -> (String -> Maybe a) -- parse input to get an 'a' value
  -> (a -> IO b) -- reaction to the 'a' input
  -> IO b -- result after successful input

prompt query help parse act = do
  putStrLn query
  answer <- getLine
  case answer of
    "quit" -> exitSuccess
    "help" -> do
        putStrLn help
        prompt query help parse act
    otherwise -> case parse answer of
      Just result -> act result
      Nothing -> do
        putStrLn "I did not understand that"
        prompt query help parse act

data Move = Hit | Stand | Play
  deriving (Enum, Eq)

playerHits hand deck = do
  (x,xs) <- hitHand hand deck
  case compare (handValue x) 21 of
    LT -> playerTurn x xs
    EQ -> playerTurn x xs
    GT -> do
            return (x,xs)

takeAction hand deck action = 
  case action of 
        Hit -> playerHits hand deck
        Stand -> return (hand,deck)

playerTurn :: Hand -> Deck -> IO (Hand, Deck)
playerTurn hand deck = 
  let decideAction action = takeAction hand deck action in
  prompt (prettyPrint hand) "You can either \"hit\" or \"stand\"." parseMove decideAction

dealerTurn :: Hand -> Deck -> IO (Hand, Deck)
dealerTurn hand deck = do
  case compare (handValue hand) 17 of
    LT -> do
            (x,xs) <- hitHand hand deck
            dealerTurn x xs
    GT -> do 
            return (hand, deck)
    EQ -> do
            return (hand, deck)

dealInitialHands deck = do
    (dealerHand,deck') <- deal deck 
    (playerHand, deck'') <- deal deck'
    return (dealerHand, playerHand, deck'')

parts "" = [] 
parts s  = if null a then (c ++ e):parts f else a:parts b
    where
    (a, b) = break isSpace s
    (c, d) = span isSpace s
    (e, f) = break isSpace d
-- To show the dealers first card, we turn the entire hand into a string, split the string on a whitespace and return only the first part
-- https://stackoverflow.com/questions/7073321/haskell-breaking-up-words-by-first-space

showDealersFirst hand deck move = do
  putStrLn ("The dealer's first card is: " ++ head(parts(list_to_string hand)))

isBlackjack :: Hand -> Bool
isBlackjack hand =
    (((scoreValue(cardScore(head hand)) == 11) && (scoreValue(cardScore(last hand)) == 10)) || 
    ((scoreValue(cardScore(last hand)) == 11) && (scoreValue(cardScore(head hand)) == 10)))

type Wallet = Int

askForMoney :: Int -> Wallet -> IO (Int,Wallet)
askForMoney bet wallet = do
  return (bet, wallet)

addMoney :: Wallet -> Wallet -> IO Wallet
addMoney bet wallet = 
  return (wallet+bet)

deductMoney bet wallet =
  return (wallet-bet)

gameLoop :: Deck -> Wallet -> IO a
gameLoop deck wallet = do
  (bet,leftInWallet) <- let getBet bet = askForMoney bet wallet in
    prompt (showWallet wallet) "You can bet any amount between 0 and the maximum amount in your wallet" safeReadInt getBet
  
  (dealerHand, playerHand, deck) <- dealInitialHands deck -- Get initial hands
  
  let beginGame move = showDealersFirst dealerHand deck move in -- Show the dealer's first card, start a new round
    prompt "Ready?" "Press Enter to continue" parseMove beginGame

  (playerHand,playerDeck) <- playerTurn playerHand deck -- Player's turn
  if (handValue playerHand) > 21 then do
    putStrLn (playerBusted playerHand) -- Show the player his final hand and score, then restart the game
    wallet <- deductMoney bet leftInWallet
    gameLoop playerDeck wallet
  else do
    (dealerHand,dealerDeck) <- dealerTurn dealerHand playerDeck -- Dealer's turn
  
    putStrLn (dealerReveals dealerHand) -- Show dealer's hand after he is done hitting

    -- *** The above is the main functionality of the game, the below is only evaluating the winner and restarting the game *** --

  if length playerHand == 2 && isBlackjack playerHand && (length dealerHand) /= 2 -- If the player has two card and blackjack, but the bank has more than 2, the player automatically wins
  then do
    putStrLn "You win!"
    wallet <- addMoney bet leftInWallet
    gameLoop deck wallet
  else if length playerHand == 2 && isBlackjack playerHand && isBlackjack dealerHand
  then do
    putStrLn "Tie; Nobody wins."
    gameLoop deck leftInWallet -- Start new round with old wallet since we had a tie
  else if length dealerHand == 2 && isBlackjack dealerHand && length playerHand /= 2
  then do
    putStrLn "The house wins."
    wallet <- deductMoney bet leftInWallet
    gameLoop deck wallet -- Start new round with updated wallet
  else if handValue playerHand > handValue dealerHand
  then do 
    putStrLn "You win!"
    wallet <- addMoney bet leftInWallet
    gameLoop deck wallet
  else if handValue playerHand == handValue dealerHand  
  then do
    putStrLn "Tie; Nobody wins."
    gameLoop deck leftInWallet
  else do
    putStrLn "The house wins."
    wallet <- deductMoney bet leftInWallet
    gameLoop deck wallet

main :: IO ()
main = do
  putStrLn "Welcome to blackjack!"
  deck <- freshDeck
  let initialMoney = 100 :: Wallet
  gameLoop deck initialMoney

{--
  TODO: 
    - Doublecheck error handling
    - Make sure bet isn't larger than wallet
    - Checking for blackjack is not working properly right now
    - Normal score evaluation also is kind of funky still
    - What to do with fractions in case of surrender?
--}