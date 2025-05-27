{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid reverse" #-}
module CourseworkOne where

import Halatro.Constants
import Halatro.Types
-- Reference : researched methods on Hoogle
import Data.List (sort,maximumBy, sortOn, subsequences)
import Data.Ord (comparing)

--------------------------------------------------------------------------------
-- Part 1: check whether a played hand is a certain hand type

-- Function to check if a hand contains a specific hand type
contains :: Hand -> HandType -> Bool
contains hand None = null hand -- An empty hand is classified as None
-- only returns True for HighCard if all other handtypes are false
contains hand HighCard = not (or [contains hand x | x <- [Pair, TwoPair, ThreeOfAKind, Straight, Flush, FullHouse, FourOfAKind, StraightFlush]])
contains hand Pair = sameRank 3 hand || sameRank 2 hand -- A pair is classified if we have two or three of the same rank
-- A TwoPair is classified if we have two distinct pairs or a full house
contains hand TwoPair = contains hand FullHouse || (length [x | x <- sortedHand hand, length (filter (== x) (sortedHand hand)) == 2] == 4) 
contains hand ThreeOfAKind = sameRank 3 hand -- A pair is classified if we have three of the same rank
-- Straight checks if the sorted hand is equal to the ordered 5 numbers where the first number is the lowest number in the hand or its a special case
contains hand Straight = (sortedHand hand == take 5 [head (sortedHand hand) ..] || sortedHand hand == [Two, Three, Four, Five, Ace]) 
        && length hand == 5 -- A straight is classified by checking for consecutive values (including low-ace straight)
contains hand Flush = or [length (filter (== x) [s | Card _ s <- hand]) == 5 | x <- [s | Card _ s <- hand]]  -- All suits must be the same
-- extracts the suit and checks if it is the same suit and appears exactly 5 times
contains hand FullHouse = sameRank 2 hand && sameRank 3 hand -- A FullHouse is classified if we have threeOfAkind and a pair
contains hand FourOfAKind = sameRank 4 hand -- A fourOfAKind is classified if we have four of the same rank
contains hand StraightFlush = contains hand Straight && contains hand Flush --A straightFlush is classified if we have a straight and a flush
contains hand RoyalFlush = contains hand StraightFlush && sortedHand hand == [Ten, Jack, Queen, King, Ace] -- a royalFlush is classified if we have a specific straight flush

-- Function to sort cards by rank
sortedHand :: Hand -> [Rank]
sortedHand hand = sort [r | Card r _ <- hand]

-- Function to check if a hand has 'n' number of same rank cards
sameRank :: Int -> Hand -> Bool
sameRank n hand = or [length (filter (== x) (sortedHand hand)) == n | x <- sortedHand hand] 
-- it filters out the cards and only include the cards that have the same rank that appear n times

--------------------------------------------------------------------------------
-- Part 2: identify the highest value hand type in a played hand

-- Function to determine the best Hand Type to play
bestHandType :: Hand -> HandType
bestHandType hand 
        | null hand = None -- if hand doesn't contain any cards, return None
        | otherwise = head [x | x <- handTypes, contains hand x] 
        -- checks starting from the best hand type if hands contain that type and returns the first one that is true
    where
        handTypes = [RoyalFlush, StraightFlush, FourOfAKind, FullHouse, Flush, Straight, ThreeOfAKind, TwoPair, Pair, HighCard, None]
        -- order in which hand types are checked

--------------------------------------------------------------------------------
-- Part 3: score a played hand

-- Function to determine which cards contribute to the score
whichCardsScore :: Hand -> [Card]
whichCardsScore hand
        | bestHandType hand == HighCard = 
            [x | x <- hand, rank x == maximum [rank y | y <- hand]]
            -- returns the card which has the highest rank
        | bestHandType hand == Pair =
            [ x | x <- hand, length (filter (\y -> rank y == rank x) hand) >= 2]
            -- returns the cards which have atleast 2 or more cards with the same rank
        | bestHandType hand == ThreeOfAKind =
            [ x | x <- hand, length (filter (\y -> rank y == rank x) hand) == 3]
            -- returns the cards which have exactly 3 cards of the same rank
        | bestHandType hand `elem` [FourOfAKind, TwoPair] =
            [ x | x <- hand, length (filter (\y -> rank y == rank x) hand) > 1]
            -- returns the cards which have atleast 2 or more cards with the same rank (last scenario since already tested for Pair,ThreeOfAKind etc)
        | bestHandType hand `elem` [RoyalFlush, StraightFlush, FullHouse, Flush, Straight] = hand
        -- returns the hand if hand type uses 5 cards
        | otherwise = []
        -- returns an empty list if hand is empty

-- Function to calculate the score of a hand
scoreHand :: Hand -> Int
-- follows the formula to first sum all the constant values of the cards that are scored and the value of the handtype and then multiply it with the multiplier
scoreHand hand = (sum [rankScore r | Card r _ <- cardScored] + fst(handTypeValues handTypeScored)) * snd(handTypeValues handTypeScored)
    where 
        cardScored = whichCardsScore hand -- calls the whichCardsScore function
        handTypeScored = bestHandType hand -- calls the bestHandType function

--------------------------------------------------------------------------------
-- Part 4: find the highest scoring hand of 5 cards out of n>=5 cards

-- Function to get the best hand from the 8 or more cards in the halatro game
highestScoringHand :: [Card] -> Hand
highestScoringHand cards 
        | length cards <= 5 = cards -- if there is less than 5 cards, it returns all the cards
        | otherwise = maximumBy (comparing scoreHand) . filter ((== 5) . length) $ subsequences cards
        -- if there is more than 5 cards, it checks against all the subsequences of 5 cards and calculates its score and returns the hand with the maximum score

--------------------------------------------------------------------------------
-- Part 5: implement an AI for maximising score across nds and 3 discards

-- Function to play the highest 5 cards
simpleAI :: [Move] -> [Card] -> Move
-- sorts the cards from lowest to highest from card ranks then reverses it and plays the first five cards
simpleAI _ cards = Move Play (take 5 $ reverse $ sort cards )

-- Function to always play the best hand type
sensibleAI :: [Move] -> [Card] -> Move
-- calls the highestScoringHand function which returns the best hand and then plays it
sensibleAI _ cards = Move Play (highestScoringHand cards)

-- Function that chooses the best move whether to play or discard based on the hand score
myAI :: [Move] -> [Card] -> Move
myAI moves cards
    | scoreHand (highestScoringHand cards) > 190 = Move Play (highestScoringHand cards)
    | otherwise = backupMove moves cards -- calls the backupMove function to decide what move to play if score is less than 190

-- Function to decide which cards to play or discard
backupMove :: [Move] -> [Card] -> Move
backupMove moves cards
    -- if there is a possibility of a flush (4 cards of the same suit), it discards the 4 other cards trying to get that suit if discard option is available
    | length possibleFlush == 4 && length cards > 4 && countDiscards moves < 3 = Move Discard (filter (`notElem` possibleFlush) cards)
    -- if discard option is not available, it would play the best hand type
    | countDiscards moves >= 3 = Move Play (highestScoringHand cards)
    -- if discard option is available, it would discard the lowest 4 hands that don't contribute to the best hand type
    | otherwise = Move Discard (take 4 sortedCards)
  where
    remaining = filter (`notElem` whichCardsScore (highestScoringHand cards)) cards -- store the cards which are not used in the best hand type
    sortedCards = sortOn rank remaining -- sorts the cards which are not used in the best hand type
    countDiscards moves = length [() | Move Discard _ <- moves] -- includes () in a list whenever a discard move is played to keep track of this move's count
    possibleFlush = [x | x <- cards, length (filter (\y -> suit y == suit x) cards) == 4] -- stores the cards which have the same suit and appear exactly 4 times
