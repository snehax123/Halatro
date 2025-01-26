module CourseworkOne where

import Halatro.Constants
import Halatro.Types

--------------------------------------------------------------------------------
-- Part 1: check whether a played hand is a certain hand type

contains :: Hand -> HandType -> Bool
contains = error "Not implemented"

--------------------------------------------------------------------------------
-- Part 2: identify the highest value hand type in a played hand

bestHandType :: Hand -> HandType
bestHandType = error "Not implemented"

--------------------------------------------------------------------------------
-- Part 3: score a played hand

whichCardsScore :: Hand -> [Card]
whichCardsScore = error "Not implemented"

scoreHand :: Hand -> Int
scoreHand = error "Not implemented"

--------------------------------------------------------------------------------
-- Part 4: find the highest scoring hand of 5 cards out of n>=5 cards

highestScoringHand :: [Card] -> Hand
highestScoringHand = error "Not implemented"

--------------------------------------------------------------------------------
-- Part 5: implement an AI for maximising score across 3 hands and 3 discards

simpleAI :: [Move] -> [Card] -> Move
simpleAI = error "Not implemented"

sensibleAI :: [Move] -> [Card] -> Move
sensibleAI = error "Not implemented"

myAI :: [Move] -> [Card] -> Move
myAI = error "Not implemented"