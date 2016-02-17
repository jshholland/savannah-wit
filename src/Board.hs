module Board where

import Control.Monad (guard)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Text.Read (readMaybe)

import Util (splitBy)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Show, Eq)
data Colour = White | Black
  deriving (Show, Eq)

data Piece = Piece
  { pType   :: PieceType
  , pColour :: Colour
  } deriving (Show, Eq)

data Rank = Rank1 | Rank2 | Rank3 | Rank4 | Rank5 | Rank6 | Rank7 | Rank8
  deriving (Show, Eq, Enum, Ord)
data File = FileA | FileB | FileC | FileD | FileE | FileF | FileG | FileH
  deriving (Show, Eq, Enum, Ord)

type Square = (File, Rank)

readRank :: Char -> Maybe Rank
readRank '1' = Just Rank1
readRank '2' = Just Rank2
readRank '3' = Just Rank3
readRank '4' = Just Rank4
readRank '5' = Just Rank5
readRank '6' = Just Rank6
readRank '7' = Just Rank7
readRank '8' = Just Rank8
readRank _   = Nothing

readFile' :: Char -> Maybe File
readFile' 'a' = Just FileA
readFile' 'b' = Just FileB
readFile' 'c' = Just FileC
readFile' 'd' = Just FileD
readFile' 'e' = Just FileE
readFile' 'f' = Just FileF
readFile' 'g' = Just FileG
readFile' 'h' = Just FileH
readFile' _   = Nothing

readSquare :: String -> Maybe Square
readSquare [f,r] = (,) <$> readFile' f <*> readRank r
readSquare _     = Nothing

newtype Board = Board { getPiece :: Square -> Maybe Piece }

data Position = Position
  { board     :: Board
  , toMove    :: Colour
  , castling  :: String
  , enPassant :: Maybe Square
  , halfMoves :: Int
  , fullMoves :: Int
  }

piecesToBoard :: String -> Maybe Board
piecesToBoard str = do
  let ranks = splitBy '/' str
  guard $ length ranks == 8
  undefined

boardToPieces :: Board -> String
boardToPieces = undefined

fenToPos :: String -> Maybe Position
fenToPos str = do
  let w = words str
  guard $ length w == 6
  let [pieces, toMove', castling', enPassant', halfMoves', fullMoves'] = w
  toMove <- case toMove' of
              "w" -> Just White
              "b" -> Just Black
              _   -> Nothing
  enPassant <- case enPassant' of
                 "-" -> Just Nothing
                 s   -> Just <$> readSquare s
  halfMoves <- readMaybe halfMoves'
  fullMoves <- readMaybe fullMoves'
  board <- piecesToBoard pieces
  castling <- pure castling'
  return Position
    { board     = board
    , toMove    = toMove
    , castling  = castling
    , enPassant = enPassant
    , halfMoves = halfMoves
    , fullMoves = fullMoves
    }

posToFen :: Position -> String
posToFen pos = undefined

start :: Position
start = fromJust $ fenToPos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
