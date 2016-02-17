module Board where

import Control.Monad (guard)
import Data.Char (digitToInt, isDigit)
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
  ranks <- sequence $ processRank <$> splitBy '/' str
  guard $ length ranks == 8 && all ((== 8) . length) ranks
  let annRanks = [[((f,r), p) | (f,p) <- zip [FileA ..] ps] | (r,ps) <- zip [Rank8, Rank7 ..] ranks]
  return $ Board $ flip M.lookup $ foldl f M.empty annRanks
  where f :: M.Map Square Piece -> [(Square, Maybe Piece)]  -> M.Map Square Piece
        f = foldl (\m (s, mp) -> maybe m (\p -> M.insert s p m) mp)

processRank :: String -> Maybe [Maybe Piece]
processRank = sequence . concatMap
   (\c -> if isDigit c
            then replicate (digitToInt c) $ Just Nothing
            else [Just <$> charToPiece c])

charToPiece :: Char -> Maybe Piece
charToPiece 'p' = Just $ Piece Pawn Black
charToPiece 'n' = Just $ Piece Knight Black
charToPiece 'b' = Just $ Piece Bishop Black
charToPiece 'r' = Just $ Piece Rook Black
charToPiece 'q' = Just $ Piece Queen Black
charToPiece 'k' = Just $ Piece King Black
charToPiece 'P' = Just $ Piece Pawn White
charToPiece 'N' = Just $ Piece Knight White
charToPiece 'B' = Just $ Piece Bishop White
charToPiece 'R' = Just $ Piece Rook White
charToPiece 'Q' = Just $ Piece Queen White
charToPiece 'K' = Just $ Piece King White
charToPiece _   = Nothing

boardToPieces :: Board -> String
boardToPieces = undefined

fenToPos :: String -> Maybe Position
fenToPos str = do
  let w = words str
  guard $ length w == 6
  let [b, t, c, ep, h, f] = w
  t' <- case t of
          "w" -> Just White
          "b" -> Just Black
          _   -> Nothing
  ep' <- case ep of
           "-" -> Just Nothing
           s   -> Just <$> readSquare s
  h' <- readMaybe h
  f' <- readMaybe f
  b' <- piecesToBoard b
  c' <- pure c
  return Position
    { board     = b'
    , toMove    = t'
    , castling  = c'
    , enPassant = ep'
    , halfMoves = h'
    , fullMoves = f'
    }

posToFen :: Position -> String
posToFen pos = undefined

start :: Position
start = fromJust $ fenToPos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
