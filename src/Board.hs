module Board
  ( Piece(..)
  , PieceType(..)
  , Colour(..)
  , Rank(..)
  , File(..)
  , Square
  , Board(..)
  , Position(..)
  , fenToPos
  , posToFen
  , start
  , pieces
  )
  where

import Control.Monad (guard)
import Data.Char (digitToInt, intToDigit, isDigit)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
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

showRank :: Rank -> Char
showRank Rank1 = '1'
showRank Rank2 = '2'
showRank Rank3 = '3'
showRank Rank4 = '4'
showRank Rank5 = '5'
showRank Rank6 = '6'
showRank Rank7 = '7'
showRank Rank8 = '8'

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

showFile :: File -> Char
showFile FileA = 'a'
showFile FileB = 'b'
showFile FileC = 'c'
showFile FileD = 'd'
showFile FileE = 'e'
showFile FileF = 'f'
showFile FileG = 'g'
showFile FileH = 'h'

readSquare :: String -> Maybe Square
readSquare [f,r] = (,) <$> readFile' f <*> readRank r
readSquare _     = Nothing

showSquare :: Square -> String
showSquare (f,r) = [showFile f, showRank r]

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
  return $ Board $ flip M.lookup $ foldl g M.empty annRanks
  where g :: M.Map Square Piece -> [(Square, Maybe Piece)]  -> M.Map Square Piece
        g = foldl (\m (s, mp) -> maybe m (\p -> M.insert s p m) mp)

processRank :: String -> Maybe [Maybe Piece]
processRank = sequence . concatMap
   (\c -> if isDigit c
            then replicate (digitToInt c) $ Just Nothing
            else [Just <$> charToPiece c])

charToPiece :: Char -> Maybe Piece
charToPiece 'p' = Just $ Piece Pawn   Black
charToPiece 'n' = Just $ Piece Knight Black
charToPiece 'b' = Just $ Piece Bishop Black
charToPiece 'r' = Just $ Piece Rook   Black
charToPiece 'q' = Just $ Piece Queen  Black
charToPiece 'k' = Just $ Piece King   Black
charToPiece 'P' = Just $ Piece Pawn   White
charToPiece 'N' = Just $ Piece Knight White
charToPiece 'B' = Just $ Piece Bishop White
charToPiece 'R' = Just $ Piece Rook   White
charToPiece 'Q' = Just $ Piece Queen  White
charToPiece 'K' = Just $ Piece King   White
charToPiece _   = Nothing

pieceToChar :: Piece -> Char
pieceToChar (Piece Pawn   Black) = 'p'
pieceToChar (Piece Knight Black) = 'n'
pieceToChar (Piece Bishop Black) = 'b'
pieceToChar (Piece Rook   Black) = 'r'
pieceToChar (Piece Queen  Black) = 'q'
pieceToChar (Piece King   Black) = 'k'
pieceToChar (Piece Pawn   White) = 'P'
pieceToChar (Piece Knight White) = 'N'
pieceToChar (Piece Bishop White) = 'B'
pieceToChar (Piece Rook   White) = 'R'
pieceToChar (Piece Queen  White) = 'Q'
pieceToChar (Piece King   White) = 'K'

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
posToFen pos = unwords
  [ boardToPieces $ board pos
  , case toMove pos of
      Black -> "b"
      White -> "w"
  , castling pos
  , maybe "-" showSquare $ enPassant pos
  , show $ halfMoves pos
  , show $ fullMoves pos
  ]

rankToString :: Board -> Rank -> String
rankToString b r = foldNums $ maybe '1' pieceToChar <$>
  [getPiece b (f,r) | f <- [FileA .. FileH]]

boardToPieces :: Board -> String
boardToPieces b = intercalate "/" $
  rankToString b <$> [Rank8, Rank7 .. Rank1]

foldNums :: String -> String
foldNums = foldr combine ""
  where combine :: Char -> String -> String
        combine c [] = [c]
        combine c (c':cs) | isDigit c
                            && isDigit c'   = c +$ c' : cs
                          | otherwise       = c:c':cs
        (+$) :: Char -> Char -> Char
        c +$ c' = intToDigit $ digitToInt c + digitToInt c'

start :: Position
start = fromJust $ fenToPos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

pieces :: Board -> [Piece]
pieces b = mapMaybe (getPiece b) [(f,r) | f <- [FileA .. FileH],
                                          r <- [Rank1 .. Rank8]]
