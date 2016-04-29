module MoveGen where

import Board

genAllMoves :: Position -> [Move]
genAllMoves pos = concatMap ($ pos)
  [ genPawnMoves
  , genKnightMoves
  , genBishopMoves
  , genRookMoves
  , genQueenMoves
  , genKingMoves
  ]

genPawnMoves :: Position -> [Move]
genPawnMoves = undefined

genKnightMoves :: Position -> [Move]
genKnightMoves = undefined

genBishopMoves :: Position -> [Move]
genBishopMoves pos = pieceToMove pos Bishop >>= genDiagonalMoves pos

genRookMoves :: Position -> [Move]
genRookMoves pos = pieceToMove pos Rook >>= genHorizontalMoves pos

genQueenMoves :: Position -> [Move]
genQueenMoves pos = do
  sq <- pieceToMove pos Queen
  genHorizontalMoves pos sq ++ genDiagonalMoves pos sq

genKingMoves :: Position -> [Move]
genKingMoves = undefined

genHorizontalMoves :: Position -> Square -> [Move]
genHorizontalMoves pos (f,r) = do
  let right = validMoves' [(f', r) | f' <- tail [f ..]]
      left = validMoves' [(f', r) | f' <- reverse $ init [minBound .. f]]
      up = validMoves' [(f, r') | r' <- tail [r ..]]
      down = validMoves' [(f, r') | r' <- reverse $ init [minBound .. r]]
  sq <- right ++ left ++ up ++ down
  return $ Move (f, r) sq Nothing
  where validMoves' = validMoves pos


genDiagonalMoves :: Position -> Square -> [Move]
genDiagonalMoves pos (f,r) = do
  let ne = validMoves' $ tail $ zip [f ..] [r ..]
      se = validMoves' $ tail $ zip [f ..] (reverse [minBound .. r])
      sw = validMoves' $ tail $ zip (reverse [minBound .. f])
                                             (reverse [minBound .. r])
      nw = validMoves' $ tail $ zip (reverse [minBound .. f])
                                             [r ..]
  sq <- ne ++ se ++ sw ++ nw
  return $ Move (f, r) sq Nothing
  where validMoves' = validMoves pos

pieceToMove :: Position -> PieceType -> [Square]
pieceToMove pos tp =
  fmap fst $ filter ((==) (Piece tp (toMove pos)) . snd) $ pieces $ board pos

validMoves :: Position -> [Square] -> [Square]
validMoves pos = foldr go []
  where go :: Square -> [Square] -> [Square]
        go to sqs = case getPiece (board pos) to of
          Nothing -> to : sqs
          Just pc -> if pColour pc == toMove pos
                       then []
                       else [to]
