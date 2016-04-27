module MoveGen where

import Board (Position)

genAllMoves :: Position -> [Position]
genAllMoves pos = concatMap ($ pos)
  [ genPawnMoves
  , genKnightMoves
  , genBishopMoves
  , genRookMoves
  , genQueenMoves
  , genKingMoves
  ]

genPawnMoves :: Position -> [Position]
genPawnMoves = undefined

genKnightMoves :: Position -> [Position]
genKnightMoves = undefined

genBishopMoves :: Position -> [Position]
genBishopMoves = undefined

genRookMoves :: Position -> [Position]
genRookMoves = undefined

genQueenMoves :: Position -> [Position]
genQueenMoves = undefined

genKingMoves :: Position -> [Position]
genKingMoves = undefined
