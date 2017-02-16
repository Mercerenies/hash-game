
{-
 - Problem Statement:
 -
 - Let S be an arbitrary string of letters, digits, parentheses, and commas. Then the game begins on a
 - 16x16 board of red tiles. The tiles are indexed by (x, y) where x and y range from 1 to 16 inclusive.
 - The tile (8, 8) is marked with a star; it is the winning square. The tile (8, 16) is a white tile; all
 - of the other tiles (including the winning square) begin red.
 -
 - The player wins the game if the winning tile becomes white. The player loses if the winning tile becomes
 - black or it is his turn and there are no valid moves.
 -
 - On the player's turn, he chooses a red tile that is adjacent to a white tile and turns it white. The player
 - always makes the first move.
 -
 - On the machine's turn, it will choose a single tile and turn it black. The tile it chooses is determined
 - using the following intentionally cryptic algorithm.
 -  (1) Let x and y be the x and y coordinates of the player's most recent move, written in string form
 -      and using exactly as many digits as are necesary (no leading zeroes)
 -  (2) Count the number of white tiles on the board. Let t be the number of white tiles, written as a
 -      string with the same rules as the coordinate strings.
 -  (3) Let u = S(x,y)t, the string formed by concatenating S, a literal open paren, x, a literal comma, y, a
 -      literal close paren, and then t.
 -  (4) Starting at the winning square, form a spiral going from (8,8) then to (8,9) then clockwise outward.
 -      Let m be the number of consecutive red tiles leading this spiral. If the winning square is not red,
 -      m = 0.
 -  (5) Let v be the sum of md5(u), written as a sequence of 32 hexadecimal digits. Then let d = v * m (mod 256)
 -  (6) Considering the game board in reading order (top-left corner is the 0th square, top-right is 15th,
 -      bottom-left is 240th, bottom-right is 255th), turn the dth tile black.
 -
 - This Haskell problem simulates this game.
 -
 - Q: Is there a value for S such that the game is unwinnable?
 -}

import Crypto.Hash.MD5
import Data.ByteString(ByteString)
import Data.ByteString.Char8(pack, unpack)
import Data.Char
import Data.Sequence(Seq)
import qualified Data.Sequence as Seq
import Control.Monad

data Color = Red | White | Black
             deriving (Show, Read, Eq, Ord, Enum)

newtype GameBoard = GameBoard (Seq Color)
    deriving (Show, Read, Eq)

newtype XY = XY (Int, Int)
    deriving (Show, Read, Eq)

newtype Spiral = Spiral Int
    deriving (Show, Read, Eq, Ord)

newtype Reading = Reading Int
    deriving (Show, Read, Eq, Ord)

-- Produces a game board with a single white square at (8, 16) and the remainder of the board being red,
-- as specified as the starting condition of the game.
gameBoard :: GameBoard
gameBoard = GameBoard $ Seq.fromFunction 256 func
    where func 247 = White
          func _ = Red

-- Extracts the sequence of the game board. The order of spaces in this sequence is not strictly defined,
-- so the coordinate functions should be used instead.
toSeq :: GameBoard -> Seq Color
toSeq (GameBoard gb) = gb

-- A coordinate scheme for accessing the game board.
class Coord a where
    toInternal :: a -> Int
    getPos :: GameBoard -> a -> Color
    getPos (GameBoard gb) x = maybe Red id $ Seq.lookup (toInternal x) gb
    setPos :: a -> Color -> GameBoard -> GameBoard
    setPos ind col (GameBoard gb) = GameBoard $ Seq.update (toInternal ind) col gb

-- Integers are trivially coordinates, in an unspecified way.
instance Coord Int where
    toInternal = id

-- XY coordinates can be used to access the grid.
instance Coord XY where
    toInternal (XY (x, y)) = (x - 1) + (y - 1) * 16

-- Spiral coordinates (from the center) can be used to access the grid.
instance Coord Spiral where
    toInternal (Spiral n) = toInternal . XY $ spiral n (8, 8) (1, 0) 1 1 1
        where rotate ( 1,  0) = ( 0,  1)
              rotate ( 0,  1) = (-1,  0)
              rotate (-1,  0) = ( 0, -1)
              rotate ( 0, -1) = ( 1,  0)
              rotate _        = error "could not rotate"
              spiral 0 (x, y) _ _ _ _ = (x, y)
              spiral m (x, y) (dx, dy)  0 z2 0 = spiral m (x, y) (rotate (dx, dy)) (succ z2) (succ z2) 1
              spiral m (x, y) (dx, dy)  0 z2 u = spiral m (x, y) (rotate (dx, dy))       z2        z2  (u - 1)
              spiral m (x, y) (dx, dy) z1 z2 u = spiral (m - 1) (x + dx, y + dy) (dx, dy) (pred z1) z2 u

-- Reading-order integers qualify as coordinates.
instance Coord Reading where
    toInternal (Reading x) = toInternal x

-- Determines whether the board is in a winning state for White.
isWinningPosition :: GameBoard -> Bool
isWinningPosition gb = getPos gb (XY (8, 8)) == White

-- Determines whether the move XY is a valid move for White.
isValidMove :: GameBoard -> XY -> Bool
isValidMove gb (XY (x, y)) = getPos gb (XY (x, y)) == Red &&
                             any (\(x', y') -> getPos gb (XY (x', y')) == White)
                                     [(x - 1, y),
                                      (x + 1, y),
                                      (x, y - 1),
                                      (x, y + 1)]

-- Determines a complete list of valid moves for White.
validMoves :: GameBoard -> [XY]
validMoves gb = [XY (x, y) | x <- [1..16], y <- [1..16], isValidMove gb (XY (x, y))]

-- Determines whether the board is in a losing state for White.
isLosingPosition :: GameBoard -> Bool
isLosingPosition gb = getPos gb (XY (8, 8)) == Black || null (validMoves gb)

-- Determines the enemy's move, based on the state of the game board and the salt S.
enemyMove :: String -> GameBoard -> XY -> Reading
enemyMove ss gb (XY (x, y)) = let t = Seq.length . Seq.filter (== White) $ toSeq gb
                                  u = ((ss ++) . ('(':) . shows x . (',':) . shows y . (')':) . shows t) ""
                                  m = length . takeWhile (== Red) . map (getPos gb . Spiral) $ [0..]
                                  v = sum . unpackAscii . hash $ pack u
                                  d = (v * m) `mod` 256
                              in Reading d
    where unpackAscii :: ByteString -> [Int]
          unpackAscii bs = let str = unpack bs
                           in concat [[n `div` 16, n `mod` 16] | ch <- str, let n = ord ch]

-- Asks the player for the next move for White, using IO. The player's input is expected to be in the form
-- "x y" (without quotes).
playMoveIO :: GameBoard -> IO XY
playMoveIO gb = do
  forM [1..16] $ \y -> do
               forM [1..16] $ \x -> do
                         putStr $ translateSquare (getPos gb $ XY (x, y))
               putStrLn ""
  putStr "> "
  [xx, yy] <- words <$> getLine
  if isValidMove gb $ XY (read xx, read yy) then
      return $ XY (read xx, read yy)
  else
      playMoveIO gb
    where translateSquare :: Color -> String
          translateSquare Red = "_"
          translateSquare White = "W"
          translateSquare Black = "B"

-- Using the list monad, perform all valid moves for White simultaneously.
playAllMoves :: GameBoard -> [XY]
playAllMoves = validMoves

-- Play the game in a given monad using the given salt. Usually, the second argument to this function
-- is either playMoveIO or playAllMoves. The third argument is the starting configuration of the board,
-- often simply gameBoard.
playGame :: Monad m => String -> (GameBoard -> m XY) -> GameBoard -> m Bool
playGame ss turn gb = do
  let moves = validMoves gb
  whiteMove <- turn gb
  if whiteMove `elem` moves then
      let gb' = setPos whiteMove White gb
          blackMove = enemyMove ss gb' whiteMove
          gb'' = setPos blackMove Black gb'
          hasWon = isWinningPosition gb'
          hasLost = isLosingPosition gb''
      in case () of
           _ | hasWon -> return True
             | hasLost -> return False
             | otherwise -> playGame ss turn gb''
  else
      error "invalid move"

-- By brute force, determines whether the given salt is winnable.
isWinnable :: String -> Bool
isWinnable ss = or $ playGame ss playAllMoves gameBoard
