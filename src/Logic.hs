{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-overflowed-literals #-}
module Logic where

import Control.Concurrent
import Control.Monad
import Control.Monad.ST
import Control.Parallel
import Data.Array
import Data.Array.ST
import Data.Array.Unsafe
import Data.IORef
import Data.List (insertBy, maximumBy, sortBy, elemIndex)
import qualified Data.Map as M
import Data.Partition
import qualified Data.Set as S
import Data.Word
import System.Mem
import System.Random
import Text.Read (readMaybe)

data Player = FirstPlayer | SecondPlayer deriving (Show, Eq, Ord)
data Cell = Cell {x :: !Word8, y :: !Word8} deriving (Eq, Ord, Ix)

instance Show Cell where
    show Cell{..} = (['A'..] !! (fromIntegral x)):show (y+1)

instance Read Cell where
    readsPrec n s = case s of
        (l:rst) -> case elemIndex l ['A'..] of
            Nothing -> []
            Just x -> do
                (yp1, rst') <- readsPrec n rst
                return (Cell (fromIntegral x) (yp1 - 1), rst')
        _ -> []

data Board = Board {lastMove :: Maybe Cell, turns :: !Word16, player :: !Player, swapped :: Bool, pos :: !(Cell -> Maybe Player)}

n :: Word8
n = 19
intn :: Int
intn = fromIntegral n

start :: Board
start = Board Nothing 0 FirstPlayer False $ const Nothing

moveAt :: Cell -> Board -> Board
moveAt cell@(Cell x y) Board{turns = 1, swapped = False, ..} | pos cell /= Nothing = Board (Just cell) 1 (opp player) True pos' where
    pos' (Cell x' y') | x' == y && y' == x = Just player
    pos' _ = Nothing
moveAt cell b@Board{..} | pos cell /= Nothing = error $ show cell ++ " is already taken in " ++ show b
moveAt cell Board{..} = Board (Just cell) (turns+1) (opp player) swapped pos' where
    pos' cell' | cell' == cell = Just player
    pos' cell' = pos cell'

legalMoves :: Board -> [Cell]
legalMoves b = [Cell x y| x <- [0..n-1], y <- [0..n-1], pos b (Cell x y) == Nothing]

instance Show Board where
    show Board{..} = unlines $ (show player ++ "'s turn."):[line y | y <- [0..intn+1]] where
        line 0 = (' ':) $ take intn ['A'..] >>= (:" ")
        line np1 | np1 == intn+1 = line 0
        line y = show y ++ " " ++ concat [cell x (y-1) | x <- [0..n-1]] ++ show y
        cell x y = case pos $ Cell (fromIntegral x) (fromIntegral y) of
            Just FirstPlayer -> if Just (Cell (fromIntegral x) (fromIntegral y)) == lastMove
                then "\b[V]" else "V "
            Just SecondPlayer -> if Just (Cell (fromIntegral x) (fromIntegral y)) == lastMove
                then "\b[H]" else "H "
            Nothing -> "- "

opp FirstPlayer = SecondPlayer
opp SecondPlayer = FirstPlayer

[left, right] = [[Cell x y | y <- [0..n-1]] | x <- [0,n-1]]
[top, bot] = [[Cell x y | x <- [0..n-1]] | y <- [0,n-1]]

nborDirs = [(-1,0), (-1,1), (0,1), (1,0), (1, -1), (0, -1)]
nbors Cell{..} = [Cell (x + dx) (y + dy) | (dx, dy) <- nborDirs, x+dx `notElem` [-1,n], y+dy `notElem` [-1,n]]

won :: Board -> Player -> Bool
won Board{..} p = rep chains (head side1) == rep chains (head side2) where
    (side1, side2) = case p of
        FirstPlayer -> (top, bot)
        SecondPlayer -> (left, right)

    set cell = if pos cell == Just p
        then S.fromList $ cell:[nbor | nbor <- nbors cell, pos nbor == Just p]
        else S.empty
    
    chains = fromSets $ (S.fromList side1):(S.fromList side2):[set (Cell x y) | x <- [0..n-1], y<-[0..n-1]]
    

winner b@Board{..} = if won b FirstPlayer 
                        then Just FirstPlayer
                     else if turns == (fromIntegral n)^2
                        then Just SecondPlayer
                     else if won b SecondPlayer
                        then Just SecondPlayer
                     else
                        Nothing

optimize Board{..} = Board lastMove turns player swapped (arr !) where
    arr = listArray (Cell 0 0, Cell (n-1) (n-1)) [pos c | c <- range (Cell 0 0, Cell (n-1) (n-1))]

data MCTS = MC {
    wins :: !Float,
    plays :: !Float,
    victor :: !(Maybe Player),
    board :: !Board,
    moves :: [Cell],
    unexplored :: [(Cell, MCTS)],
    explored :: [(Float, Cell, MCTS)]
    }

instance Show MCTS where
    show MC{..} = "MC {wins :: "++show wins++", plays :: "++show plays++", victor :: "++show victor++", board :: "++show board++", unexplored :: _, explored :: "++show explored ++ "}"

newMCTS board = igo board' moves where
    board' = optimize board
    moves = legalMoves board'
    igo board moves = MC 0 0 (if turns board == (fromIntegral n)^2 then winner board else Nothing) board moves
        [(move, igo board'' moves') | (move, moves') <- tree id moves, let board'' = moveAt move board] []
    tree front [] = []
    tree front (b:ack) = (b, front ack):(tree (front . (b:)) ack)

playOut gen mc@MC{victor = Just p, ..} = (p, mc{wins = wins', plays = plays'}, board, gen) where
    win = opp (player board) == p
    wins' = if win then wins + 1 else wins
    plays' = plays + 1

playOut gen mc@MC{unexplored = ((cell, child):unexplored'), ..}
    = (p, mc'', b, gen') where
        (p, child', b, gen') = simulate gen child
        explored' = (evalChild child', cell, child'):explored
        win = opp (player board) == p
        wins' = if win then wins + 1 else wins
        plays' = plays + 1
        mc' = mc{unexplored = unexplored', explored = explored', wins = wins', plays = plays'}
        mc'' = amaf mc' p b

playOut gen mc@MC{explored = ((_, cell, child):explored'), ..} = (p, mc'', b, gen') where
    (p, child', b, gen') = playOut gen child
    explored'' = (evalChild child', cell, child'):explored'
    win = opp (player board) == p
    wins' = if win then wins + 1 else wins
    plays' = plays + 1
    mc' = mc{explored = explored'', wins = wins', plays = plays'}
    mc'' = amaf mc' p b

newArr :: Board -> ST s (STArray s Cell (Maybe Player))
newArr board = newListArray (Cell 0 0, Cell (n-1) (n-1)) [pos board c | c <- range (Cell 0 0, Cell (n-1) (n-1))]

evalChild MC{..} = wins / plays
simulate gen mc@MC{..} = runST $ do
    (moves', gen') <- shuffle gen moves
    if pos board (head moves') /= Nothing
        then return $ simulate gen' mc
        else do
            arr <- newArr board
            (_, turn, gen'') <- foldM (\(lastMove, turn, gen) move -> do
                (lastMove', turn', gen') <- rollOut gen lastMove turn arr
                cell <- readArray arr move
                case cell of
                    Nothing -> do
                        writeArray arr move (Just turn')
                        return (Just move, opp turn', gen')
                    Just _ -> return (lastMove', turn', gen')) (Nothing, player board, gen') moves'
            arr' <- unsafeFreeze arr
            let
                finalBoard = Board undefined ((fromIntegral n)^2) turn (swapped board) (arr' !)
                Just p = winner finalBoard
                win = opp (player board) == p
                wins' = if win then wins + 1 else wins
                plays' = plays + 1
                mc' = mc{wins = wins', plays = plays'}
                mc'' = amaf mc' p finalBoard
            return (p, mc'', finalBoard, gen'')

newListArrayST :: (Int, Int) -> [a] -> ST s (STArray s Int a)
newListArrayST = newListArray

shuffle :: StdGen -> [a] -> ST s ([a], StdGen)
shuffle gen xs = do
        ar <- newListArrayST (1,n) xs
        foldM (\(lst, gen) i -> do
            let (j, gen') = randomR (i,n) gen
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return (vj:lst, gen')) ([], gen) [1..n]
  where
    n = length xs

ratio = 0.3
amaf mc winPlayer final = mc{explored = explored'', unexplored = unexplored'} where
    explored' = sortBy (\(e1,_,_) (e2,_,_) -> compare e2 e1) $ do
        original@(_,cell,child) <- explored mc
        return $ if pos final cell == (Just $ player $ board mc)
            then
                let win = winPlayer == (player $ board mc)
                    child' = child{wins = if win then wins child + ratio else wins child, plays = plays child + ratio}
                in (evalChild child', cell, child')
        else original

    (explored'', unexplored') = igo explored' (unexplored mc)
    igo exp [] = (exp, [])
    igo exp ((cell, child):unexp) =
        if pos final cell == (Just $ player $ board mc)
            then
                let win = winPlayer == (player $ board mc)
                    child' = child{wins = if win then wins child + ratio else wins child, plays = plays child + ratio}
                in igo (insertBy (\(e1,_,_) (e2,_,_) -> compare e2 e1) (evalChild child', cell, child') exp) unexp
        else
            let (exp', unexp') = igo exp unexp in (exp', (cell, child):unexp')

color :: STArray s Cell (Maybe Player) -> Cell -> ST s (Maybe Player, Cell)
color arr cell@(Cell x y) = let
        vBord = y == -1 || y == n
        hBord = x == -1 || x == n
    in
        if vBord then return $
            if hBord then (Nothing, cell)
            else (Just FirstPlayer, cell)
        else
            if hBord then return (Just SecondPlayer, cell)
            else do
                color <- readArray arr cell
                return (color, cell)

rollOut gen lastMove turn arr = do
    bridgeMove <- bridge gen lastMove turn arr
    case bridgeMove of
        Right res -> return res
        Left gen' -> do
            blockMove <- block gen' lastMove turn arr
            case blockMove of
                Right res -> return res
                Left gen'' -> return (lastMove, turn, gen'')

bridge gen Nothing turn arr = return $ Left gen
bridge gen (Just (Cell x y)) turn arr = do
    let (offset, gen') = randomR (0,5) gen
    nbrs <- take (6+2) . drop offset . cycle <$> (mapM (color arr) $ [Cell (x + dx) (y + dy) | (dx, dy) <- nborDirs])
    let igo ((c1, _):next@((cc, cell):(c2, _):_)) = if c1 == Just turn && cc == Nothing && c2 == Just turn
            then Just cell
            else igo next
        igo _ = Nothing
    case igo nbrs of
        (Just cell) -> do
            writeArray arr cell (Just turn)
            return $ Right (Just cell, opp turn, gen')
        Nothing -> return $ Left gen'

block gen Nothing turn arr = return $ Left gen
block gen (Just (Cell x y)) turn arr = do
    let blockDirs = blockers turn x y
    let (offset, gen') = randomR (0, length blockDirs - 1) gen
    let blockDirs' = take (length blockDirs) $ drop offset $ cycle blockDirs
    nbrs <- forM blockDirs' $ \(dxa, dya, dxb, dyb) -> do
        ally <- color arr $ Cell (x + dxa) (y + dya)
        blocker <- color arr $ Cell (x + dxb) (y + dyb)
        return (ally, blocker)
    
    let igo (((acol, acel), (bcol, bcel)):rst) = if acol == Just turn && bcol == Nothing
            then Just bcel
            else igo rst
        igo [] = Nothing

    case igo nbrs of
        (Just cell@(Cell x y)) | 0 <= x && x < n && 0 <= y && y < n -> do
            writeArray arr cell (Just turn)
            return $ Right (Just cell, opp turn, gen')
        _ -> return $ Left gen'

blockers FirstPlayer ex ey = extra ++ [(1, 0, 1, -1), (1, -1, 1, 0), (-1, 0, -1, 1), (-1, 1, -1, 0)] where
    extra = case compare (ex+ey) (n-1) of
        LT -> [(1, 0, 0, 1), (0, 1, 1, 0)]
        EQ -> []
        GT -> [(-1, 0, 0, -1), (0, -1, -1, 0)]
blockers SecondPlayer ex ey = extra ++ [(0, 1, -1, 1), (-1, 1, 0, 1), (0, -1, 1, -1), (1, -1, 0, -1)] where
    extra = case compare (ex+ey) (n-1) of
        LT -> [(1, 0, 0, 1), (0, 1, 1, 0)]
        EQ -> []
        GT -> [(-1, 0, 0, -1), (0, -1, -1, 0)]

parPlayOut gen mc@MC{explored = (_, cell1, child1):(_, cell2, child2):explored'} = (mc'''', gen') where
    (gen1, gen2) = split gen
    (p1, child1', b1, gen') = playOut gen child1
    (p2, child2', b2, _   ) = playOut gen child2
    explored'' = (evalChild child1', cell1, child1'):explored'
    mc' = amaf mc{explored = explored''} p1 b1
    (child2'', mc'') = child2' `par` mc' `pseq` (child2', mc')
    mc''' = amaf mc''{explored = (evalChild child2'', cell2, child2''):explored mc''} p2 b2
    
    win1 = opp (player $ board mc) == p1
    win2 = opp (player $ board mc) == p2
    mc'''' = mc'''{wins = wins mc''' + (if win1 then 1 else 0) + (if win2 then 1 else 0), plays = plays mc''' + 2}

parPlayOut gen mc = let (_, mc', _, gen') = playOut gen mc in (mc', gen')

playOuts mc n = do
    getStdRandom $ \gen -> iterate (\(mc, gen) -> let (mc', gen') = parPlayOut gen mc in (mc', gen')) (mc, gen) !! n

lci mc child = wins child / plays child - sqrt (2 * log (plays mc) / plays child)

aiMove b@Board{swapped = False, turns = 1, ..} t = do
    let mc = newMCTS b
    mcRef <- newIORef mc
    tid <- forkIO $ do
        let swapMove = head [Cell x y| x <- [0..n-1], y <- [0..n-1], pos (Cell x y) /= Nothing]
        let mcswap = newMCTS (moveAt swapMove b)
        mcswap' <- playOuts mcswap 100
        let mc' = mc{explored = (evalChild mcswap', swapMove, mcswap'):explored mc}
        writeIORef mcRef mc'
        forever $ do
            mc <- readIORef mcRef
            mc' <- playOuts mc 50
            writeIORef mcRef mc'
    threadDelay $ round $ t*10^6
    killThread tid
    mc' <- readIORef mcRef

    let child = maximumBy
            (\c1 c2 -> compare (lci mc' c1) (lci mc' c2))
            $ map (\(_,_,x) -> x) $ explored mc'
    print $ (plays child, plays mc')
    putStrLn $ (show $ 100 * evalChild child) ++ "%"
    let b' = optimize $ board child
    forkIO $ performGC
    return b'

aiMove b t = do
    let mc = newMCTS b
    mcRef <- newIORef mc
    tid <- forkIO $ forever $ do
        mc <- readIORef mcRef
        mc' <- playOuts mc 50
        writeIORef mcRef mc'
    threadDelay $ round $ t*10^6
    killThread tid
    mc' <- readIORef mcRef
    let child = maximumBy
            (\c1 c2 -> compare (lci mc' c1) (lci mc' c2))
            $ map (\(_,_,x) -> x) $ explored mc'
    print $ (plays child, plays mc')
    putStrLn $ (show $ 100 * evalChild child) ++ "%"
    let b' = optimize $ board child
    performGC
    return b'

humanMove b Nothing = do
    putStrLn "Enter move"
    text <- getLine
    humanMove b $ readMaybe text
humanMove b (Just move) = if pos b move == Nothing
    then return move
    else humanMove b Nothing

play b t = case winner b of
    Just p -> putStrLn $ show p ++ " wins!"
    Nothing -> do
        putStrLn "Thinking..."
        b' <- aiMove b t
        print b'
        case winner b' of
            Just p -> putStrLn $ show p ++ " wins!"
            Nothing -> do
                move <- humanMove b' Nothing
                let b'' = moveAt move b'
                play b'' t
