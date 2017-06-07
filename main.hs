{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as M
import Text.Printf
import qualified Data.List as L
import System.Exit
import qualified System.Process
import Data.Maybe

-- misc

type Name = String
type ID = Int

keys :: M.Map a b -> [a]
keys = (map fst) . M.toList

(!?) :: (Ord a) => [a] -> ID -> Maybe a
l !? i | abs i >= length l = Nothing
       | otherwise = Just (l!!i)

lReplace :: (Ord a) => [a] -> Int -> a -> [a]
lReplace l i n = take i l ++ [n] ++ drop (i + 1) l

-- game structure

data Room = Room
  { roomName  :: String
  , roomDesc  :: String
  , roomPaths :: M.Map Name ID
  , roomItems :: M.Map Name ID
  } deriving (Show, Eq, Ord)

-- TODO: eventually make have multiple names
data Item = Item
  { itemName    :: String
  , itemDesc    :: String
  , itemCanTake :: Bool
  } deriving (Show, Eq, Ord)

data Player = Player
  { currentRoom  :: Room
  , currentItems :: M.Map Name ID
  } deriving Show

data GameState = GameState
  { gamePlayer :: Player
  , gameRooms  :: [Room]
  , gameItems  :: [Item]
  } deriving Show

-- TODO: actually make GameState structure

-- TODO: maybe separate text generators into their own functions
printCurrentRoom :: GameState -> IO ()
printCurrentRoom state = let room     = (currentRoom . gamePlayer) state
                             pathText = ((L.intercalate ", ") . keys . roomPaths) room
                             itemText = ((L.intercalate ", ") . keys . roomItems) room
                         in putStr $ printf "Room: %s\nDescription: %s\nItems: %s\nPaths: %s\n"
                            (roomName room) (roomDesc room) itemText pathText

getRoom :: ID -> Maybe Room
getRoom id = rooms!?id

getItem :: ID -> Maybe Item
getItem id = items!?id

-- TODO: resolve inconsistency of player inputs; should everything or nothing take GameState as input?
-- SOLUTION: make everthing take and return GameState (maybe with intermediate function to get locals);
-- this makes it easier for it to be called in the main gameLoop (we don't need to initialize a new
-- GameState every recursive cycle, because the action does it for us already)
move :: Player -> String -> Player
move player pathName = let testIndex = ((M.lookup pathName) . roomPaths . currentRoom) player
                       in case testIndex >>= getRoom of
                           Just room -> Player room (currentItems player)
                           Nothing -> player

-- TODO: maybe move index testing into its own function
takeItem :: GameState -> String -> GameState
takeItem state itemName = let player     = gamePlayer state
                              cRoom      = currentRoom player
                              rItems     = roomItems cRoom
                              cItems     = currentItems player
                              roomIndex  = (fromJust . (L.elemIndex cRoom)) $ gameRooms state
                              testIndex  = ((M.lookup itemName) . roomItems) cRoom
                              succRoom   = Room (roomName cRoom) (roomDesc cRoom) (roomPaths cRoom) (M.delete itemName rItems)
                              succPlayer = Player cRoom $ M.insert itemName (fromJust testIndex) cItems
                          in case testIndex of
                               Just index -> GameState succPlayer (lReplace (gameRooms state) roomIndex succRoom) (gameItems state)
                               Nothing -> state


gameLoop :: GameState -> IO ()
gameLoop state = do
  System.Process.system "clear"
  printCurrentRoom state
  putStrLn "\nEnter command:"
  command <- getLine
  -- TODO: fix the take item problem; it's somewhere around here
  case command of
    "quit" -> exitSuccess
    c      | take 5 c == "take " -> gameLoop $ takeItem state (drop 5 c)
           | otherwise           -> gameLoop $ GameState (move (gamePlayer state) c) (gameRooms state) (gameItems state)

--TEST

-- TODO: make this eventually parse JSON, and eventually use parsec to create custom syntax
rooms = [
  Room "room 1" "the first room" (M.fromList [("north", 1), ("south", 2)]) (M.fromList [("key", 0)]),
  Room "room 2" "the second room" (M.fromList [("south", 0)]) M.empty,
  Room "room 3" "the third room" (M.fromList [("north", 0)]) M.empty]

items = [
  Item "key" "a plain key" False ]

main :: IO ()
main = do
  let game = GameState (Player (rooms!!0) M.empty) rooms items
  gameLoop game
