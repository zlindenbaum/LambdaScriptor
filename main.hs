import qualified Data.Map as M
import Text.Printf
import qualified Data.List as L
import System.Exit
import qualified System.Process

-- misc

type ID = Int

keys :: M.Map a b -> [a]
keys = (map fst) . M.toList

(!?) :: (Ord a) => [a] -> ID -> Maybe a
l !? i | abs i >= length l = Nothing
       | otherwise = Just (l!!i)

-- game structure

data Room = Room
  { roomName  :: String
  , roomDesc  :: String
  , roomPaths :: M.Map String ID
  , roomItems :: [Item]
  } deriving (Show, Eq, Ord)

data Item = Item
  { itemNames   :: [String]
  , itemDesc    :: String
  , itemCanTake :: Bool
  } deriving (Show, Eq, Ord)

data Player = Player
  { currentRoom  :: Room
  , currentItems :: [Item]
  } deriving Show

data GameState = GameState
  { gamePlayer :: Player
  , gameRooms  :: [Room]
  } deriving Show

-- TODO: actually make GameState structure

printCurrentRoom :: GameState -> IO ()
printCurrentRoom state = let room     = (currentRoom . gamePlayer) state
                             pathText = ((L.intercalate ", ") . keys . roomPaths) room
                         in putStr $ printf "Room: %s\nDescription: %s\nPaths: %s\n"
                            (roomName room) (roomDesc room) pathText

getRoom :: ID -> Maybe Room
getRoom id = rooms!?id


-- TODO: resolve inconsistency of player inputs; should everything or nothing take GameState as input?
move :: Player -> String -> Player
move player pathName = let testIndex = ((M.lookup pathName) . roomPaths . currentRoom) player
                      in case testIndex >>= getRoom of
                           Just room -> Player room (currentItems player)
                           Nothing -> player

gameLoop :: GameState -> IO ()
gameLoop state = do
  System.Process.system "clear"
  printCurrentRoom state
  putStrLn "\nEnter command:"
  command <- getLine
  case command of
    "quit" -> exitSuccess
    _      -> gameLoop $ GameState (move (gamePlayer state) command) (gameRooms state)

--TEST

-- TODO: make this eventually parse JSON, and eventually use parsec to create custom syntax
rooms = [
  Room "room 1" "the first room" (M.fromList [("north", 1), ("south", 2)]) [],
  Room "room 2" "the second room" (M.fromList [("south", 0)]) [],
  Room "room 3" "the third room" (M.fromList [("north", 0)]) []]

main :: IO ()
main = do
  let game = GameState (Player (rooms!!0) []) rooms
  gameLoop game
