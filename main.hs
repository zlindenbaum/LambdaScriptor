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
  } deriving (Show, Eq, Ord )

data Item = Item
  { itemNames   :: [String]
  , itemDesc    :: String
  , itemCanTake :: Bool
  } deriving (Show, Eq, Ord)

data Player = Player
  { currentRoom  :: Room
  , currentItems :: [Item]
  } deriving Show

-- TODO: actually make GameState structure

-- TODO: make this eventually parse JSON, and eventually use parsec to create custom syntax
rooms = [
  Room "room 1" "the first room" (M.fromList [("north", 1), ("south", 2)]) [],
  Room "room 2" "the second room" (M.fromList [("south", 0)]) [],
  Room "room 3" "the third room" (M.fromList [("north", 0)]) []]

printCurrentRoom :: Player -> IO ()
printCurrentRoom player = let room     = currentRoom player
                              pathText = ((L.intercalate ", ") . keys . roomPaths) room
                          in putStr $ printf "Room: %s\nDescription: %s\nPaths: %s\n"
                             (roomName room) (roomDesc room) pathText

getRoom :: ID -> Maybe Room
getRoom id = rooms!?id

move :: Player -> String -> Player
move player pathName = let testIndex = ((M.lookup pathName) . roomPaths . currentRoom) player
                      in case testIndex >>= getRoom of
                           Just room -> Player room (currentItems player)
                           Nothing -> player

gameLoop :: Player -> IO ()
gameLoop player = do
  System.Process.system "clear"
  printCurrentRoom player
  putStrLn "\nEnter command: "
  command <- getLine
  case command of
    "quit" -> exitSuccess
    _ -> gameLoop (move player command)


--TEST

main :: IO ()
main = do
  let p = Player (rooms!!0) []
  gameLoop p
