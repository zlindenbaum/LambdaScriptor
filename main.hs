import qualified Data.Map as M
import Text.Printf
import qualified Data.List as L

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

data GameState = GameState
  { currentRoom  :: Room
  , currentItems :: [Item]
  } deriving Show

-- TODO: make this eventually parse JSON, and eventually use parsec to create custom syntax
rooms = [
  Room "room 1" "the first room" (M.fromList [("north", 1), ("south", 2)]) [],
  Room "room 2" "the second room" (M.fromList [("south", 0)]) [],
  Room "room 3" "the third room" (M.fromList [("north", 0)]) []]

printCurrentRoom :: GameState -> IO ()
printCurrentRoom state = let room     = currentRoom state
                             pathText = ((L.intercalate ", ") . keys . roomPaths) room
                         in putStr $ printf "Room: %s\nDescription: %s\nPaths: %s\n"
                            (roomName room) (roomDesc room) pathText

getRoom :: ID -> Maybe Room
getRoom id = rooms!?id

move :: GameState -> String -> GameState
move state pathName = let testIndex = ((M.lookup pathName) . roomPaths . currentRoom) state
                      in case testIndex >>= getRoom of
                           Just room -> GameState room (currentItems state)
                           Nothing -> state


--TEST

gs = GameState (rooms!!0) []
