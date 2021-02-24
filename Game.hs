import Base

opposite :: Direction -> Direction
opposite North = South
opposite East = West
opposite South = North
opposite West = East

noActions :: Item -> GameState -> Next GameState
noActions item gameState = Same ("Nothing can be done with " ++ show item)

winningRoom = Room { name = "Winning Room", description = "This room is a winning room", isWinRoom = True, requires = Just Key, items = [], monsters = [], doors = [], actions = noActions }

startingRoom = Room { name = "Starting Room", description = "This room is the starting room", isWinRoom = False, requires = Nothing, items = [(Spoon, "A curved bowl attached to a flimsy handle")], monsters = [], doors = [(North, anotherRoom)], actions = noActions }

woodTroll = WoodTroll { health = 10, holding = Key }

action :: Item -> GameState -> Next GameState
action Key gameState    | null $ monsters $ room gameState = Same "Action unavailable!"
                        | otherwise = let monster : xs = monsters $ room gameState in
                            if health monster <= 5 then
                                Progress "The monster has been killed!" gameState{room = (room gameState) {monsters = xs, items = (holding monster, "Monster droppings") : items (room gameState)}}
                            else
                                Progress "The monster has been damanged!" gameState{room = (room gameState) {monsters = monster{health = health monster - 5} : xs}}

anotherRoom = Room { name = "Leading Room", description = "This room is a leading room containing a monster", isWinRoom = False, requires = Nothing, items = [(Key, "Looks like this would fit in those holes in the door...")], monsters = [woodTroll], doors = [], actions = action }

game0 = GS {player=Player {playerName="Foo Bar", inventory=[]}, room=startingRoom}

instance Parsable Item where
    parse "key" = Just Key
    parse "spoon" = Just Spoon
    parse _ = Nothing

instance Parsable Direction where
    parse "north" = Just North
    parse "east" = Just East
    parse "south" = Just South
    parse "west" = Just West
    parse _ = Nothing

instance Parsable Command where
    parse ('g' : 'o' : ' ' : value) = case parse value of
        Just dir -> Just (Move dir)
        Nothing -> Nothing
    parse ('g' : 'r' : 'a' : 'b' : ' ' : value) = case parse value of
        Just item -> Just (PickUp item)
        Nothing -> Nothing
    parse ('u' : 's' : 'e' : ' ' : value) = case parse value of
        Just item -> Just (Use item)
        Nothing -> Nothing
    parse "end" = Just End
    parse _  = Nothing

tellResponse :: String -> IO ()
tellResponse message = putStrLn $ "< " ++ message ++ "."

readCommand :: IO (Maybe Command)
readCommand = do
    putStr "> "
    fmap parse getLine

deleteFrom :: Eq a => a -> [(a, b)] -> [(a, b)]
deleteFrom _ [] = []
deleteFrom x ((xa, xb):xs) | x == xa = deleteFrom x xs
                           | otherwise = (xa, xb) : deleteFrom x xs

leaveRoom :: Room -> Direction -> Room -> Room
leaveRoom fromRoom dir toRoom = toRoom{doors = (opposite dir, fromRoom) : deleteFrom (opposite dir) (doors toRoom)}

step :: Command -> GameState -> Next GameState
step (Move dir) gameState = case lookup dir $ doors $ room gameState of
    Just toRoom -> case requires toRoom of
        Just requiredItem -> if requiredItem `elem` inventory (player gameState) then
            Progress "You enter the room using a required item" gameState{room = leaveRoom (room gameState) dir toRoom}
            else Same "You don't have the required item to enter this room!"
        Nothing -> Progress "You enter the room unhindered" gameState{room = leaveRoom (room gameState) dir toRoom}
    Nothing -> Same "Room undefined!" 
step (PickUp item) gameState = case unzip (items (room gameState)) of
    (beforeItems,_) -> 
        if item `elem` beforeItems then
            Progress "Item picked up" gameState{
                room = (room gameState) {items = deleteFrom item (items (room gameState))},
                player = (player gameState) {inventory = item : inventory (player gameState)}
            }
        else
            Same "Item doesn't exist!"
step (Use item) gameState =
    if item `elem` inventory (player gameState) then
        actions (room gameState) item gameState
    else
        Same "Item doens't exist in inventory"

play :: GameState -> IO ()
play gameState = do
    tellContext gameState
    playLoop gameState

playLoop :: GameState -> IO ()
playLoop gameState | isWinRoom (room gameState) = do
                        tellResponse "You have reached the end!"
                   | otherwise = do
                        maybeCommand <- readCommand
                        case maybeCommand of
                            Nothing -> do
                                tellResponse "Invalid command!"
                                playLoop gameState
                            Just command -> 
                                case command of
                                    End -> tellResponse "Thank you for playing!"
                                    _ -> case step command gameState of
                                        Same message -> do
                                            tellResponse message
                                            playLoop gameState 
                                        Progress message gameState -> do
                                            tellResponse message
                                            tellContext gameState
                                            playLoop gameState