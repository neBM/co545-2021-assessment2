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

deleteFrom :: Eq a => a -> [(a, b)] -> [(a, b)]
deleteFrom _ [] = []
deleteFrom x ((xa, xb):xs) | x == xa = deleteFrom x xs
                           | otherwise = (xa, xb) : deleteFrom x xs

leaveRoom :: Room -> Direction -> Room -> Room
leaveRoom fromRoom dir toRoom = toRoom{doors = (opposite dir, fromRoom) : deleteFrom (opposite dir) (doors toRoom)}

exists :: Eq a => a -> [a] -> Bool
exists _ [] = False
exists y (x:xs) | y == x = True
                | otherwise = exists y xs

