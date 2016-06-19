module Game where

import qualified Data.Map.Lazy as M
import Data.Maybe

type World = Int
data State = InWorld | Exit deriving (Show)
data Game = Game {world :: World, state :: State} deriving (Show)
data Event = Event {action :: Game -> UserInput -> Game, eventName :: String}
data EventSource = EventSource {events :: Game -> UserInput -> [Event], eventSourceName :: String, delay :: Int}
type UserInput = String

instance Show Event where
    show e = show $ eventName e
instance Show EventSource where
    show es = show $ eventSourceName es

eventSources :: [EventSource]
eventSources = [gameEventSource, userEventSource]

testEventSource = EventSource { events = const $ const [testEvent], eventSourceName = "testSource", delay = 1000000} 
testEvent = Event {action = const $ const Game {world = 42, state = InWorld}, eventName = "test"}

gameEventSource :: EventSource
gameEventSource = EventSource { 
    events = \game -> \userInput -> 
        if world game < 20 
            then [
                Event { 
                    action = \game -> \userInput -> game {world = world game - 1}, 
                    eventName = "World Event"
                }
            ]
            else [],
    eventSourceName = "World",
    delay = 1000000
    }

userEventSource :: EventSource
userEventSource = EventSource { 
    events = \game -> \userInput -> 
        if null userInput
            then []
            else [
                Event {
                    action = parseUserInput,
                    eventName = "User Event"
                }
            ],
    eventSourceName = "User",
    delay = 1
    }

    where
        parseUserInput :: Game -> UserInput -> Game
        parseUserInput game [] = game
        parseUserInput game@(Game {state = Exit}) _ = game
        parseUserInput game@(Game {state = InWorld}) (i:ins) =
            let
                keyBindings :: M.Map UserInput Game
                keyBindings = M.fromList
                    [   
                        ("a", game {world = world game + 1}), 
                        ("r", game {world = 0}), 
                        ("q", game {state = Exit})
                    ]
            in  case M.lookup [i] keyBindings of
                    Nothing     -> parseUserInput game ins
                    Just game'  -> parseUserInput game' ins


updateGameUI :: Game -> IO()
updateGameUI = print . world

readUserInput :: IO (UserInput)
readUserInput = (:[]) <$> getChar

initialGame :: Game
initialGame = Game {world = 1, state = InWorld}

