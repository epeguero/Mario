import Control.Monad
import Data.Maybe
import System.IO
import qualified Data.Map.Strict as M

type World = Int
data State = InWorld | Exit deriving (Show)
data Game = Game {world :: World, state :: State} deriving (Show)
data Event = WorldEvent (World -> World) | StateEvent (State -> State)

type UserInput = Char

initialGame :: Game
initialGame = Game {world = 0, state = InWorld}

main :: IO ()
main = do
  hSetEcho stdin False
  gameLoop initialGame userInputAction
  where userInputAction :: IO(String)
        userInputAction = getLine

gameLoop :: Game -> IO (String) -> IO()
gameLoop game@(Game {world = _, state = Exit}) _ = return ()
gameLoop game userInput = do  
  updateGameUI game
  events <- getEvents game userInput; putStrLn . ("Number of events: " ++) . show . length $ events;
  let game' = updateGame game events 
    in case state game' of 
      InWorld -> gameLoop game' userInput
      Exit    -> return ()

updateGame :: Game -> [Event] -> Game
updateGame game [] = game
updateGame game@(Game {world = _, state = Exit}) _ = game
updateGame game (WorldEvent e:es) = updateGame (game {world = e . world $ game}) es
updateGame game (StateEvent e:es) = updateGame (game {state = e . state $ game}) es

getEvents :: Game -> IO (String) -> IO ([Event])
getEvents (Game {world = w, state = s}) userInputAction = map (maybe (StateEvent (const Exit)) id) . filter isJust . (++getWorldEvents w) <$> getUserEvents 
  where
    getWorldEvents :: World -> [Maybe Event]
    getWorldEvents val = [if val < 5 then Just . WorldEvent $ (+1) else Nothing] 

    getUserEvents :: IO [Maybe Event]
    getUserEvents = map (flip M.lookup (keyBindings s)) <$> userInputAction
    
    keyBindings :: State -> M.Map UserInput Event
    keyBindings Exit = M.fromList []
    keyBindings InWorld = M.fromList 
        [   ('\n',WorldEvent (+1)) , 
            ('r', WorldEvent (const 0)), 
            ('q', StateEvent (const Exit))]


updateGameUI :: Game -> IO()
updateGameUI = print . world



