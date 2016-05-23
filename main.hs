import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent
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
  buffer <- newEmptyMVar
  forkIO $ gameLoop initialGame (maybe "" id <$> tryTakeMVar buffer)
  -- forkIO $ readMVar buffer >>= putStrLn . ("user input: " ++)
  forever $ putMVar buffer =<< getLine --readMVar buffer >>= putStrLn . ("user input: " ++)

gameLoop :: Game -> IO (String) -> IO()
gameLoop game@(Game {world = _, state = Exit}) _ = return ()
gameLoop game userInput = do  
  updateGameUI game
  events <- getEvents game userInput
  let game' = updateGame game events 
    in case state game' of 
      InWorld -> do gameLoop game' userInput
      Exit    -> myThreadId >>= killThread

updateGame :: Game -> [Event] -> Game
updateGame game [] = game
updateGame game@(Game {world = _, state = Exit}) _ = game
updateGame game (WorldEvent e:es) = updateGame (game {world = e . world $ game}) es
updateGame game (StateEvent e:es) = updateGame (game {state = e . state $ game}) es

getEvents :: Game -> IO (String) -> IO ([Event])
getEvents (Game {world = w, state = s}) userInputAction = do 
  threadDelay 500000
  map (maybe (StateEvent (const Exit)) id) . filter isJust . (++getWorldEvents w) <$> getUserEvents 
  where
    getWorldEvents :: World -> [Maybe Event]
    getWorldEvents val = [if val < 5 then Just . WorldEvent $ (+1) else Nothing] 

    getUserEvents :: IO [Maybe Event]
    getUserEvents = map (flip M.lookup (keyBindings s)) <$> userInputAction
    
    keyBindings :: State -> M.Map UserInput Event
    keyBindings Exit = M.fromList []
    keyBindings InWorld = M.fromList 
        [   ('a',WorldEvent (+1)) , 
            ('r', WorldEvent (const 0)), 
            ('q', StateEvent (const Exit))]


updateGameUI :: Game -> IO()
updateGameUI = print . world



