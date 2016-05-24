import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent
import Data.Maybe
import System.IO
import qualified Data.Map.Strict as M

type World = Int
data State = InWorld | Exit deriving (Show)
data Game = Game {world :: World, state :: State} deriving (Show)
data Event = WorldEvent (World -> World) | StateEvent (State -> State)
type Epoch = [Event]
type UserInput = Char

initialGame :: Game
initialGame = Game {world = 0, state = InWorld}

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    epochQueue    <- atomically newTQueue
    gameMVar      <- newMVar initialGame
    let   
        progressGame :: IO ()
        progressGame = do
            game    <- readGame
            updateGameUI game
            game'   <- foldl applyEvent game <$> readEpoch
            writeGame game'
            progressGame
            
        generateWorldEpoch :: IO ()
        generateWorldEpoch = do 
            game    <- readGame 
            writeEpoch $ gameWorldEpoch game
            threadDelay 250000
            generateWorldEpoch

        generateUserEpoch :: IO ()
        generateUserEpoch = do 
            game        <- readGame
            input       <- readInput
            case M.lookup input $ keyBindings game of
                Nothing     -> return ()
                Just event  -> writeEpoch [event]
            generateUserEpoch 
            
        updateGameUI :: Game -> IO()
        updateGameUI = print . world

        readInput :: IO (UserInput)
        readInput = getChar

        readGame :: IO (Game)
        readGame = readMVar gameMVar

        writeGame :: Game -> IO ()
        writeGame game = swapMVar gameMVar game >> return ()

        readEpoch :: IO (Epoch)
        readEpoch = atomically $ readTQueue epochQueue

        writeEpoch :: Epoch -> IO ()
        writeEpoch []       = return ()
        writeEpoch epoch    = atomically $ writeTQueue epochQueue epoch

        in do
            forkIO  $ progressGame
            forkIO  $ generateWorldEpoch
            forever $ generateUserEpoch

keyBindings :: Game -> M.Map UserInput Event
keyBindings (Game {state = Exit}) = M.fromList []
keyBindings (Game {state = InWorld}) = M.fromList 
            [   ('a',WorldEvent (+1)) , 
                ('r', WorldEvent (const 0)), 
                ('q', StateEvent (const Exit))]

gameWorldEpoch :: Game -> Epoch
gameWorldEpoch game = 
    foldl (\accu (pred,e) -> if pred game then e:accu else accu) [] 
    [((< 20) . world, WorldEvent $ (subtract 1))]

applyEvent :: Game -> Event -> Game
applyEvent game@(Game {world = _, state = Exit}) _ = game
applyEvent game (WorldEvent e) = game {world = e . world $ game}
applyEvent game (StateEvent e) = game {state = e . state $ game} 



