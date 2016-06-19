module Main(main) where

import Game

import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.QSemN
import Control.Concurrent
import System.IO

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    gameMVar            <- newMVar initialGame
    inputBuffer         <- newMVar []
    eventQueue          <- newMVar [] 
    eventSourceSem      <- newQSemN numEventSources
    turnLock            <- newMVar ()
    let 
        executeGameTurn :: IO ()
        executeGameTurn = do
            threadDelay 1000
            takeMVar turnLock   -- Mark end of the current turn
            waitForEventSourcesToFinish
            game        <- takeMVar gameMVar
            newEvents   <- swapMVar eventQueue []
            userInput   <- swapMVar inputBuffer []
            game'       <- return $ applyEvents game userInput newEvents 
            putMVar gameMVar game'
            if null newEvents
                then return ()
                else updateGameUI game' >> putStrLn ("Events applied: " ++ show (length newEvents)) 
            putMVar turnLock () -- Mark start of the next turn
            
        generateEvents :: EventSource -> IO ()
        generateEvents eventSource = do
            threadDelay $ delay eventSource
            readMVar turnLock
            waitQSemN eventSourceSem 1  -- Start event
            es <- takeMVar eventQueue
            putMVar eventQueue . (es++) =<< events eventSource <$> readMVar gameMVar <*> readMVar inputBuffer 
            signalQSemN eventSourceSem 1  -- End event

        listenForUserInput :: IO ()
        listenForUserInput = do
            readMVar turnLock
            input <- readUserInput
            putMVar inputBuffer . (++input) =<< takeMVar inputBuffer

        applyEvents :: Game -> UserInput -> [Event] -> Game
        applyEvents game _ []           = game
        applyEvents game input (e:es)   = applyEvents (action e game input) input es

        waitForEventSourcesToFinish :: IO ()
        waitForEventSourcesToFinish = do 
            waitQSemN eventSourceSem numEventSources -- Wait for all event sources to finish
            signalQSemN eventSourceSem numEventSources -- Allow event sources to continue

        in do
            updateGameUI initialGame 
            mapM_ (forkIO . forever . generateEvents) eventSources 
            forkIO . forever $ executeGameTurn
            forever listenForUserInput

numEventSources :: Int
numEventSources = length eventSources


