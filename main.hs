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
    gameMVar                <- newMVar initialGame
    inputBuffer             <- newMVar []
    eventQueue              <- newMVar [] 
    eventSourceSem          <- newQSemN numEventSources
    turnLock                <- newMVar ()
    bufferedEventSources    <- zip eventSources <$> makeBuffers numEventSources
    let 
        executeGameTurn :: IO ()
        executeGameTurn = do
            threadDelay 1000
            takeMVar turnLock   -- Mark end of the current turn
            waitForEventSourcesToFinish
            game        <- takeMVar gameMVar
            newEvents   <- swapMVar eventQueue []
            game'       <- return $ applyEvents game newEvents 
            putMVar gameMVar game'
            if null newEvents
                then return ()
                else updateGameUI game' >> putStrLn ("Events applied: " ++ show (length newEvents)) 
            putMVar turnLock () -- Mark start of the next turn
            
        generateEvents :: (EventSource, MVar UserInput) -> IO ()
        generateEvents (eventSource, inputBuffer) = do
            threadDelay $ delay eventSource
            readMVar turnLock
            waitQSemN eventSourceSem 1  -- Start event
            es <- takeMVar eventQueue
            putMVar eventQueue . (es++) =<< events eventSource <$> readMVar gameMVar <*> swapMVar inputBuffer []
            signalQSemN eventSourceSem 1  -- End event

        listenForUserInput :: IO ()
        listenForUserInput = do
            readMVar turnLock
            input <- readUserInput
            mapM_ (\(_,buff) -> takeMVar buff >>= putMVar buff . (++input)) bufferedEventSources

        applyEvents :: Game -> [Event] -> Game
        applyEvents game []     = game
        applyEvents game (e:es) = applyEvents (action e game) es

        waitForEventSourcesToFinish :: IO ()
        waitForEventSourcesToFinish = do 
            waitQSemN eventSourceSem numEventSources -- Wait for all event sources to finish
            signalQSemN eventSourceSem numEventSources -- Allow event sources to continue

        in do
            updateGameUI initialGame 
            mapM_ (forkIO . forever . generateEvents) bufferedEventSources
            forkIO . forever $ executeGameTurn
            forever listenForUserInput

makeBuffers :: Int -> IO [MVar [a]]
makeBuffers n
    | n <= 0    = return []
    | otherwise = (liftM2 (:)) (newMVar []) (makeBuffers (n-1))

numEventSources :: Int
numEventSources = length eventSources


