module Main(main) where

import Game

import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent
import System.IO

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
            input       <- readUserInput
            writeEpoch $ userEpoch game input
            generateUserEpoch

        readUserInput :: IO (UserInput)
        readUserInput = getChar

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




