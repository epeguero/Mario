main = do
  displayWorld initialWorld
  playGame initialWorld

playGame :: World -> IO(World)
playGame world = do
  newWorld <- updateWorld world  
  displayWorld newWorld
  playGame newWorld

updateWorld world = return world
displayWorld (NumWorld x) = print x

data World = NumWorld Int

initialWorld = NumWorld 1

