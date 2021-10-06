module Threads
  ( philosophers
  ) where

import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                , yield
                                                )
import qualified Control.Concurrent.Lock       as Lock
import           Control.Monad                  ( forM_
                                                , forever
                                                )
import qualified System.IO                     as Sys
import qualified System.Random                 as Random

newtype Philosopher = Philosopher { run :: IO () }

philosopher :: String -> Lock.Lock -> Lock.Lock -> Philosopher
philosopher name left right = Philosopher $ forever $ do
  putStrLn $ name ++ ": Thinking"
  Random.randomRIO (100000, 200000) >>= threadDelay
  Lock.with left $ do
    yield
    Lock.with right $ do
      putStrLn $ name <> ": Eating"
      Random.randomRIO (100000, 200000) >>= threadDelay

size = 5

rotate :: Int -> [a] -> [a]
rotate = drop <> take

philosophers :: IO ()
philosophers = do
  Sys.hSetBuffering Sys.stdout Sys.LineBuffering
  chops <- sequence $ replicate size Lock.new
  let philosophers =
        zipWith3 philosopher (map show [1 ..]) chops (rotate 1 chops)
  forM_ philosophers (forkIO . run)

  forever $ threadDelay 10000000

