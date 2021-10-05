module FP where

import qualified Control.Concurrent            as C
import qualified Control.Concurrent.MVar       as M
import           Data.Foldable                  ( traverse_ )
import           Data.Functor                   ( void )
import qualified Data.Monoid
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import           Network.HTTP.Types             ( status200 )
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp

recursiveSum :: [Int] -> Int
recursiveSum []       = 0
recursiveSum (x : xs) = x + recursiveSum xs


data Promise a = Promise
  { unPromise :: M.MVar a
  }

instance Show a => Show (Promise a) where
  show p = unsafePerformIO $ do
    val <- M.tryReadMVar $ unPromise p
    pure $ maybe "Pending..." show val

promise :: IO (Promise a)
promise = Promise <$> M.newEmptyMVar

future :: IO () -> IO ()
future = void . C.forkIO

deref :: Promise a -> IO a
deref = M.readMVar . unPromise

deliver :: Promise a -> a -> IO ()
deliver = M.putMVar . unPromise

{- main :: IO ()
main = do
  meaningOfLife <- promise
  future $ do
    mol <- deref meaningOfLife
    putStrLn $ "The meaning of life is: " <> show mol
  deliver meaningOfLife 42 -}

main :: IO ()
main = do
  let port = 3000
  future $ do
    traverse_
        (\p' -> do
          p <- p'
          deref p
        )
      $ repeat promise
  putStrLn $ "🚀  http://localhost:" <> show port <> "/"
  Warp.run port $ app 3

app :: a -> Wai.Application
app _ req respond = respond $ case Wai.pathInfo req of
  ["yay"] -> yay
  x       -> index x

yay :: Wai.Response
yay = Wai.responseLBS status200 [("Content-Type", "text/plain")] "yay"

index :: [T.Text] -> Wai.Response
index x = Wai.responseLBS status200 [("Content-Type", "text/html")] $ mconcat
  [ "<p>Hello from "
  , fromString $ show x
  , "!</p>"
  , "<p><a href='/yay'>yay</a></p>\n"
  ]
