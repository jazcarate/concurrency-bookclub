module FP
  ( server
  ) where

import qualified Control.Concurrent            as C
import qualified Control.Concurrent.MVar       as M
import           Control.Monad                  ( join )
import qualified Data.ByteString.Lazy          as B
import           Data.Foldable                  ( traverse_ )
import           Data.Functor                   ( void )
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import           Data.Text.Encoding            as T
import           Data.Text.IO                  as T
import qualified Data.Text.Read                as Read
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

requestBody :: Wai.Request -> IO T.Text
requestBody req = T.decodeUtf8 <$> B.toStrict <$> Wai.strictRequestBody req

ok :: Wai.Response
ok = Wai.responseBuilder status200 mempty "Ok"

{-
(def snippets (repeatedly promise))

(future 
  (doseq [snippet (map deref snippets)]
    (println snippet)))

(defn accept-snippet [n text]
  (deliver (nth snippets n) text))

(defroutes app-routes
  (PUT "/snippet/:n" [n :as {:keys [body]}]
    (accept-snippet (edn/read-string n) (slurp body))
    (response "OK")))

(defn -main [& args]
  (run-jetty (site app-routes) {:port 3000}))
-}

server :: IO ()
server = do
  let port = 3000
  snippets <- sequence $ replicate 10 promise
  future $ do
    traverse_ (T.putStrLn =<<) (fmap deref snippets)
  T.putStrLn $ "????  http://localhost:" <> (T.pack $ show port) <> "/"
  Warp.run port $ app snippets


app :: [Promise T.Text] -> Wai.Application
app snippets req respond = case Wai.pathInfo req of
  ["snippet", path] -> case Read.decimal path of
    Right (num, _) -> do
      body <- requestBody req
      deliver (snippets !! num ) body
      respond $ ok
