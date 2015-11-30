{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Lens
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Streaming.Char8 (toChunks, lines, fromChunks, toLazy)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Streaming
import Control.Foldl (impurely, FoldM(..))
import Pipes (Producer, (>->))
import Prelude hiding (lines)
import Streaming hiding (run)
import System.Random.PCG
import qualified Data.ByteString.Streaming.Char8 as SBS
import qualified Pipes
import qualified Pipes.ByteString as Pipes
import qualified Pipes.Group as Pipes
import qualified Pipes.Prelude as Pipes
import qualified Streaming.Prelude as Streaming

toProducer :: Monad m => Stream (Of a) m r -> Producer a m r
toProducer = Pipes.unfoldr Streaming.next

fromProducer :: Monad m => Producer a m r -> Stream (Of a) m r
fromProducer = Streaming.unfoldr Pipes.next

-- testStream :: Request -> Stream (Of ByteString) IO ()
-- testStream req = toChunks $ _
--                           $ dropHalf
--                           $ _
--                           $ lines
--                           $ fromChunks
--                           $ streamingRequest req

testStream :: Request -> Stream (Of ByteString) IO ()
testStream req = streamingRequest req

-- testStream :: MonadIO m => Request -> Producer ByteString m r
-- testStream req = impurely Pipes.foldsM (FoldM step _ _) (view Pipes.lines p)
--   where p = toProducer (streamingRequest req)
--         step x a = do
--           b <- liftIO $ withSystemRandom uniformBool
--           when b (Pipes.yield a)


dropHalf :: Stream (Of b) IO r -> Stream (Of b) IO r
dropHalf = Streaming.filterM (const (withSystemRandom uniformBool))

app :: Application
app req respond = respond res
  where res = streamingResponse (testStream req) status200 []

main :: IO ()
main = do
  Prelude.putStrLn "Starting test server on port 8089"
  run 8089 app
