{-# LANGUAGE OverloadedStrings #-}
-- |
-- Multiple socket poller in Haskell (p.43)
-- This version uses poll
--
-- Test it with `wuserver.hs`

module Main where

import Control.Monad (forever, when)
import Data.ByteString.Char8 (unpack)
import System.ZMQ4
import System.Exit
import Control.Applicative ((<$>))

main :: IO ()
main =
    withContext $ \ctx ->
        withSocket ctx Sub $ \subscriber -> do
            connect subscriber "tcp://localhost:10001"
            subscribe subscriber ""
            forever $
                poll (100) [ Sock subscriber [In] (Just $ processEvts subscriber) ]

processEvts :: (Receiver a) => Socket a -> [Event] -> IO ()
processEvts sock evts =
    when (In `elem` evts) $ do
        msg <- unpack <$> receive sock
        putStrLn $ unwords ["Processing", msg]
