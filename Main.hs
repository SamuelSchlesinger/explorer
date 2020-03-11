{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Text.HTML.Scalpel
import Control.Monad.IO.Class
import Control.Monad
import Debug.Trace
import Data.Char
import System.Environment
import Control.Applicative
import Network.URI
import Data.Functor
import Data.Maybe
import Debug.Trace
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Exception
import Control.Concurrent
import qualified Data.Set as Set
import Data.Set (Set)
import Options.Commander

data Explorer = Explorer
  { queue :: TQueue URI
  , seen  :: TVar (Set URI) }

newExplorerIO :: URI -> IO Explorer
newExplorerIO startURI = do
  queue <- newTQueueIO
  seen <- newTVarIO Set.empty
  let explorer = Explorer{..}
  atomically (addURI explorer startURI)
  return explorer

addURI :: Explorer -> URI -> STM ()
addURI Explorer{..} uri = do
  uris <- readTVar seen
  if Set.member uri uris 
    then pure ()
    else do
      writeTVar seen (Set.insert uri uris)
      writeTQueue queue uri

readURI :: Explorer -> STM URI
readURI Explorer{..} = readTQueue queue

uriCount :: Explorer -> STM Int
uriCount Explorer{..} = Set.size <$> readTVar seen

readURIs :: Explorer -> STM (Set URI)
readURIs Explorer{..} = readTVar seen

type ExplorerAPI = Named "explorer" & Arg "where-to-start" String & Raw

main :: IO ()
main = command_ $ named @"explorer" (arg @"where-to-start" (raw . entry)) :+: usage @ExplorerAPI

entry :: String -> IO ()
entry start = do
  let startURI = maybe (error "Could not parse start URI") id (parseURI start)
  x <- newEmptyMVar
  explorer <- newExplorerIO startURI
  sequence_ [ forkIO (go explorer i) | i <- [1..15] ]
  waitForInput explorer

waitForInput :: Explorer -> IO ()
waitForInput explorer = do
  _ <- getChar
  (Set.toList -> uris) <- atomically (readURIs explorer)
  traverse print uris
  print (length uris)

perhapsRelativeTo :: URI -> String -> Maybe URI
perhapsRelativeTo startURI path
  | isRelativeReference path = parseRelativeReference path <&> (`relativeTo` startURI)
  | isAbsoluteURI path = parseAbsoluteURI path
  | otherwise = Nothing

go :: Explorer -> Int -> IO ()
go explorer i = do
  startURI <- atomically (readURI explorer)
  (maybe [] id -> paths) <- (\(_ :: SomeException) -> pure (Just [])) `handle` scrapeURL (show startURI) (mapMaybe (perhapsRelativeTo startURI) <$> attrs "href" (tagSelector "a"))
  atomically $ forM_ paths (addURI explorer)
  go explorer i