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

import Text.HTML.Scalpel (scrapeURL, attrs, tagSelector)
import Control.Monad (forM_)
import Network.URI (URI(uriScheme), parseURI, isRelativeReference, relativeTo, isAbsoluteURI, parseRelativeReference, parseAbsoluteURI)
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Control.Concurrent.STM (TVar, STM, newTVarIO, atomically, readTVar, writeTVar, modifyTVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, writeTQueue, readTQueue)
import Control.Exception (handle, SomeException)
import Control.Concurrent (newEmptyMVar, forkIO, threadDelay)
import qualified Data.Set as Set
import Data.Set (Set)
import Options.Commander (Named, type (&), Arg, Raw, command_, raw, arg, named, type (+), ProgramT((:+:)), usage)

data Explorer = Explorer
  { queue :: TQueue URI
  , seen  :: TVar (Set URI) 
  , count :: TVar Int }

newExplorerIO :: URI -> IO Explorer
newExplorerIO startURI = do
  queue <- newTQueueIO
  seen <- newTVarIO Set.empty
  count <- newTVarIO 0
  let explorer = Explorer{..}
  atomically (addURI explorer startURI)
  return explorer

addURI :: Explorer -> URI -> STM ()
addURI Explorer{..} uri = do
  uris <- readTVar seen
  if Set.member uri uris && (uriScheme uri == "https" || uriScheme uri == "http")
    then pure ()
    else do
      modifyTVar count (+ 1)
      writeTVar seen (Set.insert uri uris)
      writeTQueue queue uri

readURI :: Explorer -> STM URI
readURI Explorer{..} = readTQueue queue

uriCount :: Explorer -> STM Int
uriCount Explorer{..} = readTVar count

readURIs :: Explorer -> STM (Set URI)
readURIs Explorer{..} = readTVar seen

type ExplorerAPI = Named "explorer" & Arg "where-to-start" String & Raw

main :: IO ()
main = command_ $ named @"explorer" (arg @"where-to-start" (raw . entry)) :+: usage @ExplorerAPI

entry :: String -> IO ()
entry start = do
  let startURI = maybe (error "Could not parse start URI") id (parseURI start)
  print startURI
  x <- newEmptyMVar
  explorer <- newExplorerIO startURI
  sequence_ [ forkIO (go explorer i) | i <- [1..15] ]
  uriPrinter explorer

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

uriPrinter :: Explorer -> IO ()
uriPrinter explorer = do
  _ <- getChar
  c <- atomically $ uriCount explorer
  print c
  uriPrinter explorer
