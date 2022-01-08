module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do
              savedDB <- DB.save DB.empty
              case savedDB of
                  (Error err) -> print err
                  (Success db) -> return ()

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
                  db <- DB.load
                  case db of
                    (Error err) -> print err
                    (Success snippetDB) ->
                        let
                          foundEntry = DB.findFirst (\entry -> entryId entry == getOptId getOpts) snippetDB
                        in
                          case foundEntry of
                            Nothing -> print "No entry with given id was found"
                            Just entry -> print $ show (FmtEntry entry)

-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
                      db <- DB.load
                      case db of
                        (Error err) -> print err
                        (Success snippetDB) ->
                          let
                            foundEntries = DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) snippetDB
                            in
                              case foundEntries of
                                [] -> print "No entries found"
                                _ -> print $ show (fmap FmtEntry foundEntries)

-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts =
  return ()
  where
    makeEntry :: Int -> String -> AddOptions -> Entry
    makeEntry id snippet addOpts =
      Entry
        { entryId = id,
          entrySnippet = snippet,
          entryFilename = addOptFilename addOpts,
          entryLanguage = addOptLanguage addOpts,
          entryDescription = addOptDescription addOpts,
          entryTags = addOptTags addOpts
        }

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
