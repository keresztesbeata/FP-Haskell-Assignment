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
              let newEmptyDB = DB.empty
              DB.save newEmptyDB
              return ();

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  loadedDB <- DB.load
  case loadedDB of
    (Error err) -> 
      print err
    (Success db) ->
      let
        foundEntry = DB.findFirst (\entry -> entryId entry == getOptId getOpts) db
      in
        case foundEntry of
            Nothing -> 
              print "No entry was found"
            Just entry -> 
              print $ show (FmtEntry entry)

-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  db <- DB.load
  case db of
    (Error err) -> 
      print err
    (Success snippetDB) ->
      let
        foundEntries = DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) snippetDB
      in
        case foundEntries of
          [] -> 
            print "No entries found"
          _ -> 
            print $ (show . fmap FmtEntry) foundEntries

-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  fileContent <- readFile (addOptFilename addOpts)
  loadedDB <- DB.load
  let
    db = getSuccess loadedDB DB.empty
    existingEnity = DB.findFirst (\entry -> entry == addOptsToEntry (entryId entry) fileContent addOpts) db
    in
      case existingEnity of
        Just _ -> print "Entry with this content already exists: " 
        Nothing -> do
                      DB.modify (DB.insertWith (\id -> makeEntry id fileContent addOpts))
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

addOptsToEntry :: Int -> String -> AddOptions -> Entry
addOptsToEntry id snippet addOpts =
  Entry
  { entryId = id
  , entrySnippet = snippet
  , entryFilename = addOptFilename addOpts
  , entryLanguage = addOptLanguage addOpts
  , entryDescription = addOptDescription addOpts
  , entryTags = addOptTags addOpts
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
