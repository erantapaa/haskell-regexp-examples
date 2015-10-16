{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module RegexExamples where

--
-- This file contains runnable examples of how to use all 27 instances of RegexContext.
--

import Control.Monad
import Data.Array
import qualified Data.Text as T

--
-- regex package selection
--

-- import Text.Regex.Posix       -- regex-posix
-- import Text.Regex.TDFA        -- regex-tdfa
-- import Text.Regex.TDFA.Text   -- regex-tdfa-text

import Text.Regex.PCRE           -- regex-pcre / regex-pcre-builtin


str = "a bbbb aa b" :: String
regex = "(a+) (b+)" :: String

-- bool: first match, returns True or False
exampleBool = do
  if str =~ regex then putStrLn "matched" else putStrLn "no matches"
-- Context: RegexContext a b Bool 

-- count: all matches, returns count
exampleCount = do
  if (str =~ regex :: Int) > 3
    then putStrLn "matched more than 3 times"
    else putStrLn "did not match more than 3 times"
-- Context: RegexContext a b Int

-- null: always returns ()
exampleNull = do
  let matches = str =~ regex :: ()
  print matches
-- Context: RegexContext a b ()

--
-- First match only using MatchArray and MatchResult.
--

-- first match, all captures, returns offset and length
exampleMatchArray = do
  let matches = str =~ regex :: MatchArray
      (lo,hi) = bounds matches
  if hi > 0 then putStrLn $ "matched and capture 1 has offset and length " ++ (show $ matches ! 1)
            else putStrLn $ "no match"
  print matches
-- Context: RegexContext a b MatchArray

-- first match, all captures, returns text, includes before and after text
exampleMatchResult = do
  let matches = str =~ regex :: MatchResult String
  -- Note: there is no Show instance for MatchResult
  putStrLn $ "before text         : " ++ (mrBefore matches)
  putStrLn $ "after text          : " ++ (mrAfter matches)
  putStrLn $ "match text          : " ++ (mrMatch matches)
  putStrLn $ "captures 1-n (list) : " ++ show (mrSubList matches)
  putStrLn $ "all captures (array): " ++ show (mrSubs matches)
-- Context: RegexContext a b (MatchResult b)

--
-- First match only using getAllTextSubmatches.
--

-- first match, all captures, returns text
exampleAllTextSubmatchesArrayText = do
  let matches = getAllTextSubmatches $ str =~ regex :: Array Int String
  print matches
-- Context: RegexContext a b (AllTextSubmatches (Array Int) b)

-- first match, all captures, returns text
exampleAllTextSubmatchesListText = do
  let matches = getAllTextSubmatches $ str =~ regex :: [String]
  print matches
-- Context: RegexContext a b (AllTextSubmatches [] b)

-- first match, all captures, returns text, offset and length
exampleAllTextSubmatchesListTextOffsetLength = do
  let matches = getAllTextSubmatches $ str =~ regex :: [(String,(MatchOffset,MatchLength))]
  print matches
-- Context: RegexContext a b (AllTextSubmatches [] (b, (MatchOffset, MatchLength)))

-- first match, all captures, returns text, offset and length
exampleAllTextSubmatchesArrayTextOffsetLength = do
  let matches = getAllTextSubmatches $ str =~ regex :: Array Int (String,(MatchOffset,MatchLength))
  print matches
-- Context: RegexContext a b (AllTextSubmatches (Array Int) (b, (MatchOffset, MatchLength)))

-- first match, all captures, returns offset and length
exampleAllSubmatchesListOffsetLength = do
  let matches = getAllSubmatches $ str =~ regex :: [(MatchOffset,MatchLength)]
  print matches
-- Context: RegexContext a b (AllSubmatches [] (MatchOffset, MatchLength))

--
-- First match only using tuples.
--

-- first match, 0-th capture, returns offset and length
exampleOffsetLength = do
  let matches = str =~ regex :: (MatchOffset,MatchLength)
  print matches
-- Context: RegexContext a b (MatchOffset, MatchLength)

-- first match, 0-th capture, returns text including before and after text
exampleBeforeMatchAfter = do
  let matches = str =~ regex :: (String,String,String)
  print matches
-- Context: RegexContext a b (b, b, b)

-- first match, all captures, returns text, offset and length including before and after text
exampleBeforeMatchTextAfter = do
  let matches = str =~ regex :: (String,MatchText String,String)
  print matches
-- Context: RegexContext a b (b, MatchText b, b)

-- first match, all captures, returns text
exampleBeforeMatchAfterWithCaptures = do
  let matches = str =~ regex :: (String,String,String,[String])
  print matches
-- Context: RegexContext a b (b, b, b, [b])

--
-- All matches returning a list.
--

-- all matches, all captures, returns text
exampleListOfLists = do
  let matches = str =~ regex :: [[String]]
  print matches
-- Context: RegexContext a b [[b]]

-- all matches, all captures, returns text, offset and length
exampleListOfMatchesText = do
  let matches = str =~ regex :: [MatchText String]
  print matches
-- Context: RegexContext a b [MatchText b]

-- all matches, all captures, returns offset and length
exampleListOfMatchesArray = do
  let matches = str =~ regex :: [MatchArray]
  print matches
-- Context: RegexContext a b [MatchArray]

--
-- All matches using getAllTextMatches
--

-- all matches, all captures, returns text
exampleAllTextMatchesArrayArray = do
  let matches = getAllTextMatches (str =~ regex) :: Array Int (Array Int String)
  print matches
-- Context: RegexContext a b (AllTextMatches (Array Int) (Array Int b))

-- all matches, all captures, returns text
exampleAllTextMatchesListArray = do
  let matches = getAllTextMatches $ str =~ regex :: [Array Int String]
  print matches
-- Context: RegexContext a b (AllTextMatches [] (Array Int b))

-- all matches, all captures, returns text
exampleAllTextMatchesArrayList = do
  let matches = getAllTextMatches $ str =~ regex :: Array Int [String]
  print matches
-- Context: RegexContext a b (AllTextMatches (Array Int) [b])

-- all matches, 0-th capture, returns text
exampleAllTextMatchesArrayText = do
  let matches = getAllTextMatches $ str =~ regex :: Array Int String
  print matches
-- Context: RegexContext a b (AllTextMatches (Array Int) b)

-- all matches, 0-th capture, return text
exampleAllTextMatchesListText = do
  let matches = getAllTextMatches $ str =~ regex :: [String]
  print matches
-- Context: RegexContext a b (AllTextMatches [] b)

-- all matches, all captures, returns text, offset and length
exampleAllTextMatchesArrayMatchText = do
  let matches = getAllTextMatches $ str =~ regex :: Array Int (MatchText String)
  print matches
-- Context: RegexContext a b (AllTextMatches (Array Int) (MatchText b))

--
-- All matches using getAllMatches
--

-- all matches, all captures, returns offset and length
exampleAllMatchesArrayMatchArray = do
  let matches = getAllMatches $ str =~ regex :: Array Int MatchArray
  print matches
-- Context: RegexContext a b (AllMatches (Array Int) MatchArray)

-- all matches, 0-th capture, returns offset and length
exampleAllMatchesArrayOffsetLength = do
  let matches = getAllMatches $ str =~ regex :: Array Int (MatchOffset,MatchLength)
  print matches
-- Context: RegexContext a b (AllMatches (Array Int) (MatchOffset, MatchLength))

-- all matches, 0-th capture, returns offset and length
exampleAllMAtchesListOffsetLength = do
  let matches = getAllMatches $ str =~ regex :: [(MatchOffset,MatchLength)]
  print matches
-- Context: RegexContext a b (AllMatches [] (MatchOffset, MatchLength))

