{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module RegexExamples where
{-

This file contains examples of how to use all 27 instances of RegexContext.

A RegexContext determines what information the match operator =~ returns and
how that information is structured.

The match information which can be selected by using the various instances
include:

- which matches to report on (just the first match or all matches)
- which captures to return (i.e. only the 0-th or all captures)
- which details about each capture to return (the match text, offset, length)
- whether to return the before and after text

To use this file first uncomment the appropriate import lines below to select
which regex package you want to use. See the HaskellWiki page for a comparison
of all of the regex packages:

  http://www.haskell.org/haskellwiki/Regular_expressions

Each example has a "Context:" description which indicates which RegexContext instance the
example uses. All of the Regexp instances may be found in in the Text.Regex.Base.RegexLike
module:

  http://hackage.haskell.org/package/regex-base/docs/Text-Regex-Base-RegexLike.html#t:RegexContext

-}

import Control.Monad
import Data.Array
import qualified Data.Text as T

-- for regexp-posix:
-- import Text.Regex.Posix

-- for regex-tdfa:
-- import Text.Regex.TDFA
-- and possibly: Text.Regex.TDFA.Text

-- for regex-pcre / regex-pcre-builtin
import Text.Regex.PCRE

str = "a bbbb aa b" :: String
regex = "(a+) (b+)" :: String

-- bool: first match, returns True or False
boolExample = 
  if str =~ regex then putStrLn "matched" else putStrLn "no matches"
-- Context: RegexContext a b Bool 

-- count: all matches, returns count
countExample =
  if (str =~ regex :: Int) > 3
    then putStrLn "matched more than 3 times"
    else putStrLn "did not match more than 3 times"
-- Context: RegexContext a b Int

-- match-array: 1st match, all captures, returns offset and length
matchArrayExample = do
  let matches = str =~ regex :: MatchArray
      (lo,hi) = bounds matches
  if hi > 0 then putStrLn $ "matched and capture 1 has offset and length " ++ (show $ matches ! 1)
            else putStrLn $ "no match"
  print matches
-- Context: RegexContext a b MatchArray

-- lists-of-lists: all matches, all captures, returns text
listOfListsExample = do
  let matches = str =~ regex :: [[String]]
  print matches
-- Context: RegexContext a b [[b]]

-- list-of-matchtext: all matches, all captures, returns text, offset and length
listOfMatchTextExample = do
  let matches = str =~ regex :: [MatchText String]
  print matches
-- Context: RegexContext a b [MatchText b]

-- list-of-matcharray: all matches, all captures, returns offset and length
listOfMatchArrayyExample = do
  let matches = str =~ regex :: [MatchArray]
  print matches
-- Context: RegexContext a b [MatchArray]

-- matchresult: 1st match, all captures, returns text, includes before and after text
matchResultExample = do
  let matches = str =~ regex :: MatchResult String
  -- Note: there is no Show instance for MatchResult
  putStrLn $ "before text         :" ++ (mrBefore matches)
  putStrLn $ "after text          :" ++ (mrAfter matches)
  putStrLn $ "match text          :" ++ (mrMatch matches)
  putStrLn $ "captures 1-n (list) :" ++ show (mrSubList matches)
  putStrLn $ "all captures (array):" ++ show (mrSubs matches)
-- Context: RegexContext a b (MatchResult b)

-- alltextmatches-array-array: all matches, all captures, returns text
allTextMatchesArrayArrayExample = do
  let matches = getAllTextMatches (str =~ regex) :: Array Int (Array Int String)
  print matches
-- Context: RegexContext a b (AllTextMatches (Array Int) (Array Int b))

-- alltextmatches-list-array: all matches, all captures, returns text
allTextMatchesListArrayExample = do
  let matches = getAllTextMatches $ str =~ regex :: [Array Int String]
  print matches
-- Context: RegexContext a b (AllTextMatches [] (Array Int b))

-- alltextmatches-array-list: all matches, all captures, returns text
allTextMatchesArrayListExample = do
  let matches = getAllTextMatches $ str =~ regex :: Array Int [String]
  print matches
-- Context: RegexContext a b (AllTextMatches (Array Int) [b])

-- alltextmatches-array-text: all matches, 0-th capture, returns text
allTextMatchesArrayTextExample = do
  let matches = getAllTextMatches $ str =~ regex :: Array Int String
  print matches
-- Context: RegexContext a b (AllTextMatches (Array Int) b)

-- alltextmatches-list-text: all matches, 0-th capture, return text
allTextMatchesListTextExample = do
  let matches = getAllTextMatches $ str =~ regex :: [String]
  print matches
-- Context: RegexContext a b (AllTextMatches [] b)

-- alltextmatches-array-matchtext: all matches, all captures, returns text, offset and length
allTextMatchesArrayMatchTextExample = do
  let matches = getAllTextMatches $ str =~ regex :: Array Int (MatchText String)
  print matches
-- Context: RegexContext a b (AllTextMatches (Array Int) (MatchText b))

-- allmatches-array-matcharray: all matches, all captures, returns offset and length
allMatchesArrayMatchArrayExample = do
  let matches = getAllMatches $ str =~ regex :: Array Int MatchArray
  print matches
-- Context: RegexContext a b (AllMatches (Array Int) MatchArray)

-- allmatches-array-offset-length: all matches, 0-th capture, returns offset and length
allMatchesArrayOffsetLengthExample = do
  let matches = getAllMatches $ str =~ regex :: Array Int (MatchOffset,MatchLength)
  print matches
-- Context: RegexContext a b (AllMatches (Array Int) (MatchOffset, MatchLength))

-- allmatches-list-offset-length: all matches, 0-th capture, returns offset and length
allMatchesListOffsetLengthExample = do
  let matches = getAllMatches $ str =~ regex :: [(MatchOffset,MatchLength)]
  print matches
-- Context: RegexContext a b (AllMatches [] (MatchOffset, MatchLength))

-- alltextsubmatches-array-text: 1st match, all captures, returns text
allTextSubmatchesArrayTextExample = do
  let matches = getAllTextSubmatches $ str =~ regex :: Array Int String
  print matches
-- Context: RegexContext a b (AllTextSubmatches (Array Int) b)

-- alltextsubmatches-list-text: 1st match, all captures, returns text
allTextSubmatchesListTextExample = do
  let matches = getAllTextSubmatches $ str =~ regex :: [String]
  print matches
-- Context: RegexContext a b (AllTextSubmatches [] b)

-- alltextsubmatches-list-text-offset-length: 1st match, all captures, returns text, offset and length
allTextSubmatchesListTextOffsetLengthExample = do
  let matches = getAllTextSubmatches $ str =~ regex :: [(String,(MatchOffset,MatchLength))]
  print matches
-- Context: RegexContext a b (AllTextSubmatches [] (b, (MatchOffset, MatchLength)))

-- altextsubmatches-array-text-offset-length: 1st match, all captures, returns text, offset and length
allTextSubmatchesArrayTextOffsetLengthExample = do
  let matches = getAllTextSubmatches $ str =~ regex :: Array Int (String,(MatchOffset,MatchLength))
  print matches
-- Context: RegexContext a b (AllTextSubmatches (Array Int) (b, (MatchOffset, MatchLength)))

-- allsubmatches-list-offset-length: 1st match, all captures, returns offset and length
allSubmatchesListOffsetLengthExample = do
  let matches = getAllSubmatches $ str =~ regex :: [(MatchOffset,MatchLength)]
  print matches
-- Context: RegexContext a b (AllSubmatches [] (MatchOffset, MatchLength))

-- offset-length: 1st match, 0-th capture, returns offset and length
offsetLengthExample = do
  let matches = str =~ regex :: (MatchOffset,MatchLength)
  print matches
-- Context: RegexContext a b (MatchOffset, MatchLength)

-- beforeMatchAfter:  1st match, 0-th capture, returns text including before and after text
beforeMatchAfterExample = do
  let matches = str =~ regex :: (String,String,String)
  print matches
-- Context: RegexContext a b (b, b, b)

-- beforeMatchTextAfter: 1st match, all captures, returns text, offset and length including before and after text
beforeMatchTextAfterExample = do
  let matches = str =~ regex :: (String,MatchText String,String)
  print matches
-- Context: RegexContext a b (b, MatchText b, b)

-- beforeMatchAfterWithCaptures: 1st match, all captures, returns text
beforeMatchAfterWithCapturesExample = do
  let matches = str =~ regex :: (String,String,String,[String])
  print matches
-- Context: RegexContext a b (b, b, b, [b])

-- null: always returns ()
nullExample = do
  let matches = str =~ regex :: ()
  print matches

