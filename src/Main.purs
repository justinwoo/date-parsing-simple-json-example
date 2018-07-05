module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Control.Monad.Except.Trans (except, lift, runExceptT)
import Data.Either (Either(..))
import Foreign (ForeignError)
import Data.JSDate (JSDate, parse, toDateString)
import Data.List.NonEmpty (NonEmptyList)
import Simple.JSON (readJSON)

type MyThing =
  { a :: String
  , b :: JSDate
  }

readJSONMyThing :: String -> Effect (Either (NonEmptyList ForeignError) MyThing)
readJSONMyThing jsonString = runExceptT do
  record <- except $ readJSON jsonString

  -- knows that record.b should be parsed as `String` because of this usage
  jsDate <- lift $ parse record.b

  -- set record.b from `String` to `JSDate`
  pure $ record { b = jsDate }

testJson1 :: String
testJson1 = """
{
  "a": "asdf",
  "b": "2017-11-04T13:25:50.589Z"
}
"""

testJson2 :: String
testJson2 = """
{
  "a": "asdf",
  "b": "Sat Nov 04 2017 15:23:27 GMT+0200 (FLE Standard Time)"
}
"""

testJson3 :: String
testJson3 = """
{
  "a": "asdf",
  "b": "2017-11-04T14:01:15.445Z"
}
"""

main :: Effect Unit
main = do
  logDate =<< readJSONMyThing testJson1
  logDate =<< readJSONMyThing testJson2
  logDate =<< readJSONMyThing testJson3

  -- output
  -- Sat Nov 04 2017
  -- Sat Nov 04 2017
  -- Sat Nov 04 2017
  where
    logDate (Right myThing) = do
      log $ toDateString myThing.b
    logDate (Left err) = do
      log $ "failed to parse: " <> show err
