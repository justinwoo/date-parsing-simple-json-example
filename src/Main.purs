module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except.Trans (except, lift, runExceptT)
import Data.Either (Either(..))
import Data.Foreign (ForeignError)
import Data.JSDate (JSDate, LOCALE, parse, toDateString)
import Data.List.NonEmpty (NonEmptyList)
import Data.Record (set)
import Simple.JSON (readJSON)
import Type.Prelude (SProxy(..))

type MyThing =
  { a :: String
  , b :: JSDate
  }

readJSONMyThing :: forall e
   . String
  -> Eff ( locale :: LOCALE | e) (Either (NonEmptyList ForeignError) MyThing)
readJSONMyThing jsonString = runExceptT do
  record <- except $ readJSON jsonString

  -- knows that record.b should be parsed as `String` because of this usage
  jsDate <- lift $ parse record.b

  -- set record.b from `String` to `JSDate`
  pure $ set bProxy jsDate record :: MyThing
  where
    bProxy = SProxy :: SProxy "b"

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

main :: forall e. Eff (locale :: LOCALE, console :: CONSOLE | e) Unit
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
