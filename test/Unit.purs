module Test.Unit (combinatorUnitTests) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Request.Duplex (RequestDuplex', as, boolean, default, flag, int, many, many1, optional, param, params, parse, path, prefix, print, prop, record, rest, root, segment, string, suffix)
import Request.Duplex.Parser (RouteError(..))
import Request.Duplex.Types (Method(..))
import Test.Assert (assertEqual)

req url = { method: Get, url }

combinatorUnitTests :: Effect Unit
combinatorUnitTests = do
  -- boolean
  assertEqual { actual: parse (boolean segment) $ req "true", expected: Right $ true }
  assertEqual { actual: parse (boolean segment) $ req "false", expected: Right $ false }
  assertEqual { actual: parse (boolean segment) $ req "x", expected: Left (Expected "Boolean" "x") }
  assertEqual { actual: parse (boolean segment) $ req "", expected: Left EndOfPath }
  -- prefix
  assertEqual { actual: parse (prefix "api" segment) $ req "api/a", expected: Right "a" }
  assertEqual { actual: parse (prefix "api" segment) $ req "api/a", expected: Right "a" }
  assertEqual { actual: parse (prefix "/api/v1" segment) $ req "%2Fapi%2Fv1/a", expected: Right "a" }
  assertEqual { actual: parse (prefix "/api/v1" segment) $ req "/api/v1/a", expected: Left (Expected "/api/v1" "") }
  -- path
  assertEqual { actual: parse (path "/api/v1" segment) $ req "/api/v1/a", expected: Right "a" }
  assertEqual { actual: parse (path "/api/v1" segment) $ req "/api/v2/a", expected: Left (Expected "v1" "v2") }
  -- segment
  assertEqual { actual: parse segment $ req "abc", expected: Right "abc" }
  assertEqual { actual: parse segment $ req "abc%20def", expected: Right "abc def" }
  assertEqual { actual: parse segment $ req "abc/def", expected: Right "abc" }
  assertEqual { actual: parse segment $ req "/abc", expected: Right "" }
  assertEqual { actual: parse segment $ req "", expected: Left EndOfPath }
  -- root
  assertEqual { actual: parse (root segment) $ req "/abc", expected: Right "abc" }
  assertEqual { actual: parse (root segment) $ req "abc", expected: Left (Expected "" "abc") }
  assertEqual { actual: parse (root segment) $ req "/", expected: Left EndOfPath }
  -- int
  assertEqual { actual: parse (int segment) $ req "1", expected: Right 1 }
  assertEqual { actual: parse (int segment) $ req "x", expected: Left (Expected "Int" "x") }
  -- param
  assertEqual { actual: parse (param "search") $ req "?search=keyword", expected: Right "keyword" }
  assertEqual { actual: parse (param "search") $ req "/", expected: Left (MissingParam "search") }
  assertEqual { actual: parse (optional (param "search")) $ req "/", expected: Right Nothing }
  -- suffix
  assertEqual { actual: parse (suffix segment "latest") $ req "release/latest", expected: Right "release" }
  assertEqual { actual: parse (suffix segment "latest") $ req "/latest", expected: Right "" }
  assertEqual { actual: parse (suffix segment "x/y") $ req "a/x%2Fy", expected: Right "a" }
  assertEqual { actual: parse (suffix segment "latest") $ req "/", expected: Left EndOfPath }
  assertEqual { actual: parse (suffix segment "x/y") $ req "a/x/y", expected: Left (Expected "x/y" "x") }
  -- rest
  assertEqual { actual: parse rest $ req "", expected: Right [] }
  assertEqual { actual: parse rest $ req "a/b", expected: Right [ "a", "b" ] }
  assertEqual { actual: parse (path "a/b" rest) $ req "a/b/c/d", expected: Right [ "c", "d" ] }
  assertEqual { actual: print rest [ "a", "b" ], expected: req "a/b" }
  -- default
  assertEqual { actual: parse (default 0 $ int segment) $ req "1", expected: Right 1 }
  assertEqual { actual: parse (default 0 $ int segment) $ req "x", expected: Right 0 }
  -- as
  assertEqual { actual: parse (sort segment) $ req "asc", expected: Right Asc }
  assertEqual { actual: parse (sort segment) $ req "x", expected: Left (Expected "asc or desc" "x") }
  -- many1
  assertEqual { actual: parse (many1 (int segment)) $ req "1/2/3/x", expected: Right [ 1, 2, 3 ] }
  assertEqual { actual: parse (many1 (int segment)) $ req "x", expected: Left (Expected "Int" "x") :: Either RouteError (Array Int) }
  -- many
  assertEqual { actual: parse (many (int segment)) $ req "1/2/3/x", expected: Right [ 1, 2, 3 ] }
  assertEqual { actual: parse (many (int segment)) $ req "x", expected: Right [] }
  -- flag
  assertEqual { actual: parse (flag (param "x")) $ req "?x", expected: Right true }
  assertEqual { actual: parse (flag (param "x")) $ req "?x=true", expected: Right true }
  assertEqual { actual: parse (flag (param "x")) $ req "?x=false", expected: Right true }
  assertEqual { actual: parse (flag (param "x")) $ req "?y", expected: Right false }
  -- string
  assertEqual { actual: parse (string segment) $ req "x", expected: Right "x" }
  assertEqual { actual: parse (string segment) $ req "%20", expected: Right " " }
  -- optional
  assertEqual { actual: parse (optional segment) $ req "a", expected: Right (Just "a") }
  assertEqual { actual: parse (optional segment) $ req "", expected: Right Nothing }
  assertEqual { actual: print (optional segment) (Just "a"), expected: req "a" }
  assertEqual { actual: print (optional segment) Nothing, expected: req "" }
  -- record
  assertEqual { actual: parse (path "blog" date) $ req "blog/2019/1/2", expected: Right { year: 2019, month: 1, day: 2 } }
  -- params
  assertEqual { actual: parse search $ req "?page=3&filter=Galaxy%20Quest", expected: Right { page: 3, filter: Just "Galaxy Quest" } }

data Sort
  = Asc
  | Desc

derive instance eqSort :: Eq Sort

instance showSort :: Show Sort where
  show Asc = "asc"
  show Desc = "desc"

sortToString :: Sort -> String
sortToString = case _ of
  Asc -> "asc"
  Desc -> "desc"

sortFromString :: String -> Either String Sort
sortFromString = case _ of
  "asc" -> Right Asc
  "desc" -> Right Desc
  val -> Left $ "asc or desc"

sort :: RequestDuplex' String -> RequestDuplex' Sort
sort = as sortToString sortFromString

date :: RequestDuplex' { year :: Int, month :: Int, day :: Int }
date =
  record
    # prop (SProxy :: _ "year") (int segment)
    # prop (SProxy :: _ "month") (int segment)
    # prop (SProxy :: _ "day") (int segment)

search :: RequestDuplex' { page :: Int, filter :: Maybe String }
search =
  params
    { page: int
    , filter: optional <<< string
    }
