module Routing.Duplex
  ( RouteDuplex(..)
  , RouteDuplex'
  , parse
  , print
  , prefix
  , suffix
  , path
  , root
  , end
  , segment
  , param
  , flag
  , many1
  , many
  , rest
  , default
  , optional
  , as
  , int
  , boolean
  , string
  , record
  , prop
  , (:=)
  , params
  , buildParams
  , class RouteDuplexParams
  , class RouteDuplexBuildParams
  ) where

import Prelude
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Data.Either (Either)
import Data.Foldable (class Foldable, foldMap, foldr)
import Data.Maybe (Maybe)
import Data.Profunctor (class Profunctor)
import Data.String (Pattern(..))
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row as Row
import Prim.RowList (kind RowList, class RowToList, Cons, Nil)
import Record as Record
import Routing.Duplex.Parser (RouteParser)
import Routing.Duplex.Parser as Parser
import Routing.Duplex.Printer (RoutePrinter)
import Routing.Duplex.Printer as Printer
import Type.Data.RowList (RLProxy(..))

-- | The core abstraction of this library. The values of this type can be used both for parsing
-- | values of type `o` from `String` as well as printing values of type `i` into `String`.
-- |
-- | For most purposes, you'll likely want `RouterDuplex'` which uses the same
-- | type for both parameters.
data RouteDuplex r i o
  = RouteDuplex (i -> RoutePrinter r) (RouteParser r o)

-- | A type restricted variant of `RouteDuplex` where input and output are
-- | the same type. This type will typically be your custom `Route` data type
-- | representing valid routes within your application.
type RouteDuplex' r a
  = RouteDuplex r a a

derive instance functorRouteDuplex :: Functor (RouteDuplex r i)

instance applyRouteDuplex :: Apply (RouteDuplex r i) where
  apply (RouteDuplex encl decl) (RouteDuplex encr decr) = RouteDuplex (append <$> encl <*> encr) (decl <*> decr)

instance applicativeRouteDuplex :: Applicative (RouteDuplex r i) where
  pure = RouteDuplex (const mempty) <<< pure

instance profunctorRouteDuplex :: Profunctor (RouteDuplex r) where
  dimap f g (RouteDuplex enc dec) = RouteDuplex (f >>> enc) (g <$> dec)

-- | Uses a given codec to parse a value of type `o` out of String representing
-- | the path, query and fragment (hash) of a URI (see
-- | [URI - generic syntax](https://en.wikipedia.org/wiki/Uniform_Resource_Identifier#Generic_syntax))
-- | or produce a `RouteError` if parsing fails.
parse :: forall i o. RouteDuplex () i o -> String -> Either Parser.RouteError o
parse (RouteDuplex _ dec) = Parser.run dec

-- | Renders a value of type `i` into a String representation of URI path,
-- | query and fragment (hash).
print :: forall i o. RouteDuplex () i o -> i -> String
print (RouteDuplex enc _) = Printer.run <<< enc

-- | Strips (when parsing) or adds (when printing) a given string segment of the
-- | path. **Note:** this combinator only deals with a single segment.
-- | If you pass it a string containing '/' it will [percent encode](https://en.wikipedia.org/wiki/Percent-encoding) it and treat it as single segment.
-- | E.g. `prefix "/api/v1"` will attempt to match single segment `"%2Fapi%2Fv1"` which is probably not what you want.
-- | See `path` if you want to deal with prefixes consisting of multiple segments.
-- |
-- |```purescript
-- | parse (prefix "api" segment) "api/a" == Right "a"
-- |
-- | parse (prefix "/api/v1" segment)) "/api/v1/a" == Left (Expected "/api/v1" "")
-- |
-- | -- contrast with `path`
-- | parse (path "/api/v1" segment)) "/api/v1/a" == Right "a"
-- |```
prefix :: forall a b r. String -> RouteDuplex r a b -> RouteDuplex r a b
prefix s (RouteDuplex enc dec) = RouteDuplex (\a -> Printer.put s <> enc a) (Parser.prefix s dec)

-- | Similar to `prefix`. Strips (when parsing) or adds (when printing) a given
-- | string segment from the end of the path. The same precautions for `prefix` apply here.
suffix :: forall a b r. RouteDuplex r a b -> String -> RouteDuplex r a b
suffix (RouteDuplex enc dec) s = RouteDuplex (\a -> enc a <> Printer.put s) (dec <* Parser.prefix s (pure unit))

-- | Strips (when parsing) or adds (when printing) a given String prefix,
-- | potentially consisting of multiple path segments. Constrast this with `prefix`,
-- | which only deals with single segment.
-- |
-- |```purescript
-- | parse (path "/api/v1" segment) "/api/v1/a" == Right "a"
-- | parse (path "/api/v1" segment) "/api/v2/a" == Left (Expected "v1" "v2")
-- |```
path :: forall a b r. String -> RouteDuplex r a b -> RouteDuplex r a b
path = flip (foldr prefix) <<< String.split (Pattern "/")

-- | Modifies a given codec to require a prefix of '/'.
-- | You can think of it as stripping and adding the '/' at the beginning of path,
-- | failing if it's not there.
-- |
-- |```purescript
-- | parse (root segment) "/abc" == Right "abc"
-- | parse (root segment) "abc" == Left (Expected "" "abc")
-- |
-- | print (root segment) "abc" == "/abc"
-- |```
root :: forall a b r. RouteDuplex r a b -> RouteDuplex r a b
root = path ""

-- | `end codec` will only suceed if `codec` succeeds and there are no
-- | additional path segments remaining to be processed.
-- |
-- |```purescript
-- | parse (end segment) "abc" == Right "abc"
-- | parse (end segment) "abc/def" == Left (ExpectedEndOfPath "def")
-- |```
end :: forall a b r. RouteDuplex r a b -> RouteDuplex r a b
end (RouteDuplex enc dec) = RouteDuplex enc (dec <* Parser.end)

-- | Consumes or prints a single path segment.
-- | **Note:** [URI encoding and decoding](https://en.wikipedia.org/wiki/Percent-encoding) is done automatically.
-- |
-- | ```purescript
-- | parse segment "abc"         == Right "abc"
-- | parse segment "abc%20def"   == Right "abc def" -- automatic decoding of uri components
-- | parse segment "abc/def"     == Right "abc"
-- | parse segment "/abc"        == Right "" -- the empty string before the first '/'
-- | parse (root segment) "/abc" == Right "abc"
-- |
-- | print segment "hello there" == "hello%20there"
-- | print segment "" == "/"
-- | ```
segment :: forall r. RouteDuplex' r String
segment = RouteDuplex Printer.put Parser.take

-- | `param name` consumes or prints a query parameter with the given `name`.
-- | Parsing will fail if the parameter is not there.
-- |
-- |```purescript
-- | parse (param "search") "?search=keyword" == Right "keyword"
-- | parse (param "search") "/"               == Left (MissingParam "search")
-- | parse (optional (param "search")) "/"    == Right Nothing
-- |```
param :: forall r. String -> RouteDuplex' r String
param p = RouteDuplex (Printer.param p) (Parser.param p)

-- | Consumes or prints a query flag (i.e. parameter without value).
-- | **Note:** that this combinator ignores the value of the parameter. It only cares about its presence/absence.
-- | Presence is interpreted as `true`, absence as `false`.
-- |
-- |```purescript
-- | parse (flag (param "x")) "?x"        == Right true
-- | parse (flag (param "x")) "?x=true",  == Right true
-- | parse (flag (param "x")) "?x=false", == Right true -- value is ignored, what matters is presence of the parameter x
-- | parse (flag (param "x")) "?y",       == Right false
-- |```
flag :: forall r. RouteDuplex' r String -> RouteDuplex' r Boolean
flag (RouteDuplex enc dec) = RouteDuplex enc' dec'
  where
  enc' true = enc ""

  enc' _ = mempty

  dec' = Parser.default false (dec $> true)

-- | Repeatedly applies a given codec to parse one or more values from path segments.
-- | Parsing will fail if no segment can be parsed.
-- |
-- |```purescript
-- | parse (many1 (int segment)) "1/2/3/x" == Right [1,2,3]
-- | parse (many1 (int segment)) "x",      == Left (Expected "Int" "x") :: Either RouteError (Array Int)
-- |```
many1 ::
  forall f a b r.
  Foldable f =>
  Alt f =>
  Applicative f =>
  RouteDuplex r a b ->
  RouteDuplex r (f a) (f b)
many1 (RouteDuplex enc dec) = RouteDuplex (foldMap enc) (Parser.many1 dec)

-- | Similar to `many1`, except also succeeds when no values can be parsed.
-- |
-- |```purescript
-- | parse (many (int segment)) "1/2/3/x" == Right [1,2,3]
-- | parse (many (int segment)) "x",      == Right []
-- |```
many ::
  forall f a b r.
  Foldable f =>
  Alternative f =>
  RouteDuplex r a b ->
  RouteDuplex r (f a) (f b)
many (RouteDuplex enc dec) = RouteDuplex (foldMap enc) (Parser.many dec)

-- | Consumes or prints all the remaining segments.
-- |
-- |```purescript
-- | parse rest "" == Right []
-- | parse (path "a/b" rest) "a/b/c/d" == Right ["c", "d"]
-- |
-- | print rest ["a", "b"] == "a/b"
-- |```
rest :: forall r. RouteDuplex' r (Array String)
rest = RouteDuplex (foldMap Printer.put) Parser.rest

-- | Sets a default value which will be returned when parsing fails.
-- | Does not influence printing in any way.
-- |
-- |```purescript
-- | parse (default 0 $ int segment) "1" == Right 1
-- | parse (default 0 $ int segment) "x" == Right 0
-- |```
default :: forall a b r. b -> RouteDuplex r a b -> RouteDuplex r a b
default d (RouteDuplex enc dec) = RouteDuplex enc (Parser.default d dec)

-- | Augments the behavior of a given codec by making it return `Nothing` if parsing
-- | fails, or `Just value` if it succeeds.
-- |
-- |```purescript
-- | parse (optional segment) "a"        == Right (Just "a")
-- | parse (optional segment) ""         == Right Nothing
-- | 
-- | print (optional segment) (Just "a") == "a"
-- | print (optional segment) Nothing    == ""
-- |```
optional :: forall a b r. RouteDuplex r a b -> RouteDuplex r (Maybe a) (Maybe b)
optional (RouteDuplex enc dec) = RouteDuplex (foldMap enc) (Parser.optional dec)

-- | Builds a codec for a custom type out of printer and parser functions.
-- |
-- |```purescript
-- | data Sort = Asc | Desc
-- |
-- | sortToString :: Sort -> String
-- | sortToString = case _ of
-- |   Asc -> "asc"
-- |   Desc -> "desc"
-- |
-- | sortFromString :: String -> Either String Sort
-- | sortFromString = case _ of
-- |   "asc" -> Right Asc
-- |   "desc" -> Right Desc
-- |   val -> Left $ "Not a sort: " <> val
-- |
-- | sort :: RouteDuplex' String -> RouteDuplex' Sort
-- | sort = as sortToString sortFromString
-- |```
as :: forall s a b r. (a -> s) -> (String -> Either String b) -> RouteDuplex r s String -> RouteDuplex r a b
as f g (RouteDuplex enc dec) = RouteDuplex (enc <<< f) (Parser.as identity g dec)

-- | Refines a codec of Strings to Ints.
-- |
-- | ```purescript
-- | parse (int segment) "1"  == Right 1
-- | parse (int segment) "x"  == Left (Expected "Int" "x")
-- |
-- | print (int segment) 1    == "1"
-- | ```
int :: forall r. RouteDuplex' r String -> RouteDuplex' r Int
int = as show Parser.int

-- | Refines a codec of Strings to Booleans, where `true` and `false` are the
-- | strings `"true"` and `"false"`, and other strings are rejected.
-- |
-- | ```purescript
-- | parse (boolean segment) "true"  == Right true
-- | parse (boolean segment) "x"     == Left (Expected "Boolean" "x")
-- |
-- | print (boolean segment) true    == "true"
-- | ```
boolean :: forall r. RouteDuplex' r String -> RouteDuplex' r Boolean
boolean = as show Parser.boolean

-- | This does nothing (internally it's defined as identity).
-- | It can be used to restrict a type parameter of a polymorphic `RouteDuplex' a` to `String`.
string :: forall r. RouteDuplex' r String -> RouteDuplex' r String
string = identity

-- | Combined with `prop` or `:=`, builds a Record where the order of
-- | parsing and printing matters.
-- |
-- | ```purescript
-- | date =
-- |   record
-- |     # prop (SProxy :: _ "year") (int segment)
-- |     # prop (SProxy :: _ "month") (int segment)
-- |     # prop (SProxy :: _ "day") (int segment)
-- |
-- | parse (path "blog" date) "blog/2019/1/2" ==
-- |   Right { year: 2019, month: 1, day: 2 }
-- | ````
record :: forall r a. RouteDuplex r a {}
record = RouteDuplex mempty (pure {})

-- | See `record`.
prop ::
  forall sym a b r r1 r2 r3 rx.
  IsSymbol sym =>
  Row.Cons sym a rx r1 =>
  Row.Cons sym b r2 r3 =>
  Row.Lacks sym r2 =>
  SProxy sym ->
  RouteDuplex r a b ->
  RouteDuplex r { | r1 } { | r2 } ->
  RouteDuplex r { | r1 } { | r3 }
prop sym (RouteDuplex f g) (RouteDuplex x y) = RouteDuplex (\r -> x r <> f (Record.get sym r)) (flip (Record.insert sym) <$> y <*> g)

infix 2 prop as :=

class RouteDuplexParams (r1 :: # Type) (r2 :: # Type) (ctx :: # Type) | r1 -> r2 where
  -- | Builds a `RouteDuplex` from a record of query parameter parsers/printers, where
  -- | each property corresponds to a query parameter with the same name.
  -- |
  -- | ```purescript
  -- | search =
  -- |   params
  -- |     { page: int
  -- |     , filter: optional <<< string
  -- |     }
  -- |
  -- | parse search "?page=3&filter=Galaxy%20Quest" ==
  -- |   Right { page: 3, filter: Just "Galaxy Quest" }
  -- | ```
  params :: { | r1 } -> RouteDuplex' ctx { | r2 }

instance routeDuplexParams ::
  ( RowToList r1 rl
  , RouteDuplexBuildParams rl r1 r2 () r2 ctx
  ) =>
  RouteDuplexParams r1 r2 ctx where
  params r =
    record
      # buildParams (RLProxy :: RLProxy rl) r

class RouteDuplexBuildParams (rl :: RowList) (r1 :: # Type) (r2 :: # Type) (r3 :: # Type) (r4 :: # Type) ctx | rl -> r1 r2 r3 r4 where
  buildParams ::
    RLProxy rl ->
    { | r1 } ->
    RouteDuplex ctx { | r2 } { | r3 } ->
    RouteDuplex ctx { | r2 } { | r4 }

instance buildParamsCons ::
  ( IsSymbol sym
  , Row.Cons sym (RouteDuplex ctx String String -> RouteDuplex ctx a b) rx1 r1
  , Row.Cons sym a rx2 r2
  , Row.Cons sym b r3 rx3
  , Row.Lacks sym r3
  , RouteDuplexBuildParams rest r1 r2 rx3 r4 ctx
  ) =>
  RouteDuplexBuildParams (Cons sym (RouteDuplex ctx String String -> RouteDuplex ctx a b) rest) r1 r2 r3 r4 ctx where
  buildParams _ r prev =
    prev
      # prop sym ((Record.get sym r) (param (reflectSymbol sym)))
      # buildParams (RLProxy :: RLProxy rest) r
    where
    sym = SProxy :: SProxy sym

instance buildParamsNil ::
  RouteDuplexBuildParams Nil r1 r2 r3 r3 ctx where
  buildParams _ r = identity
