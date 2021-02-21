module Request.Duplex.Parser
  ( RouteError(..)
  , RouteResult(..)
  , RequestParser(..)
  , runRequestParser
  , parsePath
  , run
  , prefix
  , take
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
  , hash
  , end
  , module Request.Duplex.Types
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, empty)
import Control.Lazy (class Lazy)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Lazy as Z
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Global.Unsafe (unsafeDecodeURIComponent)
import Request.Duplex.Types (RouteParams, RouteState, UrlParts, Request)

data RouteResult a
  = Fail RouteError
  | Success RouteState a

derive instance eqRouteResult :: Eq a => Eq (RouteResult a)
derive instance functorRouteResult :: Functor RouteResult
derive instance genericRouteResult :: Generic (RouteResult a) _
instance showRouteResult :: Show a => Show (RouteResult a) where show = genericShow

data RouteError
  = Expected String String
  | ExpectedEndOfPath String
  | MissingParam String
  | EndOfPath

derive instance eqRouteError :: Eq RouteError
derive instance genericRouteError :: Generic RouteError _
instance showRouteError :: Show RouteError where show = genericShow

data RequestParser a
  = Alt (NonEmptyArray (RequestParser a))
  | Chomp (RouteState -> RouteResult a)
  | Prefix String (RequestParser a)

derive instance functorRequestParser :: Functor RequestParser

instance applyRequestParser :: Apply RequestParser where
  apply fx x = Chomp \state ->
    case runRequestParser state fx of
      Fail err -> Fail err
      Success state' f -> f <$> runRequestParser state' x

instance applicativeRequestParser :: Applicative RequestParser where
  pure = Chomp <<< flip Success

instance altRequestParser :: Alt RequestParser where
  alt (Alt ls) (Alt rs) = Alt (ls `altAppend` rs)
  alt (Alt ls) b = Alt (ls `altSnoc` b)
  alt a (Alt rs) = Alt (a `altCons` rs)
  alt (Prefix pre a) (Prefix pre' b)
    | pre == pre' = Prefix pre (a <|> b)
  alt a b = Alt (NEA.cons a (NEA.singleton b))

instance lazyRequestParser :: Lazy (RequestParser a) where
  defer k =
    Chomp \state ->
      runRequestParser state (Z.force parser)
    where
    parser = Z.defer k

altAppend :: forall a.
  NonEmptyArray (RequestParser a) ->
  NonEmptyArray (RequestParser a) ->
  NonEmptyArray (RequestParser a)
altAppend ls rs
  | Prefix pre a <- NEA.last ls
  , Prefix pre' b <- NEA.head rs
  , pre == pre' =
      let
        rs' =
          NEA.cons' (Prefix pre (a <|> b)) (NEA.tail rs)
      in
        case NEA.fromArray (NEA.init ls) of
          Just ls' -> ls' `altAppend` rs'
          Nothing  -> rs'
  | otherwise = ls <> rs

altCons :: forall a.
  RequestParser a ->
  NonEmptyArray (RequestParser a) ->
  NonEmptyArray (RequestParser a)
altCons (Prefix pre a) rs
  | Prefix pre' b <- NEA.head rs
  , pre == pre' =
      NEA.cons' (Prefix pre (a <|> b)) (NEA.tail rs)
altCons a rs = NEA.cons a rs

altSnoc :: forall a.
  NonEmptyArray (RequestParser a) ->
  RequestParser a ->
  NonEmptyArray (RequestParser a)
altSnoc ls (Prefix pre b)
  | Prefix pre' a <- NEA.last ls
  , pre == pre' =
      NEA.snoc' (NEA.init ls) (Prefix pre (a <|> b))
altSnoc ls b = NEA.snoc ls b

chompPrefix :: String -> RouteState -> RouteResult Unit
chompPrefix pre state =
  case Array.head state.url.segments of
    Just pre' | pre == pre' -> Success (state { url { segments = Array.drop 1 state.url.segments }}) unit
    Just pre' -> Fail $ Expected pre pre'
    _ -> Fail $ EndOfPath

runRequestParser :: forall a. RouteState -> RequestParser a -> RouteResult a
runRequestParser = go
  where
  go state = case _ of
    Alt xs -> foldl (goAlt state) (Fail EndOfPath) xs
    Chomp f -> f state
    Prefix pre p ->
      case chompPrefix pre state of
        Fail err -> Fail err
        Success state' _ -> go state' p

  goAlt state (Fail _) p = runRequestParser state p
  goAlt _ res _ = res

parsePath :: String -> UrlParts
parsePath =
  splitAt (flip Tuple "") "#"
    >>> lmap splitPath
    >>> toRouteState
  where
  splitPath =
    splitAt (flip Tuple "") "?"
      >>> bimap splitSegments splitParams

  splitSegments = splitNonEmpty (Pattern "/") >>> case _ of
    ["", ""] -> [""]
    xs -> map unsafeDecodeURIComponent xs

  splitParams =
    splitNonEmpty (Pattern "&") >>> map splitKeyValue

  splitKeyValue =
    splitAt (flip Tuple "") "=" >>> bimap unsafeDecodeURIComponent unsafeDecodeURIComponent

  splitNonEmpty _ "" = []
  splitNonEmpty p s  = split p s

  toRouteState (Tuple (Tuple segments params) h) =
    { segments, params, hash: h }

  splitAt k p str =
    case String.indexOf (Pattern p) str of
      Just ix -> Tuple (String.take ix str) (String.drop (ix + String.length p) str)
      Nothing -> k str

run :: forall a. RequestParser a -> Request -> Either RouteError a
run p = parsePath' >>> flip runRequestParser p >>> case _ of
  Fail err -> Left err
  Success _ res -> Right res

  where
    parsePath' { method, url } = { method, url: _ } $ parsePath url

prefix :: forall a. String -> RequestParser a -> RequestParser a
prefix = Prefix

take :: RequestParser String
take = Chomp \state ->
  case Array.uncons state.url.segments of
    Just { head, tail } -> Success (state { url { segments = tail }}) head
    _ -> Fail EndOfPath

param :: String -> RequestParser String
param key = Chomp \state ->
  case Tuple.lookup key state.url.params of
    Just a -> Success state a
    _ -> Fail $ MissingParam key

flag :: String -> RequestParser Boolean
flag = default false <<< map (const true) <<< param

many1 :: forall t a. Alt t => Applicative t => RequestParser a -> RequestParser (t a)
many1 p = Chomp go
  where
  go :: RouteState -> RouteResult (t a)
  go state =
    case runRequestParser state p of
      Fail err -> Fail err
      Success state' a -> go' state' (pure a)

  go' :: RouteState -> t a -> RouteResult (t a)
  go' state xs =
    case runRequestParser state p of
      Fail err -> Success state xs
      Success state' a -> go' state' (xs <|> pure a)

many :: forall t a. Alternative t => RequestParser a -> RequestParser (t a)
many p = many1 p <|> pure empty

rest :: RequestParser (Array String)
rest = Chomp \state -> Success (state { url { segments = [] }}) state.url.segments

default :: forall a. a -> RequestParser a -> RequestParser a
default = flip (<|>) <<< pure

optional :: forall a. RequestParser a -> RequestParser (Maybe a)
optional = default Nothing <<< map Just

as :: forall a b. (a -> String) -> (a -> Either String b) -> RequestParser a -> RequestParser b
as print decode p = Chomp \state ->
  case runRequestParser state p of
    Fail err -> Fail err
    Success state' a ->
      case decode a of
        Left err -> Fail $ Expected err (print a)
        Right b -> Success state' b

int :: String -> Either String Int
int = maybe (Left "Int") Right <<< Int.fromString

hash :: RequestParser String
hash = Chomp \state -> Success state state.url.hash

end :: RequestParser Unit
end = Chomp \state ->
  case Array.head state.url.segments of
    Nothing -> Success state unit
    Just str -> Fail (ExpectedEndOfPath str)

boolean :: String -> Either String Boolean
boolean = case _ of
  "true" -> Right true
  "false" -> Right false
  _ -> Left "Boolean"
