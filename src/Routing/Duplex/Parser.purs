module Routing.Duplex.Parser
  ( RouteError(..)
  , RouteResult(..)
  , RouteParser(..)
  , runRouteParser
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
  , module Routing.Duplex.Types
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
import Routing.Duplex.Types (RouteParams, RouteState)

data RouteResult r a
  = Fail RouteError
  | Success (RouteState r) a

derive instance eqRouteResult :: (Eq (RouteState r), Eq a) => Eq (RouteResult r a)
derive instance functorRouteResult :: Functor (RouteResult r)
derive instance genericRouteResult :: Generic (RouteResult r a) _
instance showRouteResult :: (Show (RouteState r), Show a) => Show (RouteResult r a) where show = genericShow

data RouteError
  = Expected String String
  | ExpectedEndOfPath String
  | MissingParam String
  | EndOfPath

derive instance eqRouteError :: Eq RouteError
derive instance genericRouteError :: Generic RouteError _
instance showRouteError :: Show RouteError where show = genericShow

data RouteParser r a
  = Alt (NonEmptyArray (RouteParser r a))
  | Chomp (RouteState r -> RouteResult r a)
  | Prefix String (RouteParser r a)

derive instance functorRouteParser :: Functor (RouteParser r)

instance applyRouteParser :: Apply (RouteParser r) where
  apply fx x = Chomp \state ->
    case runRouteParser state fx of
      Fail err -> Fail err
      Success state' f -> f <$> runRouteParser state' x

instance applicativeRouteParser :: Applicative (RouteParser r) where
  pure = Chomp <<< flip Success

instance altRouteParser :: Alt (RouteParser r) where
  alt (Alt ls) (Alt rs) = Alt (ls `altAppend` rs)
  alt (Alt ls) b = Alt (ls `altSnoc` b)
  alt a (Alt rs) = Alt (a `altCons` rs)
  alt (Prefix pre a) (Prefix pre' b)
    | pre == pre' = Prefix pre (a <|> b)
  alt a b = Alt (NEA.cons a (NEA.singleton b))

instance lazyRouteParser :: Lazy (RouteParser r a) where
  defer k =
    Chomp \state ->
      runRouteParser state (Z.force parser)
    where
    parser = Z.defer k

altAppend :: forall a r.
  NonEmptyArray (RouteParser r a) ->
  NonEmptyArray (RouteParser r a) ->
  NonEmptyArray (RouteParser r a)
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

altCons :: forall a r.
  RouteParser r a ->
  NonEmptyArray (RouteParser r a) ->
  NonEmptyArray (RouteParser r a)
altCons (Prefix pre a) rs
  | Prefix pre' b <- NEA.head rs
  , pre == pre' =
      NEA.cons' (Prefix pre (a <|> b)) (NEA.tail rs)
altCons a rs = NEA.cons a rs

altSnoc :: forall a r.
  NonEmptyArray (RouteParser r a) ->
  RouteParser r a ->
  NonEmptyArray (RouteParser r a)
altSnoc ls (Prefix pre b)
  | Prefix pre' a <- NEA.last ls
  , pre == pre' =
      NEA.snoc' (NEA.init ls) (Prefix pre (a <|> b))
altSnoc ls b = NEA.snoc ls b

chompPrefix :: forall r. String -> RouteState r -> RouteResult r Unit
chompPrefix pre state =
  case Array.head state.segments of
    Just pre' | pre == pre' -> Success (state { segments = Array.drop 1 state.segments }) unit
    Just pre' -> Fail $ Expected pre pre'
    _ -> Fail $ EndOfPath

runRouteParser :: forall a r. RouteState r -> RouteParser r a -> RouteResult r a
runRouteParser = go
  where
  go state = case _ of
    Alt xs -> foldl (goAlt state) (Fail EndOfPath) xs
    Chomp f -> f state
    Prefix pre p ->
      case chompPrefix pre state of
        Fail err -> Fail err
        Success state' _ -> go state' p

  goAlt state (Fail _) p = runRouteParser state p
  goAlt _ res _ = res

parsePath :: String -> RouteState ()
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

run :: forall a. RouteParser () a -> String -> Either RouteError a
run p = parsePath >>> flip runRouteParser p >>> case _ of
  Fail err -> Left err
  Success _ res -> Right res

prefix :: forall a r. String -> RouteParser r a -> RouteParser r a
prefix = Prefix

take :: forall r. RouteParser r String
take = Chomp \state ->
  case Array.uncons state.segments of
    Just { head, tail } -> Success (state { segments = tail }) head
    _ -> Fail EndOfPath

param :: forall r. String -> RouteParser r String
param key = Chomp \state ->
  case Tuple.lookup key state.params of
    Just a -> Success state a
    _ -> Fail $ MissingParam key

flag :: forall r. String -> RouteParser r Boolean
flag = default false <<< map (const true) <<< param

many1 :: forall r t a. Alt t => Applicative t => RouteParser r a -> RouteParser r (t a)
many1 p = Chomp go
  where
  go :: RouteState r -> RouteResult r (t a)
  go state =
    case runRouteParser state p of
      Fail err -> Fail err
      Success state' a -> go' state' (pure a)

  go' :: RouteState r -> t a -> RouteResult r (t a)
  go' state xs =
    case runRouteParser state p of
      Fail err -> Success state xs
      Success state' a -> go' state' (xs <|> pure a)

many :: forall r t a. Alternative t => RouteParser r a -> RouteParser r (t a)
many p = many1 p <|> pure empty

rest :: forall r. RouteParser r (Array String)
rest = Chomp \state -> Success (state { segments = [] }) state.segments

default :: forall a r. a -> RouteParser r a -> RouteParser r a
default = flip (<|>) <<< pure

optional :: forall a r. RouteParser r a -> RouteParser r (Maybe a)
optional = default Nothing <<< map Just

as :: forall a b r. (a -> String) -> (a -> Either String b) -> RouteParser r a -> RouteParser r b
as print decode p = Chomp \state ->
  case runRouteParser state p of
    Fail err -> Fail err
    Success state' a ->
      case decode a of
        Left err -> Fail $ Expected err (print a)
        Right b -> Success state' b

int :: String -> Either String Int
int = maybe (Left "Int") Right <<< Int.fromString

hash :: forall r. RouteParser r String
hash = Chomp \state -> Success state state.hash

end :: forall r. RouteParser r Unit
end = Chomp \state ->
  case Array.head state.segments of
    Nothing -> Success state unit
    Just str -> Fail (ExpectedEndOfPath str)

boolean :: String -> Either String Boolean
boolean = case _ of
  "true" -> Right true
  "false" -> Right false
  _ -> Left "Boolean"
