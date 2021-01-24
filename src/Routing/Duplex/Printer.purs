module Routing.Duplex.Printer
  ( RoutePrinter(..)
  , put
  , param
  , flag
  , hash
  , run
  , printPath
  ) where

import Prelude

import Data.Array as Array
import Data.Function (applyFlipped)
import Data.Newtype (class Newtype, unwrap)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), uncurry)
import Global.Unsafe (unsafeEncodeURIComponent)
import Routing.Duplex.Types (RouteState, emptyRouteState)

newtype RoutePrinter r = RoutePrinter (RouteState r -> RouteState r)

derive instance newtypeRoutePrinter :: Newtype (RoutePrinter r) _

instance semigroupRoutePrinter :: Semigroup (RoutePrinter r) where
  append (RoutePrinter f) (RoutePrinter g) = RoutePrinter (f >>> g)

instance monoidRoutePRinter :: Monoid (RoutePrinter r) where
  mempty = RoutePrinter identity

put :: forall r. String -> RoutePrinter r
put str = RoutePrinter \state -> state { segments = Array.snoc state.segments str }

param :: forall r. String -> String -> RoutePrinter r
param key val = RoutePrinter \state -> state { params = Array.cons (Tuple key val) state.params }

flag :: forall r. String -> Boolean -> RoutePrinter r
flag key val
  | val = param key ""
  | otherwise = mempty

hash :: forall r. String -> RoutePrinter r
hash h = RoutePrinter _ { hash = h }

run :: RoutePrinter () -> String
run = printPath <<< applyFlipped emptyRouteState <<< unwrap

printPath :: RouteState () -> String
printPath { segments, params, hash: hash' } =
  printSegments segments <> printParams params <> printHash hash'
  where
  printSegments = case _ of
    [""] -> "/"
    xs -> joinWith "/" $ map unsafeEncodeURIComponent xs

  printParams [] = ""
  printParams ps = "?" <> joinWith "&" (uncurry printParam <$> ps)

  printParam key ""  = unsafeEncodeURIComponent key
  printParam key val = unsafeEncodeURIComponent key <> "=" <> unsafeEncodeURIComponent val

  printHash "" = ""
  printHash h  = "#" <> h
