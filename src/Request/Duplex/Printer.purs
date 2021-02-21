module Request.Duplex.Printer
  ( RequestPrinter(..)
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
import Request.Duplex.Types (RouteState, UrlParts, Request, emptyRouteState)

newtype RequestPrinter = RequestPrinter (RouteState -> RouteState)

derive instance newtypeRequestPrinter :: Newtype RequestPrinter _

instance semigroupRequestPrinter :: Semigroup RequestPrinter where
  append (RequestPrinter f) (RequestPrinter g) = RequestPrinter (f >>> g)

instance monoidRoutePRinter :: Monoid RequestPrinter where
  mempty = RequestPrinter identity

put :: String -> RequestPrinter
put str = RequestPrinter \state -> state { url { segments = Array.snoc state.url.segments str }}

param :: String -> String -> RequestPrinter
param key val = RequestPrinter \state -> state { url { params = Array.cons (Tuple key val) state.url.params }}

flag :: String -> Boolean -> RequestPrinter
flag key val
  | val = param key ""
  | otherwise = mempty

hash :: String -> RequestPrinter
hash h = RequestPrinter _ { url { hash = h }}

run :: RequestPrinter -> Request
run = printRouteState <<< applyFlipped emptyRouteState <<< unwrap

printRouteState ∷ RouteState → Request
printRouteState { method, url } = { method, url: printPath url }

printPath :: UrlParts -> String
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
