module Routing.Duplex.Types where

import Data.Tuple (Tuple)

type RouteParams = Array (Tuple String String)

type RouteState r =
  { segments :: Array String
  , params :: RouteParams
  , hash :: String
  | r
  }

emptyRouteState :: RouteState ()
emptyRouteState =
  { segments: []
  , params: []
  , hash: ""
  }
