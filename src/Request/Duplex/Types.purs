module Request.Duplex.Types where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Tuple (Tuple)

data Method
  = Get
  | Post

derive instance eqMethod ∷ Eq Method

derive instance ordMethod ∷ Ord Method

derive instance genericMethod ∷ Generic Method _

instance showMethod ∷ Show Method where
  show = genericShow

type RouteParams
  = Array (Tuple String String)

type UrlParts
  = { segments :: Array String
    , params :: RouteParams
    , hash :: String
    }

type RouteState
  = { method :: Method
    , url :: UrlParts
    }

emptyUrlParts :: UrlParts
emptyUrlParts =
  { segments: []
  , params: []
  , hash: ""
  }

emptyRouteState :: RouteState
emptyRouteState =
  { method: Get
  , url: emptyUrlParts
  }

type Request
  = { method :: Method, url ∷ String }
