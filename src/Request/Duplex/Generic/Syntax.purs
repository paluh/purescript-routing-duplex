module Request.Duplex.Generic.Syntax where

import Prelude

import Data.Generic.Rep (Argument, Product)
import Request.Duplex (class RequestDuplexParams, RequestDuplex, RequestDuplex', params, prefix, suffix)
import Request.Duplex.Generic (class GRequestDuplexCtr, gRequestDuplexCtr, product)

class GSep a b c | a b -> c where
  gsep :: a -> b -> RequestDuplex' c

instance gsepStringString ::
  GSep String String Unit where
  gsep a b = prefix a $ prefix b $ pure unit
else
instance gsepStringRoute ::
  GRequestDuplexCtr a b =>
  GSep String (RequestDuplex a a) b where
  gsep a = prefix a <<< gRequestDuplexCtr
else
instance gsepRouteString ::
  GRequestDuplexCtr a b =>
  GSep (RequestDuplex a a) String b where
  gsep = suffix <<< gRequestDuplexCtr
else
instance gsepProduct ::
  GRequestDuplexCtr b c =>
  GSep (RequestDuplex a a) (RequestDuplex b b) (Product (Argument a) c) where
  gsep = product

infixr 1 gsep as /

class GParams a b c | a b -> c where
  gparams :: a -> b -> RequestDuplex' c

instance gparamsString ::
  RequestDuplexParams r1 r2 =>
  GParams String { | r1 } { | r2 } where
  gparams a = prefix a <<< params
else
instance gparamsRoute ::
  RequestDuplexParams r1 r2 =>
  GParams (RequestDuplex a a) { | r1 } (Product (Argument a) (Argument { | r2 })) where
  gparams a = product a <<< params

infix 8 gparams as ?
