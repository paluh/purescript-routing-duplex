module Request.Duplex.Generic where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), from, to)
import Data.Profunctor (dimap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Record as Record
import Request.Duplex (RequestDuplex(..), RequestDuplex', end)

sum :: forall a rep r.
  Generic a rep =>
  GRequestDuplex rep r =>
  { | r } ->
  RequestDuplex' a
sum = dimap from to <<< gRequestDuplex

class GRequestDuplex rep (r :: # Type) | rep -> r where
  gRequestDuplex :: { | r } -> RequestDuplex' rep

instance gRouteSum ::
  ( GRequestDuplex a r
  , GRequestDuplex b r
  ) =>
  GRequestDuplex (Sum a b) r where
  gRequestDuplex r = RequestDuplex enc dec
    where
    RequestDuplex encl decl = gRequestDuplex r
    RequestDuplex encr decr = gRequestDuplex r
    enc = case _ of
      Inl a -> encl a
      Inr b -> encr b
    dec = Inl <$> decl <|> Inr <$> decr

instance gRouteConstructor ::
  ( IsSymbol sym
  , Row.Cons sym (RequestDuplex a a) rx r
  , GRequestDuplexCtr a b
  ) =>
  GRequestDuplex (Constructor sym b) r where
  gRequestDuplex r = RequestDuplex enc dec
    where
    RequestDuplex enc' dec' =
      end
        $ (gRequestDuplexCtr :: RequestDuplex' a -> RequestDuplex' b)
        $ Record.get (SProxy :: SProxy sym) r
    enc (Constructor a) = enc' a
    dec = Constructor <$> dec'

class GRequestDuplexCtr a b | a -> b where
  gRequestDuplexCtr :: RequestDuplex' a -> RequestDuplex' b

instance gRouteProduct ::
  GRequestDuplexCtr (Product a b) (Product a b) where
  gRequestDuplexCtr = identity
else
instance gRouteNoArguments ::
  GRequestDuplexCtr NoArguments NoArguments where
  gRequestDuplexCtr = identity
else
instance gRouteArgument ::
  GRequestDuplexCtr (Argument a) (Argument a) where
  gRequestDuplexCtr = identity
else
instance gRouteAll ::
  GRequestDuplexCtr a (Argument a) where
  gRequestDuplexCtr (RequestDuplex enc dec) =
    RequestDuplex (\(Argument a) -> enc a) (Argument <$> dec)

product :: forall a b c.
  GRequestDuplexCtr b c =>
  RequestDuplex' a ->
  RequestDuplex' b ->
  RequestDuplex' (Product (Argument a) c)
product (RequestDuplex encl decl) l = RequestDuplex enc dec
  where
  RequestDuplex encr decr = gRequestDuplexCtr l
  enc (Product (Argument a) b) = encl a <> encr b
  dec = Product <$> (Argument <$> decl) <*> decr

noArgs:: RequestDuplex' NoArguments
noArgs = pure NoArguments

infixr 0 product as ~
