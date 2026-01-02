{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Nacre.Group where

import GHC.Generics
import Nacre.Function
import Nacre.Request (Request)
import Prelude hiding ((>>))

(>>) :: a -> b -> Group a b
(>>) = (:<>)

data Group a b = a :<> b

class Zip c h s | c h -> s where
    (=:=) :: c -> h -> s

infixr 3 =:=

instance Zip (Contract (Request m p q b h --> o)) (Handler ctx (Request m p q b h --> o)) (Server ctx (Request m p q b h --> o)) where
    contract =:= (Handler h) = contract := h

instance (Zip c1 h1 s1, Zip c2 h2 s2) => Zip (Group c1 c2) (Group h1 h2) (Group s1 s2) where
    (c1 :<> c2) =:= (h1 :<> h2) = (c1 =:= h1) :<> (c2 =:= h2)

class GZip c h s where
    gzip :: c x -> h x -> s x

instance (GZip c h s) => GZip (M1 i meta c) (M1 i meta h) (M1 i meta s) where
    gzip (M1 c) (M1 h) = M1 (gzip c h)

instance (GZip c1 h1 s1, GZip c2 h2 s2) => GZip (c1 :*: c2) (h1 :*: h2) (s1 :*: s2) where
    gzip (c1 :*: c2) (h1 :*: h2) = gzip c1 h1 :*: gzip c2 h2

instance (Zip c h s) => GZip (K1 R c) (K1 R h) (K1 R s) where
    gzip (K1 c) (K1 h) = K1 (c =:= h)

zipRecords ::
    (Generic c, Generic h, Generic s, GZip (Rep c) (Rep h) (Rep s)) =>
    c ->
    h ->
    s
zipRecords c h = to (gzip (from c) (from h))
