module Nacre.Responses where

data Responses i o where
    OneResponse ::
        Response IsoContract s h b ->
        Responses (Response Value s h b) (Response Value s h b)
    NestedResponse ::
        IsoContract Responses (r Value) ->
        Responses (r Value) (r Value)
    AltResponse ::
        Responses i1 o1 ->
        Responses i2 o2 ->
        Responses (Either i1 i2) (Either o1 o2)

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
    '[] ++ ys = ys
    (x : xs) ++ ys = x : (xs ++ ys)

type family GCode (f :: Type -> Type) :: [[Type]] where
    GCode (D1 _ f) = GCode f
    GCode (f :+: g) = GCode f ++ GCode g
    GCode (C1 _ f) = '[GFields f]
    GCode V1 = '[]

type family GFields (f :: Type -> Type) :: [Type] where
    GFields (f :*: g) = GFields f ++ GFields g
    GFields (S1 _ f) = '[GField f]
    GFields U1 = '[]

type family GField (f :: Type -> Type) :: Type where
    GField (Rec0 a) = a

class ResponseContractC (xs :: [Type]) where
    type ResponseContractT xs :: Type

instance {-# OVERLAPPING #-} ResponseContractC '[Response Value s h b] where
    type ResponseContractT '[Response Value s h b] = Response IsoContract s h b

instance (GenericResponses r) => ResponseContractC '[r Value] where
    type ResponseContractT '[r Value] = IsoContract Responses (r Value)

type family ResponseContracts (xss :: [[Type]]) :: [Type] where
    ResponseContracts '[] = '[]
    ResponseContracts (xs : xss) = ResponseContractT xs : ResponseContracts xss

type family ResponseChains (args :: [Type]) (result :: Type) :: Type where
    ResponseChains '[] result = result
    ResponseChains (a : as) result = a -> ResponseChains as result

class
    (Generic (r Value)) =>
    GenericResponses (r :: ((Type -> Type -> Type) -> Type -> Type) -> Type)
    where
    responses ::
        ResponseChains
            (ResponseContracts (GCode (Rep (r Value))))
            (IsoContract Responses (r Value))
    responses = undefined
