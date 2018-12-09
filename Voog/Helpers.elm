module Voog.Helpers exposing (reverseId, sign)


reverseId : ( Int, Int ) -> ( Int, Int )
reverseId id =
    ( Tuple.second id, Tuple.first id )


sign : Float -> Float
sign a =
    if a < 0 then
        -1

    else if a > 0 then
        1

    else
        0
