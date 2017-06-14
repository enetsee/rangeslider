module Interpolate exposing (linear, roundTo)


linear : number -> number -> number -> number
linear a b =
    if a == b then
        \_ -> a
    else
        let
            delta =
                b - a
        in
            \t ->
                a + delta * t


roundTo : Float -> (a -> Float) -> a -> Float
roundTo step interpolate =
    (\c -> step * (toFloat << round <| c / step))
        << interpolate
