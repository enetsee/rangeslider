module RangeSlider.Helpers exposing (mkPx, maybe)


mkPx : Float -> String
mkPx px =
    toString (100.0 * px) ++ "%"


maybe : a -> (b -> a) -> Maybe b -> a
maybe whenNothing withJust maybeVal =
    case maybeVal of
        Just val ->
            withJust val

        _ ->
            whenNothing
