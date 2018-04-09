module RangeSlider.Style exposing (Config, default)

import Css exposing (..)


{-| -}
type alias Config =
    { rangeSlider : List Style
    , track : List Style
    , progress : List Style
    , handleLower : List Style
    , handleUpper : List Style
    , labelLower : List Style
    , labelUpper : List Style
    , tick : List Style
    }


{-| -}
default : Config
default =
    Config
        defaultRangeSlider
        defaultTrack
        defaultProgress
        defaultHandleLeft
        defaultHandleRight
        defaultLabelLower
        defaultLabelUpper
        defaultTick


defaultRangeSlider : List Style
defaultRangeSlider =
    [ position relative
    , cursor Css.default
    , width (pct 100)
    , height (px 40)
    , outline none
    , batch userSelect
    , boxSizing borderBox
    , marginLeft (px 0)
    ]


userSelect : List Style
userSelect =
    [ property "user-select" "none"
    , property "-webkit-user-select" "none"
    , property "-moz-user-select" "none"
    , property "-ms-user-select" "none"
    ]


trackBackground : Color
trackBackground =
    rgba 92 112 128 0.2


topPos : Px
topPos =
    (px 5)


trackHeight : Px
trackHeight =
    (px 6)


defaultTrack : List Style
defaultTrack =
    [ position absolute
    , top topPos
    , right zero
    , left zero
    , borderRadius (px 3)
    , backgroundColor trackBackground
    , height trackHeight
    , boxSizing borderBox
    ]


progressColor : Color
progressColor =
    hex "#137cbd"


defaultProgress : List Style
defaultProgress =
    [ position absolute, top topPos, right zero, left zero, borderRadius zero, backgroundColor progressColor, height trackHeight, boxSizing borderBox ]


handleColor : Color
handleColor =
    hex "#f5f8fa"


handleTextColor : Color
handleTextColor =
    hex "182026"


handleGradient1 : Color
handleGradient1 =
    rgba 255 255 255 0.8


handleGradient2 : Color
handleGradient2 =
    rgba 255 255 255 0.0


handleSize : number
handleSize =
    16


handleBase : List Style
handleBase =
    [ property "box-shadow" "inset 0 0 0 1px rgba(16, 22, 26, 0.2), inset 0 -1px 0 rgba(16, 22, 26, 0.1)"
    , backgroundColor handleColor
    , backgroundImage (linearGradient2 toBottom (stop handleGradient1) (stop handleGradient2) [])
    , color handleTextColor
    , cursor pointer
    , position absolute
    , top zero
    , left zero
    , borderRadius (px 3)
    , width (px handleSize)
    , height (px handleSize)
    , zIndex (int 50)
    , outline inherit
    , focus [ zIndex (int 100) ]
    , boxSizing borderBox
    ]


defaultHandleLeft : List Style
defaultHandleLeft =
    [ batch handleBase
    , borderTopRightRadius zero
    , borderBottomRightRadius zero
    , width (px (handleSize / 2))
    , marginLeft (px (-handleSize / 2))
    ]


defaultHandleRight : List Style
defaultHandleRight =
    [ batch handleBase
    , borderTopLeftRadius zero
    , borderBottomLeftRadius zero
    , width (px (handleSize / 2))
    ]


labelBase : List Style
labelBase =
    [ display inlineBlock
    , padding2 (px 2) (px 5)
    , verticalAlign top
    , lineHeight (em 1)
    , fontSize (px 12)
    , position absolute
    , transform (translate2 (pct -50) (px 24))
    , textAlign center
    , borderRadius (px 3)
    , backgroundColor (hex "#394b59")
    , color (hex "#f5f8fa")
    , property "box-shadow" "0 0 0 1px rgba(16, 22, 26, 0.1), 0 2px 4px rgba(16, 22, 26, 0.2), 0 8px 24px rgba(16, 22, 26, 0.2)"
    , boxSizing borderBox
    ]


defaultLabelLower : List Style
defaultLabelLower =
    [ batch labelBase, marginLeft (px 8) ]


defaultLabelUpper : List Style
defaultLabelUpper =
    labelBase


defaultTick : List Style
defaultTick =
    [ display inlineBlock
    , boxSizing borderBox
    , padding2 (px 2) (px 5)
    , verticalAlign top
    , lineHeight (em 1)
    , fontSize (px 12)
    , position absolute
    , transform (translate2 (pct -50) (px 24))
    , textAlign center
    , before [ property "content" "'|'", position absolute, left (pct 50), top (pct -50), fontSize (em 0.4) ]
    ]
