module Bbox exposing (..)

import String

import Svg exposing (..)
import Svg.Attributes exposing (..)

bbox target =
  svg
    [ viewBox "0 0 100 100" ]
    [ rect [ x "0", y "0", width "100", height "100", fill "#EFEFEF" ] []
    , renderBox target
    ]

renderBox target =
  if target.valid
    then rect [ x (toString target.bbox_l)
              , y (toString target.bbox_t)
              , width (toString (target.bbox_r - target.bbox_l))
              , height (toString (target.bbox_b - target.bbox_t))
              ] []
    else text_ [ x "50", y "50", textAnchor "middle", fontSize "10px" ] [ text "No Target Data" ]
