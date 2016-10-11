module PFD exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

--pfd : Svg msg
pfd pitchRads rollRads =
  svg
    [ viewBox "0 0 100 100" ]
    [ rect [ x "0", y "0", width "100", height "100", fill "rgb(76,76,76)" ] []
    , artificialHorizon pitchRads rollRads
    ]

artificialHorizon pitchRads rollRads =
  let skyColor = "#72cde4"
      groundColor = "#c0a020"
      ground = Svg.path [ d "M-150,0 L-150,150 L150,150 L150,0 Z", fill groundColor ] []
      sky = Svg.path [ d "M-150,0 L-150,-150 L150,-150 L150,0 Z", fill skyColor ] []
      horizon = line [ x1 "-150", y1 "0", x2 "150", y2 "0", strokeWidth "2.5", stroke "#343434" ] []
      ladder h w =
        let translate = "translate(" ++ toString -(w / 2) ++ " " ++ toString -h ++ ")"
        in line [ x1 "0", y1 "0", x2 (toString w), y2 "0"
                , transform translate, stroke "#202020" ] []
      rungs = [ ladder 7 12, ladder 14 17, ladder 21 12, ladder 28 17
              , ladder -7 12, ladder -14 17, ladder -21 12, ladder -28 17
              ]
      movingGroup =
        let translate = "translate(0 " ++ toString (pitchRads * (180 / pi) / 2) ++ ")"
            rotate    = "rotate(" ++ toString (rollRads * (180 / pi)) ++ ")"
        in g [ transform (translate ++ " " ++ rotate) ]
             (List.append [ ground, sky, horizon ] rungs)
      indicator_left = Svg.path [
          d "M-25,0 L-13,0 L-7,5 M-25,0 Z"
        , strokeWidth "2.5", stroke "black", fill "none" ] []
      indicator_right = Svg.path [
          d "M25,0 L13,0 L7,5 M25,0 Z"
        , strokeWidth "2.5", stroke "black", fill "none" ] []
        
  in g [ transform "translate(50 50)" ] [ movingGroup, indicator_left, indicator_right ]
