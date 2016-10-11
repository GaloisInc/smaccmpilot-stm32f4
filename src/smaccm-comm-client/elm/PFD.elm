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
      ground = Svg.path [ d "M-100,50 L-100,200 L200,200 L200,50 Z", fill groundColor ] []
      sky = Svg.path [ d "M-100,50 L-100,-100 L200,-100 L200,50 Z", fill skyColor ] []
      horizon = line [ x1 "-100", y1 "50", x2 "200", y2 "50", strokeWidth "2.5", stroke "#343434" ] []
      ladder h w =
        let translate = "translate(" ++ toString (50 - (w / 2)) ++ " " ++ toString (50 - h) ++ ")"
        in line [ x1 "0", y1 "0", x2 (toString w), y2 "0"
                , transform translate, stroke "#202020" ] []
      rungs = [ ladder 7 12, ladder 15 17, ladder 22 12, ladder 30 17
              , ladder -7 12, ladder -15 17, ladder -22 12, ladder -30 17
              ]
      movingGroup =
        let translate = "translate(0 " ++ toString (pitchRads * (180 / pi) / 2) ++ ")"
            rotate    = "rotate(" ++ toString (rollRads * (180 / pi)) ++ " 50 50)"
        in g [ transform (translate ++ " " ++ rotate) ]
             (List.append [ ground, sky, horizon ] rungs)
      indicator_left = Svg.path [
          d "M25,50 L37,50 L43,55 M25,50 Z"
        , strokeWidth "2.5", stroke "black", fill "none" ] []
      indicator_right = Svg.path [
          d "M75,50 L63,50 L57,55 M75,50 Z"
        , strokeWidth "2.5", stroke "black", fill "none" ] []
        
  in g [] [ movingGroup, indicator_left, indicator_right ]
