module PFD exposing (..)

import String

import Svg exposing (..)
import Svg.Attributes exposing (..)

--pfd : Svg msg
pfd pitchRads rollRads baroAlt yawRads voltage =
  svg
    [ viewBox "0 0 100 100" ]
    [ rect [ x "0", y "0", width "100", height "100", fill "rgb(76,76,76)" ] []
    , artificialHorizon pitchRads rollRads baroAlt yawRads voltage
    ]

-- we linearize pitch in the display; this is the factor we
-- divide degrees by to get y coordinate changes
pitchScale = 1.2

artificialHorizon pitchRads rollRads baroAlt yawRads voltage =
  let pitchDeg = pitchRads * 180 / pi
      rollDeg = rollRads * 180 / pi
      yawDeg = if yawRads < 0 then 360 + (yawRads * (180 / pi)) else yawRads * (180 / pi)
      skyColor = "#72cde4"
      groundColor = "#c0a020"
      ground = Svg.path [ d "M-150,0 L-150,150 L150,150 L150,0 Z", fill groundColor ] []
      sky = Svg.path [ d "M-150,0 L-150,-150 L150,-150 L150,0 Z", fill skyColor ] []
      horizon = line [ x1 "-150", y1 "0", x2 "150", y2 "0", strokeWidth "2.5", stroke "#545454" ] []
      movingGroup =
        let translate = "translate(0 " ++ toString (pitchDeg / pitchScale) ++ ")"
            rotate    = "rotate(" ++ toString -rollDeg ++ ")"
        in g [ transform (rotate ++ " " ++ translate) ]
             [ ground, sky, horizon ]
      indicator = g [ stroke "black", strokeWidth "1" ] [
          line [ x1 "-7", y1 "0", x2 "-2", y2 "0" ] []
        , line [ x1 "7", y1 "0", x2 "2", y2 "0" ] []
        , circle [ cx "0", cy "0", r "2", fill "none" ] []
        ]
      indicator_left = Svg.path [
          d "M-25,0 L-13,0 L-7,5 M-25,0 Z"
        , strokeWidth "2.5", stroke "black", fill "none" ] []
      indicator_right = Svg.path [
          d "M25,0 L13,0 L7,5 M25,0 Z"
        , strokeWidth "2.5", stroke "black", fill "none" ] []
      pitchLadder =
        let rt = "rotate(" ++ (toString -rollDeg) ++ ")"
            tr = "translate(0 " ++ toString (pitchDeg / pitchScale) ++ ")"
        in g [ Svg.Attributes.clipPath "url(#pitchClip)" ] [
           defs [] [ Svg.clipPath [ id "pitchClip" ] [ rect [ x "-35", y "-45", width "60", height "80" ] [] ] ]
         , g [ transform (rt ++ " " ++ tr) ] mkPitchLadder
         ]
      headingIndicator =
        g [ Svg.Attributes.clipPath "url(#headingClip)" ] [
            defs [] [ Svg.clipPath [ id "headingClip" ] [ rect [ x "-40", y "35", width "80", height "15" ] [] ] ]
          , g [] (mkHeadingIndicator yawDeg)
          ]
      altIndicator =
        g [ Svg.Attributes.clipPath "url(#altClip)" ] [
            defs [] [ Svg.clipPath [ id "altClip" ] [ rect [ x "25", y "-45", width "25", height "75" ] [] ] ]
          , g [] (mkAltLadder baroAlt)
          ]

      voltsStr = toString voltage
      voltsFmtd = case String.indices "." (String.reverse voltsStr) of
                  [] -> voltsStr ++ ".0v"
                  [1] -> voltsStr ++ "0v"
                  [n] -> String.dropRight (n-1) voltsStr ++ "v"
                  _ -> "error"
      voltageFill =
        if voltage > 10.5
        then "green"
        else if voltage > 10.1
             then "orange"
             else "red"
      voltageReadout =
        g [] [
           Svg.rect [ x "-48", y "-48", width "16", height "6", fill voltageFill ] []
         , text_ [ textAnchor "end", x "-34", y "-43.5"
                 , fontFamily "Lucida Console", fontSize "4", fill "white"
                 ] [ text voltsFmtd ]
         ]

  in g [ transform "translate(50 50)" ] [
         movingGroup
       , pitchLadder
       , altIndicator
       , headingIndicator
       , indicator
       , voltageReadout
    ]

mkPitchLadder =
  let pitchRung deg =
        let label = text_ [
                textAnchor "end", x "-24", y "1"
              , fontFamily "Lucida Console", fontSize "4"
              ] [ text (toString deg ++ "°") ]
            rung = g [ stroke "#202020" ] [
                line [ x1 "-23", y1 "0", x2 "-13", y2 "0" ] []
              , line [ x1  "23", y1 "0", x2  "13", y2 "0" ] []
              ]
            translate = "translate(0 " ++ toString -(toFloat deg / pitchScale) ++ ")"
        in g [ transform translate ] <| if deg /= 0 then [ rung, label ] else [ rung ]
  in List.map (\x -> pitchRung (x*15)) (List.range -6 6)

mkAltLadder baroAlt =
  let short = "M42,0 L47,0 Z"
      long =  "M37,0 L47,0 Z"
      altScale = 10
      -- get two decimal places of precision
      altStr = toString baroAlt
      altFmtd = case String.indices "." (String.reverse altStr) of
                  [] -> altStr ++ ".00"
                  [1] -> altStr ++ "0"
                  [n] -> String.dropRight (n-2) altStr
                  _ -> "error"
      altRung m =
        let label = text_ [
                textAnchor "end", x "36", y "1"
              , fontFamily "Lucida Console", fontSize "4"
              ] [ text (toString m ++ "m") ]
            rung len = Svg.path [ d len, stroke "#202020" ] []
            translate = "translate(0 " ++ toString ((toFloat -m + baroAlt) * altScale - 20) ++ ")"
        in if m % 2 == 0
           then g [ transform translate ] [ rung long, label ]
           else g [ transform translate ] [ rung short ]
      indicator = g [] [
          Svg.path [ d "M45,-20 L42,-22.5 L27,-22.5 L27,-17.5 L42,-17.5 Z", stroke "black" ] []
        , text_ [ textAnchor "end", x "42", y "-18.5"
                , fontFamily "Lucida Console", fontSize "4", fill "white"
                ] [ text altFmtd ]
        ]
  in List.reverse <| indicator :: List.map altRung (List.range (round baroAlt - 20) (round baroAlt + 20))

mkHeadingIndicator yawDeg =
  let short = "M0,44 L0,47 Z"
      long =  "M0,41 L0,47 Z"
      hdgScale = 5
      hdgStr = toString (truncate yawDeg)
      hdgRung deg =
        let label = text_ [
                textAnchor "middle", x "0", y "40.5"
              , fontFamily "Lucida Console", fontSize "4"
              ] [ text (toString (deg % 360) ++ "°") ]
            rung len = Svg.path [ d len, stroke "#202020" ] []
            translate = "translate(" ++ toString ((toFloat deg - yawDeg) * hdgScale) ++ " 0)"
        in if deg % 5 == 0
           then g [ transform translate ] [ rung long, label ]
           else g [ transform translate ] [ rung short ]
      indicator = g [] [
          Svg.path [ d "M0,45.5 L-3.5,43 L-3.5,39 L3.5,39 L3.5,43 Z", stroke "black" ] []
        , text_ [ textAnchor "middle", x "0", y "42.5"
                , fontFamily "Lucida Console", fontSize "4", fill "white"
                ] [ text hdgStr ]
        ]
  in List.reverse <| indicator :: List.map (\x -> hdgRung (x*1)) (List.range (round yawDeg - 20) (round yawDeg + 20))
