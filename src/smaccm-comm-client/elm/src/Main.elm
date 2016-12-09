module Main exposing (..)

import Basics.Extra exposing (never)
import Bootstrap.Html exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Shorthand exposing (..)
import Html.Attributes exposing (..)
import Http
import Task
import Time exposing (Time, millisecond, second)
import PFD exposing (..)

import SMACCMPilot.Comm.Interface.ControllableVehicle as CV exposing (ControllableVehicle)
import SMACCMPilot.Comm.Types.ArmingMode as ArmingMode
import SMACCMPilot.Comm.Types.ControlSource as ControlSource
import SMACCMPilot.Comm.Types.PackedStatus as PackedStatus exposing (PackedStatus)
import SMACCMPilot.Comm.Types.RebootMagic as RebootMagic
import SMACCMPilot.Comm.Types.RebootReq as RebootReq
import SMACCMPilot.Comm.Types.ThrottleMode as ThrottleMode
import SMACCMPilot.Comm.Types.Tristate as Tristate
import SMACCMPilot.Comm.Types.YawMode as YawMode

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { cv : ControllableVehicle
  , refreshRate : Float -- milliseconds
  , altSmoother : Smoother
  , latencySmoother : Smoother
  , lastUpdate : Time
  , lastUpdateDts : List Float -- milliseconds
  , httpError : Maybe Http.Error
  }

cvc : CV.Client Msg
cvc = CV.client FetchFail CVResponse "http://localhost:8080"

init : (Model, Cmd Msg)
init =
  let altWeights = [
          -0.004079714666025863
        , -0.011002950421607123
        , -0.004853702172814258
        , 0.03994619789045572
        , 0.12899291808975988
        , 0.22464354904548067
        , 0.26647104342536615
        , 0.22464354904548067
        , 0.12899291808975988
        , 0.03994619789045572
        , -0.004853702172814258
        , -0.011002950421607123
        , -0.004079714666025863
        ]
      latencyWeights = [
          0.11538864317459548
        , 0.36510250931275834
        , 0.5000373407321809
        , 0.36510250931275834
        , 0.11538864317459548
        , 0.0006633631777757225
        ]
  in ( { cv = CV.init
       , refreshRate = 66
       , altSmoother = mkSmoother altWeights
       , latencySmoother = mkSmoother latencyWeights
       , lastUpdate = 0
       , lastUpdateDts = []
       , httpError = Nothing
       }
     , cvc.getPackedStatus
     )

-- UPDATE

type Msg
  = Poll
  | SetRefreshRate Float -- hz
  | UpdateLatency Time
  | UpdateTime Time
  | CVResponse CV.Response
  | SendReboot
  | FetchTuning
  | FetchFail Http.Error

cvh : CV.Handler Model Msg
cvh = let upd m f = { m | cv = f m.cv } in CV.updatingHandler upd

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Poll ->
      (model, cvc.getPackedStatus)
    SetRefreshRate hz ->
      { model | refreshRate = 1*second / (hz + 0.0001) } ! []
    UpdateLatency time ->
      let -- use the current dt if we haven't seen a sample since the last update
          dt = Time.inMilliseconds time - Time.inMilliseconds model.lastUpdate
          window = model.latencySmoother.firState.size
          s' =
            case model.lastUpdateDts of
              [] -> putSome model.latencySmoother (List.repeat window dt)
              ls -> putSome model.latencySmoother ls
      in { model | latencySmoother = s', lastUpdateDts = [] } ! []
    UpdateTime time ->
      let dt = Time.inMilliseconds time - Time.inMilliseconds model.lastUpdate
      in { model | lastUpdate = time, lastUpdateDts = dt :: model.lastUpdateDts } ! []
    CVResponse resp ->
      let (model', cmd) = CV.handle cvh resp model
          model'' = updateSmoothers model'
      in { model'' | httpError = Nothing } ! [ cmd, Task.perform never UpdateTime Time.now ]
    SendReboot -> (model, cvc.setRebootReq (RebootReq.RebootReq RebootMagic.LinuxRebootMagic1))
    FetchTuning -> model ! fetchTuning
    FetchFail err ->
      { model | httpError = Just err } ! []

fetchTuning : List (Cmd Msg)
fetchTuning = [
    cvc.getAltitudeRatePid
  , cvc.getAltitudePositionPid
  , cvc.getAttitudeRollStab
  , cvc.getAttitudePitchStab
  , cvc.getYawRatePid
  , cvc.getYawPositionPid
  ]

updateSmoothers : Model -> Model
updateSmoothers model =
  { model | altSmoother = putNext model.altSmoother model.cv.packedStatus.lidar_alt }

-- VIEW

view : Model -> Html Msg
view model =
  container_ [ panelDefault_ [ panelHeading_ [ panelTitle_ "SMACCMPilot" ], panelBody_ [
      row_ [
        colXs_ 4 [ pfd
                     model.cv.packedStatus.pitch
                     model.cv.packedStatus.roll
                     model.altSmoother.value
                     model.cv.packedStatus.yaw ]
      , colXs_ 8 [
          panelDefault_ [
            ul [ class "nav nav-tabs", attribute "role" "tablist" ] [
                li' { class = "active" } [ a [ href "#calibration", attribute "data-toggle" "tab" ] [ strong_ "Calibration" ] ]
              , li_ [ a [href "#status", attribute "data-toggle" "tab" ] [ strong_ "Control Law Status"] ]
              , li_ [ a [href "#tuning", attribute "data-toggle" "tab" , onClick FetchTuning ] [ strong_ "Tuning" ] ]
              ]
          , panelBody_ [
              div' { class = "tab-content"} [
                div [ class "tab-pane active", id "calibration" ] [ renderCalibration model.cv.packedStatus ]
              , div [ class "tab-pane", id "status" ] [ renderStatus model.cv.packedStatus ]
              , div [ class "tab-pane", id "tuning" ] [ renderTuning model ]
              ]
            ]
          ]
        ]
    ]
    , hr_
    , row_ [ colXs_ 12 [ table' { class = "table" } [ tbody_ [
           tr
             [ let v = model.latencySmoother.value
               in class (if v < 1000
                         then "success"
                         else if v >= 1000 && v < 5000
                         then "warning"
                         else "danger") ]
             [ thLabel 80 "Time between updates", td_ [ text (toString (round model.latencySmoother.value) ++ "ms") ] ]
         , tr_ [ thLabel 80 "Refresh rate"
               , td_ [ div' { class = "input-group" } [
                   inputFloat' {
                     class = "form-control"
                   , name = "refresh-rate"
                   , placeholder = Nothing
                   , value = 15
                   , min = Just 0
                   , max = Just 15
                   , step = Nothing
                   , update = {
                         onEnter = Nothing
                       , onKeyboardLost = Nothing
                       , onInput = Just (\res -> Maybe.map SetRefreshRate (Result.toMaybe res))
                       }
                   }
                 , span' { class = "input-group-addon" } [ text "hz" ] ] ] ]
         , tr_ [ thLabel 80 "Linux VM"
               , td_ [ node "button"
                   [ class "btn btn-primary btn-lg btn-block", onClick SendReboot ]
                   [ text "Reboot" ] ] ]
         ] ] ] ]
    , row_ [ colXs_ 12 [
          case model.httpError of
            Just err -> div [ class "alert alert-warning" ] [ text (toString err) ]
            Nothing -> div [] []
        ] ]
    ] ] ]
    
    

thLabel w str = th [ style [ ("width", toString w ++ "%"), ("vertical-align", "middle") ] ] [ text str ]

renderTuning : Model -> Html Msg
renderTuning model = div_ [
    panelDefault_ [
        panelHeading_ [ panelTitle_ "Altitude" ]
      , panelBody_ [ text "Altitude stuff" ] ]
  , panelDefault_ [
        panelHeading_ [ panelTitle_ "Attitude" ]
      , panelBody_ [ text "Attitude stuff" ] ]
  , panelDefault_ [
        panelHeading_ [ panelTitle_ "Yaw" ]
      , panelBody_ [ text "Yaw stuff" ] ]
  ]

renderCalibration : PackedStatus -> Html Msg
renderCalibration packedStatus =
  let label str = th [ style [ ("width", "10%") ] ] [ text str ]
  in table' { class = "table" } [ tbody_ [
         tr_ [ label "Gyro", td_ [ renderCalProgress packedStatus.gyro_progress ] ]
       , tr_ [ label "Mag", td_ [ renderCalProgress packedStatus.mag_progress ] ]
       , tr_ [ label "Accel", td_ [ renderCalProgress packedStatus.accel_progress ] ]
       ] ]

renderStatus : PackedStatus -> Html Msg
renderStatus packedStatus =
  let label str = th [ style [ ("width", "20%"), ("vertical-align", "middle") ] ] [ text str ]
      bgrp btns = div [ class "btn-group btn-group-justified btn-group-sm" ] btns
      btn lbl active =
        let cls = if active then "btn-primary" else "btn-default"
        in div [ class "btn-group" ] [ node "button" [ class ("btn " ++ cls), attribute "disabled" "disabled" ] [ text lbl ] ]
      tristate lbl st =
        tr_ [ label lbl, td_ [ bgrp [
                  btn "Negative" (st == Tristate.Negative)
                , btn "Neutral" (st == Tristate.Neutral)
                , btn "Positive" (st == Tristate.Positive)
                ] ] ]
  in table' { class = "table" } [ tbody_ [
         tr_ [ label "Arming Mode", td_ [ bgrp [
                   btn "Safe" (packedStatus.arming_mode == ArmingMode.Safe)
                 , btn "Armed" (packedStatus.arming_mode == ArmingMode.Armed)
                 ] ] ]
       , tr_ [ label "Throttle Mode", td_ [ bgrp [
                   btn "DirectUi" (packedStatus.control_modes.thr_mode == ThrottleMode.DirectUi)
                 , btn "AltUi" (packedStatus.control_modes.thr_mode == ThrottleMode.AltUi)
                 , btn "AltSetpt" (packedStatus.control_modes.thr_mode == ThrottleMode.AltSetpt)
                 ] ] ]
       , tr_ [ label "UI Mode", td_ [ bgrp [
                   btn "Ppm" (packedStatus.control_modes.ui_mode == ControlSource.Ppm)
                 , btn "Gcs" (packedStatus.control_modes.ui_mode == ControlSource.Gcs)
                 , btn "Nav" (packedStatus.control_modes.ui_mode == ControlSource.Nav)
                 ] ] ]
       , tr_ [ label "Yaw Mode", td_ [ bgrp [
                   btn "Rate" (packedStatus.control_modes.yaw_mode == YawMode.Rate)
                 , btn "Heading" (packedStatus.control_modes.yaw_mode == YawMode.Heading)
                 ] ] ]
       , tristate "rcinput" packedStatus.rcinput
       , tristate "telem" packedStatus.telem
       , tristate "px4io" packedStatus.px4io
       , tristate "sens_cal" packedStatus.sens_cal
       ] ]
    
renderCalProgress : Float -> Html Msg
renderCalProgress p =
  let inProgress = p < 1.0
      pct = toString (p * 100) ++ "%"
  in div [ class "progress", style [ ("margin-bottom", "0") ] ]
       [ div [ classList [
                   ("progress-bar", True)
                 , ("progress-bar-striped", True)
                 , ("active", inProgress)
                 , ("progress-bar-info", inProgress)
                 , ("progress-bar-success", not inProgress)
                 ]
             , style [ ("width", pct) ]
             ]
           [ text pct ]
       ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [
    Time.every (model.refreshRate * millisecond) (\_ -> Poll)
  , Time.every second UpdateLatency
  ]

-- MOVE TO ANOTHER MODULE

type alias FIR =
  { weights : List Float
  , inputs : List Float
  , size : Int
  }

{-| Initialize a FIR filter with impulse response coefficients listed from most to least recent -}
mkFir : List Float -> FIR
mkFir ws =
  let s = List.length ws
  in { weights = ws, inputs = List.repeat s 0.0, size = s }

{-| Run a FIR filter with a list of inputs from most to least recent. Returns the intermediate outputs and the updated filter state -}
fir : FIR -> List Float -> (List Float, FIR)
fir f is =
  let go i (os, f') =
        let (o, f'') = fir1 f' i
        in (o :: os, f'')
  in List.foldr go ([], f) is

{-| Run a FIR filter with a single new input. Returns the output and the updated filter state -}
fir1 : FIR -> Float -> (Float, FIR)
fir1 f i =
  let output = List.foldr go 0 (List.map2 (,) f.weights inputs')
      inputs' = List.take f.size (i :: f.inputs)
      f' = { f | inputs = inputs' }
      go (w, i) s = s + (w * i)
  in (output, f')

type alias Smoother =
  { firState : FIR
  , value : Float
  , mag : Float
  }

mkSmoother ws =
  { firState = mkFir ws, mag = List.sum ws, value = 0 }

putNext : Smoother -> Float -> Smoother
putNext s i =
  let (v, fs') = fir1 s.firState i
  in { s | firState = fs', value = v / s.mag }

putSome : Smoother -> List Float -> Smoother
putSome s is =
  let (vs, fs') = fir s.firState is
  in { s | firState = fs', value = Maybe.withDefault s.value (Maybe.map (\x -> x / s.mag) (List.head vs)) }
