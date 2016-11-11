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
import Time exposing (Time, millisecond)
import PFD exposing (..)

import SMACCMPilot.Comm.Interface.ControllableVehicle as CV
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
  { packedStatus : PackedStatus
  , lastUpdate : Time
  , lastUpdateDt : Float -- milliseconds
  , httpError : Maybe Http.Error
  }

cvc : CV.Client Msg
cvc = CV.client FetchFail CVResponse "http://localhost:8080"

init : (Model, Cmd Msg)
init =
  ( Model PackedStatus.init 0 0 Nothing
  , cvc.getPackedStatus
  )

-- UPDATE

type Msg
  = Poll
  | UpdateTime Time
  | CVResponse CV.Response
  | SendReboot
  | FetchFail Http.Error

-- elm-compiler bug #635: no qualified names in record update
defaultHandler : CV.Handler Model Msg
defaultHandler = CV.defaultHandler
cvh : CV.Handler Model Msg
cvh = { defaultHandler | handleGotPackedStatus = updatePackedStatus }

updatePackedStatus : PackedStatus -> Model -> (Model, Cmd Msg)
updatePackedStatus new model = {model | packedStatus = new} ! []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Poll ->
      (model, cvc.getPackedStatus)
    UpdateTime time ->
      let dt = Time.inMilliseconds time - Time.inMilliseconds model.lastUpdate
      in {model | lastUpdate = time, lastUpdateDt = dt} ! []
    CVResponse resp ->
      let (model', cmd) = CV.handle cvh resp model
      in {model' | httpError = Nothing} ! [cmd, Task.perform never UpdateTime Time.now]
    SendReboot -> (model, cvc.setRebootReq (RebootReq.RebootReq RebootMagic.LinuxRebootMagic1))
    FetchFail err ->
      {model | httpError = Just err} ! []

-- VIEW

view : Model -> Html Msg
view model =
  container_ [
      row_ [
        colXs_ 4 [ pfd
                     model.packedStatus.pitch
                     model.packedStatus.roll
                     model.packedStatus.baro_alt
                     model.packedStatus.yaw ]
      , colXs_ 8 [
          panelDefault_ [
            ul [ class "nav nav-tabs", attribute "role" "tablist" ] [
                li' { class = "active" } [ a [ href "#calibration", attribute "data-toggle" "tab" ] [ strong_ "Calibration" ] ]
              , li_ [ a [href "#status", attribute "data-toggle" "tab" ] [ strong_ "Status"] ]
              , li_ [ a [href "#tuning", attribute "data-toggle" "tab" ] [ strong_ "Tuning" ] ]
              ]
          , panelBody_ [
              div' { class = "tab-content"} [
                div [ class "tab-pane active", id "calibration" ] [ renderCalibration model.packedStatus ]
              , div [ class "tab-pane", id "status" ] [ renderStatus model.packedStatus ]
              , div [ class "tab-pane", id "tuning" ] [ text "Tuning TODO" ]
              ]
            ]
          ]
        ]
    , row_ [ colXs_ 4 [ node "button" [ class "btn btn-primary btn-lg btn-block", onClick SendReboot ] [ text "Reboot" ] ] ]
    , row_ [ colXs_ 12 [
        case model.httpError of
          Just err -> div [ class "alert alert-warning" ] [ text (toString err) ]
          Nothing -> div [] []
      ] ]
    ]
  ]

renderPidTabs : Html Msg
renderPidTabs = panelDefault_ [
    ul [ class "nav nav-tabs", attribute "role" "tablist" ] [
        li' { class = "active" } [ a [ href "#altitude", attribute "data-toggle" "tab" ] [ text "Altitude" ] ]
      , li_ [ a [href "#attitude", attribute "data-toggle" "tab" ] [ text "Attitude"] ]
      ]
  , div' { class = "tab-content" } [
        div [ class "tab-pane active", id "altitude", attribute "role" "tabpanel" ] [ text "Altitude PID config" ]
      , div [ class "tab-pane", id "attitude", attribute "role" "tabpanel" ] [ text "Attitude PID config" ]
      ]
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
subscriptions model =
  Time.every (66 * millisecond) (\_ -> Poll)
