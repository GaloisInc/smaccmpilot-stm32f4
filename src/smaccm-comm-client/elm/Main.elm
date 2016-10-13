module Main exposing (..)

import Basics.Extra exposing (never)
import Bootstrap.Html exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Shorthand exposing (..)
import Html.Attributes exposing (..)
import Http
import Task
import Time exposing (Time, millisecond)
import PFD exposing (..)

import SMACCMPilot.Comm.Types.ArmingMode as ArmingMode
import SMACCMPilot.Comm.Types.ControlSource as ControlSource
import SMACCMPilot.Comm.Types.GpsFix as GpsFix
import SMACCMPilot.Comm.Types.PackedStatus as PackedStatus
import SMACCMPilot.Comm.Types.ThrottleMode as ThrottleMode
import SMACCMPilot.Comm.Types.Tristate as Tristate
import SMACCMPilot.Comm.Types.YawMode as YawMode

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL


type alias Model =
  { packedStatus : PackedStatus.PackedStatus
  , lastUpdate : Time
  , lastUpdateDt : Float -- milliseconds
  , httpError : Maybe Http.Error
  }

initPackedStatus : PackedStatus.PackedStatus
initPackedStatus = {
    valid = False
  , roll = 0
  , pitch = 0
  , yaw = 0
  , baro_alt = 0
  , fix = GpsFix.FixNone
  , num_sv = 0
  , lat = 0
  , lon = 0
  , alt = 0
  , vground = 0
  , heading = 0
  , rcinput = Tristate.Negative
  , telem = Tristate.Negative
  , px4io = Tristate.Negative
  , sens_cal = Tristate.Negative
  , gyro_progress = 0
  , mag_progress = 0
  , accel_progress = 0
  , arming_mode = ArmingMode.Safe
  , control_modes = {
        ui_mode = ControlSource.Ppm
      , yaw_mode = YawMode.Rate
      , thr_mode = ThrottleMode.DirectUi
      }
  , battery_voltage = 0
  }

init : (Model, Cmd Msg)
init =
  ( Model initPackedStatus 0 0 Nothing
  , fetchPackedStatus
  )

-- UPDATE


type Msg
  = Poll
  | UpdateTime Time
  | FetchPackedStatusSucceed PackedStatus.PackedStatus
  | FetchFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Poll ->
      (model, fetchPackedStatus)
    UpdateTime time ->
      let dt = Time.inMilliseconds time - Time.inMilliseconds model.lastUpdate
      in ({model | lastUpdate = time, lastUpdateDt = dt}, Cmd.none)
    FetchPackedStatusSucceed newPackedStatus ->
      ({model | packedStatus = newPackedStatus, httpError = Nothing}, updateTime)
    FetchFail err ->
      ({model | httpError = Just err}, Cmd.none)

updateTime = Task.perform never UpdateTime Time.now

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
          h2 [] [text ("Update latency: " ++ toString model.lastUpdateDt ++ "ms")]
        , renderCalProgress model.packedStatus.gyro_progress
        , renderCalProgress model.packedStatus.mag_progress
        -- , h2_ ("Heading: " ++ toString (if model.packedStatus.yaw < 0
        --                                 then 360 + (model.packedStatus.yaw * 180 / pi)
        --                                 else model.packedStatus.yaw * 180 / pi))
        , case model.httpError of
            Just err -> div [ class "alert alert-warning" ] [ text (toString err) ]
            Nothing -> div [] []
        ]
      ]
    ]
{-
<ul class="nav nav-tabs">
  <li role="presentation" class="active"><a href="#">Home</a></li>
  <li role="presentation"><a href="#">Profile</a></li>
  <li role="presentation"><a href="#">Messages</a></li>
</ul>
<!-- Tab panes -->
  <div class="tab-content">
    <div role="tabpanel" class="tab-pane active" id="home">...</div>
    <div role="tabpanel" class="tab-pane" id="profile">...</div>
    <div role="tabpanel" class="tab-pane" id="messages">...</div>
    <div role="tabpanel" class="tab-pane" id="settings">...</div>
  </div>
-}
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

{-
<div class="progress">
  <div class="progress-bar progress-bar-striped active" role="progressbar" aria-valuenow="45" aria-valuemin="0" aria-valuemax="100" style="width: 45%">
    <span class="sr-only">45% Complete</span>
  </div>
</div>
-}
renderCalProgress : Float -> Html Msg
renderCalProgress p =
  let inProgress = p < 1.0
      pct = toString (p * 100) ++ "%"
  in div' { class = "progress" }
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


-- HTTP


fetchPackedStatus : Cmd Msg
fetchPackedStatus =
  let url = "http://localhost:8080/controllable_vehicle_i/packed_status"
  in Task.perform
       FetchFail
       FetchPackedStatusSucceed
       (Http.get PackedStatus.decode url)
