module Main exposing (..)

import Basics.Extra exposing (never)
import Bootstrap.Html exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Shorthand exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Json
import Json.Decode exposing ((:=))
import Task
import Time exposing (Time, millisecond)
import PFD exposing (..)

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

type alias PackedStatus =
  { valid : Bool
  , roll : Float
  , pitch : Float
  , yaw : Float
  , baro_alt : Float
  , fix : GpsFix
  , num_sv : Int
  , lat : Int
  , lon : Int
  , alt : Int
  , vground : Int
  , heading : Float
  , rcinput : Tristate
  , telem : Tristate
  , px4io : Tristate
  , sens_cal : Tristate
  , gyro_progress : Float
  , mag_progress : Float
  , accel_progress : Float
  , arming_mode : ArmingMode
  , control_modes : ControlModes
  , battery_voltage : Float
  }

type GpsFix = FixNone | Fix2d | Fix3d

type Tristate = Negative | Neutral | Positive

type alias ControlModes =
  { ui_mode : ControlSource
  , yaw_mode : YawMode
  , thr_mode : ThrottleMode
  }

type ArmingMode = Safe | Armed

type ControlSource = Ppm | Gcs | Nav

type YawMode = Rate | Heading

type ThrottleMode = DirectUi | AltUi | AltSetpt

initPackedStatus : PackedStatus
initPackedStatus = {
    valid = False
  , roll = 0
  , pitch = 0
  , yaw = 0
  , baro_alt = 0
  , fix = FixNone
  , num_sv = 0
  , lat = 0
  , lon = 0
  , alt = 0
  , vground = 0
  , heading = 0
  , rcinput = Negative
  , telem = Negative
  , px4io = Negative
  , sens_cal = Negative
  , gyro_progress = 0
  , mag_progress = 0
  , accel_progress = 0
  , arming_mode = Safe
  , control_modes = {
        ui_mode = Ppm
      , yaw_mode = Rate
      , thr_mode = DirectUi
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
  | FetchPackedStatusSucceed PackedStatus
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
       (Http.get decodePackedStatus url)

decodePackedStatus : Json.Decoder PackedStatus
decodePackedStatus = PackedStatus
  `Json.map` ("valid" := Json.bool)
  `thenMap`  ("roll"  := Json.float)
  `thenMap`  ("pitch" := Json.float)
  `thenMap`  ("yaw"   := Json.float)
  `thenMap`  ("baro_alt" := Json.float)
  `thenMap`  ("fix" := decodeGpsFix)
  `thenMap`  ("num_sv" := Json.int)
  `thenMap`  ("lat" := Json.int)
  `thenMap`  ("lon" := Json.int)
  `thenMap`  ("alt" := Json.int)
  `thenMap`  ("vground" := Json.int)
  `thenMap`  ("heading" := Json.float)
  `thenMap`  ("rcinput" := decodeTristate)
  `thenMap`  ("telem" := decodeTristate)
  `thenMap`  ("px4io" := decodeTristate)
  `thenMap`  ("sens_cal" := decodeTristate)
  `thenMap`  ("gyro_progress" := Json.float)
  `thenMap`  ("mag_progress" := Json.float)
  `thenMap`  ("accel_progress" := Json.float)
  `thenMap`  ("arming_mode" := decodeArmingMode)
  `thenMap`  ("control_modes" := decodeControlModes)
  `thenMap`  ("battery_voltage" := Json.float)

decodeGpsFix : Json.Decoder GpsFix
decodeGpsFix =
  Json.customDecoder Json.string (\tag ->
    case tag of
      "FixNone" -> Ok FixNone
      "Fix2d" -> Ok Fix2d
      "Fix3d" -> Ok Fix3d
      str -> Err ("unrecognized GpsFix tag: " ++ str))

decodeTristate : Json.Decoder Tristate
decodeTristate =
  Json.customDecoder Json.string (\tag ->
    case tag of
      "Negative" -> Ok Negative
      "Neutral" -> Ok Neutral
      "Positive" -> Ok Positive
      str -> Err ("unrecognized Tristate tag: " ++ str))

decodeArmingMode : Json.Decoder ArmingMode
decodeArmingMode =
  Json.customDecoder Json.string (\tag ->
    case tag of
      "Safe" -> Ok Safe
      "Armed" -> Ok Armed
      str -> Err ("unrecognized ArmingMode tag: " ++ str))

decodeControlModes : Json.Decoder ControlModes
decodeControlModes = ControlModes
  `Json.map` ("ui_mode" := decodeControlSource)
  `thenMap` ("yaw_mode" := decodeYawMode)
  `thenMap` ("thr_mode" := decodeThrottleMode)

decodeControlSource : Json.Decoder ControlSource
decodeControlSource =
  Json.customDecoder Json.string (\tag ->
    case tag of
      "Ppm" -> Ok Ppm
      "Gcs" -> Ok Gcs
      "Nav" -> Ok Nav
      str -> Err ("unrecognized ControlSource tag: " ++ str))

decodeYawMode : Json.Decoder YawMode
decodeYawMode =
  Json.customDecoder Json.string (\tag ->
    case tag of
      "Rate" -> Ok Rate
      "Heading" -> Ok Heading
      str -> Err ("unrecognized YawMode tag: " ++ str))

decodeThrottleMode : Json.Decoder ThrottleMode
decodeThrottleMode =
  Json.customDecoder Json.string (\tag ->
    case tag of
      "DirectUi" -> Ok DirectUi
      "AltUi" -> Ok AltUi
      "AltSetpt" -> Ok AltSetpt
      str -> Err ("unrecognized ThrottleMode tag: " ++ str))

thenMap : Json.Decoder (a -> b) -> Json.Decoder a -> Json.Decoder b
thenMap f aDecoder =
  f `Json.andThen` (\f' -> f' `Json.map` aDecoder)
