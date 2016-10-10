module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Json.Decode exposing ((:=))
import Task
import Time exposing (Time, millisecond)


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
  ( Model initPackedStatus  Nothing
  , fetchPackedStatus
  )



-- UPDATE


type Msg
  = Poll
  | FetchPackedStatusSucceed PackedStatus
  | FetchFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Poll ->
      (model, fetchPackedStatus)

    FetchPackedStatusSucceed newPackedStatus ->
      ({model | packedStatus = newPackedStatus, httpError = Nothing}, Cmd.none)

    FetchFail err ->
      ({model | httpError = Just err}, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text ("Gyro calibration: " ++ toString model.packedStatus.gyro_progress)]
    , h2 [] [text ("Mag calibration: " ++ toString model.packedStatus.mag_progress)]
    , case model.httpError of
        Just err -> div [ style [("color", "red")] ] [ text (toString err) ]
        Nothing -> div [] []
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (1000 * millisecond) (\_ -> Poll)


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
