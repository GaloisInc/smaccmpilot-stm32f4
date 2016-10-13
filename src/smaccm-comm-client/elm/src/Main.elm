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

import SMACCMPilot.Comm.Interface.ControllableVehicle as CV
import SMACCMPilot.Comm.Types.PackedStatus as PackedStatus exposing (PackedStatus)

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
  | FetchFail Http.Error

-- elm-compiler bug #635: no qualified names in record update
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
          h2 [] [text ("Update latency: " ++ toString model.lastUpdateDt ++ "ms")]
        , renderCalProgress model.packedStatus.gyro_progress
        , renderCalProgress model.packedStatus.mag_progress
        , case model.httpError of
            Just err -> div [ class "alert alert-warning" ] [ text (toString err) ]
            Nothing -> div [] []
        ]
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
