port module Main exposing (..)

import Char
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes as A exposing (..)
import Http
import Keyboard
import Set exposing (Set)
import Task
import Time exposing (..)

import Bbox exposing (..)
import PFD exposing (..)

import SMACCMPilot.Comm.Interface.ControllableVehicle as CV exposing (ControllableVehicle)
import SMACCMPilot.Comm.Types.ArmingMode as ArmingMode
import SMACCMPilot.Comm.Types.ControlSource as ControlSource
import SMACCMPilot.Comm.Types.PackedStatus as PackedStatus exposing (PackedStatus)
import SMACCMPilot.Comm.Types.PidConfig as PidConfig exposing (PidConfig)
import SMACCMPilot.Comm.Types.RebootMagic as RebootMagic
import SMACCMPilot.Comm.Types.RebootReq as RebootReq
import SMACCMPilot.Comm.Types.ThrottleMode as ThrottleMode
import SMACCMPilot.Comm.Types.Tristate as Tristate
import SMACCMPilot.Comm.Types.UserInput as UI
import SMACCMPilot.Comm.Types.YawMode as YawMode

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { cv : ControllableVehicle
  , cvc : CV.Client Msg
  , refreshRate : Float -- milliseconds
  , latencySmoother : Smoother
  , latencyModalVisible : Bool
  , lastUpdate : Time
  , lastUpdateDts : List Float -- milliseconds
  , httpError : Maybe Http.Error
  , httpTimeout : Time
  , keysDown : Set Char
  , keysSincePeriod : Set Char
  }

type alias Pid =
  { key : String
  , prettyName : String
  , fromCv : ControllableVehicle -> PidConfig
  , updCv : ControllableVehicle -> PidConfig -> ControllableVehicle
  , get : CV.Client Msg -> Cmd Msg
  , set : ControllableVehicle -> CV.Client Msg -> PidConfig -> Cmd Msg
  }

mkCvc : Time -> CV.Client Msg
mkCvc timeout = CV.client timeout FetchFail CVMsg "http://localhost:8080"

initTimeout : Time
initTimeout = 2 * second

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
      initCvc = mkCvc initTimeout
      initRefreshRate = 200 * millisecond
  in ( { cv = CV.init
       , cvc = initCvc
       , refreshRate = initRefreshRate
       , latencySmoother = mkSmoother latencyWeights
       , latencyModalVisible = False
       , lastUpdate = 0
       , lastUpdateDts = []
       , httpError = Nothing
       , httpTimeout = initTimeout
       , keysDown = Set.empty
       , keysSincePeriod = Set.empty
       }
     , Cmd.batch [ initCvc.pollPackedStatus (Just initRefreshRate), Task.perform InitializeTime Time.now ]
     )

-- UPDATE

type Msg
  = SetRefreshRate String -- Float hz
  | UpdateLatency Time
  | InitializeTime Time
  | UpdateTime Time
  | CVMsg CV.Msg
  | SendReboot
  | KeyUp Keyboard.KeyCode
  | KeyDown Keyboard.KeyCode
  | FetchTuning
  | FetchFail Http.Error
  | SetTimeout String -- Int ms
  | FocusPFD
  | FocusCameraTarget
  | SendControlInput
  | UpdatePid Pid PidConfig
  | UpdateNominalThrottle Float
  | Nop

cvh : CV.Handler Model Msg
cvh = let upd m f = { m | cv = f m.cv } in CV.updatingHandler upd

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetRefreshRate hzStr ->
      case String.toFloat hzStr of
        Ok hz -> let rate = 1*second / (hz + 0.0001)
                 in { model | refreshRate = rate, cv = CV.mapPollRates (always rate) model.cv } ! []
        Err _ -> model ! []
    UpdateLatency time ->
      let -- use the current dt if we haven't seen a sample since the last update
          dt = Time.inMilliseconds time - Time.inMilliseconds model.lastUpdate
          window = model.latencySmoother.firState.size
          s1 =
            case model.lastUpdateDts of
              [] -> putSome model.latencySmoother (List.repeat window dt)
              ls -> putSome model.latencySmoother ls
          v = s1.value
          doShow = v > 5000 && model.refreshRate < 10 * second
      in { model | latencySmoother = s1, lastUpdateDts = [], latencyModalVisible = doShow }
         ! [ if doShow
             then if model.latencyModalVisible then showModal () else Cmd.none
             else hideModal () ]
    InitializeTime time -> { model | lastUpdate = time } ! []
    UpdateTime time ->
      let dt = Time.inMilliseconds time - Time.inMilliseconds model.lastUpdate
      in { model | lastUpdate = time, lastUpdateDts = dt :: model.lastUpdateDts } ! []
    CVMsg msg ->
      let (model1, cmd) = CV.handle cvh (\m -> m.cvc) msg model
          model2 = updateSmoothers model1
      in case CV.networkMsg msg of
           True -> { model2 | httpError = Nothing } ! [ cmd, Task.perform UpdateTime Time.now ]
           False -> model2 ! [ cmd ]
    SendReboot -> (model, model.cvc.setRebootReq (RebootReq.RebootReq RebootMagic.LinuxRebootMagic1))
    FetchTuning -> model ! fetchTuning model
    FetchFail err ->
      { model | httpError = Just err } ! []
    SetTimeout msStr ->
      case String.toInt msStr of
        Ok ms ->
          let to = toFloat ms * millisecond
          in { model | cvc = mkCvc to, httpTimeout = to } ! []
        Err _ -> model ! []
    FocusPFD -> model ! [ model.cvc.pollCameraTargetInput Nothing ]
    FocusCameraTarget -> model ! [ model.cvc.pollCameraTargetInput (Just model.refreshRate) ]
    KeyUp kc -> { model | keysDown = Set.remove (Char.fromCode kc |> Char.toUpper) model.keysDown
                } ! []
    KeyDown kc -> handleKeyDown model kc
    SendControlInput ->
      if Set.isEmpty model.keysDown && Set.isEmpty model.keysSincePeriod
        then model ! []
        else let uir0 = { throttle = 1, pitch = 0, roll = 0, yaw = 0 }
                 uir = Set.foldl addControlInput uir0 (Set.union model.keysDown model.keysSincePeriod)
             in { model | keysSincePeriod = Set.empty } ! [ model.cvc.setUserInputRequest uir ]
    UpdatePid pid pidConfig ->
      let cv = model.cv
      in { model | cv = pid.updCv cv pidConfig } ! [ pid.set model.cv model.cvc pidConfig ]
    UpdateNominalThrottle thr ->
      let cv = model.cv
      in { model | cv = { cv | nominalThrottle = thr } } ! [ model.cvc.setNominalThrottle thr ]
    Nop -> model ! []

addControlInput : Char -> UI.UserInput -> UI.UserInput
addControlInput key uir =
  case key of
    'W' -> { uir | pitch = uir.pitch + 0.3 }
    'S' -> { uir | pitch = uir.pitch - 0.3 }
    'A' -> { uir | roll  = uir.roll  - 0.2 }
    'D' -> { uir | roll  = uir.roll  + 0.2 }
    'Q' -> { uir | yaw   = uir.yaw   - 0.13 }
    'E' -> { uir | yaw   = uir.yaw   + 0.13 }
    _   -> uir

handleKeyDown : Model -> Char.KeyCode -> (Model, Cmd Msg)
handleKeyDown model kc =
  case Char.fromCode kc |> Char.toUpper of
    -- Reboot VM
    '=' -> model ! [ model.cvc.setRebootReq (RebootReq.RebootReq RebootMagic.LinuxRebootMagic1) ]
    -- Otherwise keep track of which keys are down
    _ -> { model | keysDown = Set.insert (Char.fromCode kc |> Char.toUpper) model.keysDown
                 , keysSincePeriod = Set.insert (Char.fromCode kc |> Char.toUpper) model.keysSincePeriod
         } ! []

fetchTuning : Model -> List (Cmd Msg)
fetchTuning model = model.cvc.getNominalThrottle :: List.map (\pid -> pid.get model.cvc) pids

updateSmoothers : Model -> Model
updateSmoothers model = model

-- VIEW

view : Model -> Html Msg
view model =
  div [ class "container" ] [
    errorModal model
  , div [ class "panel panel-default" ] [ div [ class "panel-heading" ] [ h2 [ class "panel-title" ] [ text "SMACCMPilot" ] ], div [ class "panel-body" ] [
      div [ class "row" ] [
        div [ class "col-xs-5" ] [
          div [ class "panel panel-default" ] [
            ul [ class "nav nav-tabs", attribute "role" "tablist" ] [
                li [ class "active" ] [ a [ href "#pfd", attribute "data-toggle" "tab", onClick FocusPFD ] [ strong [ ] [ text "PFD" ] ] ]
              , li [ ] [ a [ href "#bbox", attribute "data-toggle" "tab", onClick FocusCameraTarget ] [ strong [ ] [ text "Camera Target" ] ] ]
              ]
          , div [ class "panel-body" ] [
              div [ class "tab-content" ] [
                div [ class "tab-pane active", id "pfd" ] [
                    pfd model.cv.packedStatus.pitch
                        model.cv.packedStatus.roll
                        model.cv.packedStatus.alt_est
                        model.cv.packedStatus.yaw
                        model.cv.packedStatus.battery_voltage
                    ]
              , div [ class "tab-pane", id "bbox" ] [ bbox model.cv.cameraTargetInput ]
              ]
            ]
          ]
        ]
      , div [ class "col-xs-7" ] [
          div [ class "panel panel-default" ] [
            ul [ class "nav nav-tabs", attribute "role" "tablist" ] [
                li [ class "active" ] [ a [href "#status", attribute "data-toggle" "tab" ] [ strong [ ] [ text "Control Law Status" ] ] ]
              , li [ ] [ a [href "#tuning", attribute "data-toggle" "tab" , onClick FetchTuning ] [ strong [ ] [ text "Tuning" ] ] ]
              ]
          , div [ class "panel-body" ] [
              div [ class "tab-content" ] [
                div [ class "tab-pane active", id "status" ] [ renderStatus model.cv.packedStatus ]
              , div [ class "tab-pane", id "tuning" ] [ renderTunings model ]
              ]
            ]
          ]
        ]
    ]
    , hr [ ] [ ]
    , div [ class "row" ] [
        div [ class "col-xs-12" ] [ table [ class "table" ] [ tbody [ ] [
            tr
              [ let v = model.latencySmoother.value
                in class (if v < 1000
                          then "success"
                          else if v >= 1000 && v < 5000
                          then "warning"
                          else "danger") ]
              [ thLabel 80 "Time between updates", td [ ] [ text (toString (round model.latencySmoother.value) ++ "ms") ] ]
          , tr [ ] [ thLabel 80 "Refresh rate"
                   , td [ ] [ div [ class "input-group" ] [
                                input [ class "form-control"
                                      , name "refresh-rate"
                                      , type_  "number"
                                      , value (toString (round (1000 / model.refreshRate)))
                                      , A.min (toString 0)
                                      , A.max (toString 15)
                                      , onInput SetRefreshRate
                                      ] []
                     , span [ class "input-group-addon" ] [ text "hz" ] ] ] ]
          , tr [ ] [ thLabel 80 "Request timeout"
                   , td [ ] [ div [ class "input-group" ] [
                                input [ class "form-control"
                                      , name "request-timeout"
                                      , type_  "number"
                                      , value (toString (inMilliseconds (model.httpTimeout)))
                                      , A.min (toString 0)
                                      , A.max (toString (inMilliseconds (60 * second)))
                                      , onInput SetTimeout
                                      ] []
                     , span [ class "input-group-addon" ] [ text "ms" ] ] ] ]
          , tr [ ] [ thLabel 80 "Linux VM"
                   , td [ ] [ node "button"
                                [ class "btn btn-primary btn-lg btn-block", onClick SendReboot ]
                                [ text "Reboot" ] ] ]
         ] ] ] ]
    , div [ class "row" ] [ div [ class "col-xs-12" ] [
          case model.httpError of
            Just err -> div [ class "alert alert-warning" ] [ text (toString err) ]
            Nothing -> div [] []
        ] ]
    ] ] ]

thLabel w str = th [ style [ ("width", toString w ++ "%"), ("vertical-align", "middle") ] ] [ text str ]

renderTunings : Model -> Html Msg
renderTunings model =
  let onFloatInput f = onInput (\str -> case String.toFloat str of
                                          Ok v -> f v
                                          Err _ -> Nop)
      nominalThrottle =
        div [ class "panel panel-default" ] [
            div [ class "panel-heading" ] [ h2 [ class "panel-title" ] [
                                               a [ attribute "data-toggle" "collapse", href ("#nominal-throttle") ]
                                                 [ text "Nominal Throttle" ] ] ]
             , div [ id "nominal-throttle", class "panel-body panel-collapse collapse" ] [
                 table [ class "table table-bordered" ] [
                     thead [ ] [ tr [ ] [ th [ ] [ text "%" ] ] ]
                   , tbody [ ] [ tr [ ] [ td [ ] [ div [ class "input-group" ] [
                                                      input [ class "form-control"
                                                            , name "nominal_throttle"
                                                            , type_ "number"
                                                            , A.min "0.0"
                                                            , A.max "100.0"
                                                            , step "0.01"
                                                            , value (toString model.cv.nominalThrottle)
                                                            , onFloatInput UpdateNominalThrottle
                                                            ] [ ] ] ] ] ] ] ] ]
      renderTuning pid =
        let pidConfig = pid.fromCv model.cv
            pidField field val upd = td [ ] [ div [ class "input-group" ] [
                                                 input [ class "form-control"
                                                       , name (pid.key ++ "_" ++ field)
                                                       , type_ "number"
                                                       , A.min "-1000.0"
                                                       , A.max "1000.0"
                                                       , step "0.01"
                                                       , value (toString val)
                                                       , onFloatInput upd
                                                       ] [ ] ] ]
        in div [ class "panel panel-default" ] [
               div [ class "panel-heading" ] [ h2 [ class "panel-title" ] [
                                                  a [ attribute "data-toggle" "collapse", href ("#" ++ pid.key) ]
                                                    [ text pid.prettyName ] ] ]
             , div [ id pid.key, class "panel-body panel-collapse collapse" ] [
                 table [ class "table table-bordered" ] [
                     thead [ ] [ tr [ ] (List.map (\hd -> th [ ] [ text hd ]) [ "p", "i", "d", "dd", "i_min", "i_max" ]) ]
                   , tbody [ ] [ tr [ ] [
                         pidField "p_gain" pidConfig.p_gain (\v -> UpdatePid pid { pidConfig | p_gain = v })
                       , pidField "i_gain" pidConfig.i_gain (\v -> UpdatePid pid { pidConfig | i_gain = v })
                       , pidField "d_gain" pidConfig.d_gain (\v -> UpdatePid pid { pidConfig | d_gain = v })
                       , pidField "dd_gain" pidConfig.dd_gain (\v -> UpdatePid pid { pidConfig | dd_gain = v })
                       , pidField "i_min" pidConfig.i_min (\v -> UpdatePid pid { pidConfig | i_min = v })
                       , pidField "i_max" pidConfig.i_max (\v -> UpdatePid pid { pidConfig | i_max = v })
                       ] ] ] ] ]
  in nominalThrottle :: List.map renderTuning pids |> div [ ]

renderStatus : PackedStatus -> Html Msg
renderStatus packedStatus =
  let label str = th [ style [ ("width", "20%"), ("vertical-align", "middle") ] ] [ text str ]
      bgrp btns = div [ class "btn-group btn-group-justified btn-group-sm" ] btns
      btn lbl active =
        let cls = if active then "btn-primary" else "btn-default"
        in div [ class "btn-group" ] [ node "button" [ class ("btn " ++ cls), attribute "disabled" "disabled" ] [ text lbl ] ]
      tristate lbl st =
        tr [ ] [ label lbl, td [ ] [ bgrp [
                     btn "Negative" (st == Tristate.Negative)
                   , btn "Neutral" (st == Tristate.Neutral)
                   , btn "Positive" (st == Tristate.Positive)
                   ] ] ]
  in table [ class "table" ] [ tbody [ ] [
         tr [ ] [ label "Arming Mode", td [ ] [ bgrp [
                      btn "Safe" (packedStatus.arming_mode == ArmingMode.Safe)
                    , btn "Armed" (packedStatus.arming_mode == ArmingMode.Armed)
                    ] ] ]
       , tr [ ] [ label "Throttle Mode", td [ ] [ bgrp [
                      btn "DirectUi" (packedStatus.control_modes.thr_mode == ThrottleMode.DirectUi)
                    , btn "AltUi" (packedStatus.control_modes.thr_mode == ThrottleMode.AltUi)
                    , btn "AltSetpt" (packedStatus.control_modes.thr_mode == ThrottleMode.AltSetpt)
                    ] ] ]
       , tr [ ] [ label "UI Mode", td [ ] [ bgrp [
                      btn "Ppm" (packedStatus.control_modes.ui_mode == ControlSource.Ppm)
                    , btn "Gcs" (packedStatus.control_modes.ui_mode == ControlSource.Gcs)
                    , btn "Nav" (packedStatus.control_modes.ui_mode == ControlSource.Nav)
                    ] ] ]
       , tr [ ] [ label "Yaw Mode", td [ ] [ bgrp [
                      btn "Rate" (packedStatus.control_modes.yaw_mode == YawMode.Rate)
                    , btn "Heading" (packedStatus.control_modes.yaw_mode == YawMode.Heading)
                    ] ] ]
       , tristate "rcinput" packedStatus.rcinput
       , tristate "telem" packedStatus.telem
       , tristate "px4io" packedStatus.px4io
       , tristate "sens_valid" packedStatus.sens_valid
       ] ]

errorModal model =
  div [ class "modal fade"
      , id "errorModal"
      , attribute "tabindex" "-1"
      , attribute "role" "dialog" ] [
    div [ class "modal-dialog modal-lg"
        , attribute "role" "document" ] [
      div [ class "modal-content"
          , style [ ("background-color", "black")
                  , ("color", "red")
                  , ("text-align", "center") ] ] [
        div [ class "modal-body" ] [
          let elapsed = toString (round (model.latencySmoother.value / 1000))
          in h1 [ class "modal-title" ] [ text ("Connection Lost: " ++ elapsed ++ "s") ]
        ]
      ]
    ]
  ]

port showModal : () -> Cmd msg
port hideModal : () -> Cmd msg

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [
    Sub.batch (CV.subscriptions model.cv CVMsg)
  , Time.every second UpdateLatency
  , Keyboard.downs KeyDown
  , Keyboard.ups KeyUp
  , if model.cv.packedStatus.control_modes.ui_mode == ControlSource.Gcs
      then Time.every (250 * millisecond) (always SendControlInput)
      else Sub.none
  ]

-- Ugly PID business

pids : List Pid
pids = [
    { key = "alt_pos"
    , prettyName = "Altitude Position"
    , fromCv = \cv -> cv.altitudePositionPid
    , updCv = \cv pidConfig -> { cv | altitudePositionPid = pidConfig }
    , get = \cvc -> cvc.getAltitudePositionPid
    , set = \_ cvc pidConfig -> cvc.setAltitudePositionPid pidConfig
    }
  , { key = "roll_pos"
    , prettyName = "Roll Position"
    , fromCv = \cv -> cv.attitudeRollStab.pos
    , updCv = \cv pidConfig ->
        let stab = cv.attitudeRollStab
        in { cv | attitudeRollStab = { stab | pos = pidConfig } }
    , get = \cvc -> cvc.getAttitudeRollStab
    , set = \cv cvc pidConfig ->
        let stab = cv.attitudeRollStab
        in cvc.setAttitudeRollStab { stab | pos = pidConfig }
    }
  , { key = "roll_rate"
    , prettyName = "Roll Rate"
    , fromCv = \cv -> cv.attitudeRollStab.rate
    , updCv = \cv pidConfig ->
        let stab = cv.attitudeRollStab
        in { cv | attitudeRollStab = { stab | rate = pidConfig } }
    , get = \cvc -> cvc.getAttitudeRollStab
    , set = \cv cvc pidConfig ->
        let stab = cv.attitudeRollStab
        in cvc.setAttitudeRollStab { stab | rate = pidConfig }
    }
  , { key = "pitch_pos"
    , prettyName = "Pitch Position"
    , fromCv = \cv -> cv.attitudePitchStab.pos
    , updCv = \cv pidConfig ->
        let stab = cv.attitudePitchStab
        in { cv | attitudePitchStab = { stab | pos = pidConfig } }
    , get = \cvc -> cvc.getAttitudePitchStab
    , set = \cv cvc pidConfig ->
        let stab = cv.attitudePitchStab
        in cvc.setAttitudePitchStab { stab | pos = pidConfig }
    }
  , { key = "pitch_rate"
    , prettyName = "Pitch Rate"
    , fromCv = \cv -> cv.attitudePitchStab.rate
    , updCv = \cv pidConfig ->
        let stab = cv.attitudePitchStab
        in { cv | attitudePitchStab = { stab | rate = pidConfig } }
    , get = \cvc -> cvc.getAttitudePitchStab
    , set = \cv cvc pidConfig ->
        let stab = cv.attitudePitchStab
        in cvc.setAttitudePitchStab { stab | rate = pidConfig }
    }
  , { key = "yaw_pos"
    , prettyName = "Yaw Position"
    , fromCv = \cv -> cv.yawPositionPid
    , updCv = \cv pidConfig -> { cv | yawPositionPid = pidConfig }
    , get = \cvc -> cvc.getYawPositionPid
    , set = \_ cvc pidConfig -> cvc.setYawPositionPid pidConfig
    }
  , { key = "yaw_rate"
    , prettyName = "Yaw Rate"
    , fromCv = \cv -> cv.yawRatePid
    , updCv = \cv pidConfig -> { cv | yawRatePid = pidConfig }
    , get = \cvc -> cvc.getYawRatePid
    , set = \_ cvc pidConfig -> cvc.setYawRatePid pidConfig
    }
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
  let go i (os, f1) =
        let (o, f2) = fir1 f1 i
        in (o :: os, f2)
  in List.foldr go ([], f) is

{-| Run a FIR filter with a single new input. Returns the output and the updated filter state -}
fir1 : FIR -> Float -> (Float, FIR)
fir1 f i =
  let output = List.foldr go 0 (List.map2 (,) f.weights inputs1)
      inputs1 = List.take f.size (i :: f.inputs)
      f1 = { f | inputs = inputs1 }
      go (w, i) s = s + (w * i)
  in (output, f1)

type alias Smoother =
  { firState : FIR
  , value : Float
  , mag : Float
  }

mkSmoother ws =
  { firState = mkFir ws, mag = List.sum ws, value = 0 }

putNext : Smoother -> Float -> Smoother
putNext s i =
  let (v, fs1) = fir1 s.firState i
  in { s | firState = fs1, value = v / s.mag }

putSome : Smoother -> List Float -> Smoother
putSome s is =
  let (vs, fs1) = fir s.firState is
  in { s | firState = fs1, value = Maybe.withDefault s.value (Maybe.map (\x -> x / s.mag) (List.head vs)) }
