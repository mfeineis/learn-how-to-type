module App exposing (main)

import App.Keys exposing (RawKey(..))
import App.Page exposing (GameState(..), Intent(..))
import Browser
import Html.Styled
import Task
import Time


type Msg
    = Intent Intent
    | KeyDowned RawKey
    | KeyUpped RawKey
    | KeyRolled RawKey
    | SessionStarted Time.Posix
    | TimeSliceElapsed Time.Posix


type alias Model =
    { state : GameState
    , inputs : List { input : RawKey, target : RawKey, isMatch : Bool }
    , targets : List RawKey
    , current : RawKey
    , sessionStart : Time.Posix
    , sessionTime : Time.Posix
    , sessionTimebox : Time.Posix
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Intent EndSession ->
            initialModel
                |> withCmds []

        Intent StartSession ->
            { initialModel | state = Running }
                |> withCmds
                    [ Task.perform SessionStarted Time.now
                    , rollKey
                    ]

        KeyDowned key ->
            case key of
                RawKey "Escape" ->
                    { model | state = Paused }
                        |> withCmds []

                RawKey "Shift" ->
                    model |> withCmds []

                RawKey "CapsLock" ->
                    model |> withCmds []

                RawKey "Control" ->
                    model |> withCmds []

                _ ->
                    if model.state == Running then
                        let
                            isMatch = key == model.current
                            cmds = if isMatch then [ rollKey ] else []
                            item = { input = key, target = model.current, isMatch = isMatch }
                        in
                        { model
                            | inputs = item :: model.inputs
                        }
                        |> withCmds cmds
                    else
                        model |> withCmds []

        KeyRolled key ->
            { model
                | current = key
                , targets = model.current :: model.targets
            }
            |> withCmds []

        KeyUpped _ ->
            model |> withCmds []

        SessionStarted posix ->
            { model | sessionStart = posix } |> withCmds []

        TimeSliceElapsed posix ->
            let
                elapsedMillis =
                    (Time.posixToMillis posix) - (Time.posixToMillis model.sessionStart)

                millisLeft =
                    (Time.posixToMillis model.sessionTimebox) - elapsedMillis
            in
            if model.state == Running && millisLeft >= 0 then
                { model | sessionTime = posix } |> withCmds []
            else
                { model | state = TimeboxElapsed } |> withCmds []


rollKey : Cmd Msg
rollKey =
    App.Keys.rollKey KeyRolled


initialModel : Model
initialModel =
    { state = Paused
    , inputs = []
    , targets = []
    , current = RawKey "Escape"
    , sessionStart = Time.millisToPosix 0
    , sessionTime = Time.millisToPosix 0
    , sessionTimebox = Time.millisToPosix (2 * 60 * 1000)
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.state == Running then
            Time.every 1000 TimeSliceElapsed
          else
            Sub.none
        , App.Keys.subscriptions KeyDowned KeyUpped model
        ]


-- Setup


type alias Flags =
    {}


init : Flags -> () -> () -> ( Model, Cmd Msg )
init flags url navKey =
    initialModel |> withCmds []


main : Program Flags Model Msg
main =
    Browser.element
        { init = \flags -> init flags () () 
        , subscriptions = subscriptions
        , update = update
        , view =
            \model ->
                let { body } = App.Page.view model
                in
                Html.Styled.div []
                    (List.map (Html.Styled.map Intent) body)
                    |> Html.Styled.toUnstyled
        }


-- Utilities


withCmds : List (Cmd msg) -> model -> ( model, Cmd msg )
withCmds cmds model =
    ( model, Cmd.batch cmds )

