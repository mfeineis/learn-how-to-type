module App.Page exposing (GameState(..), Intent(..), view)

import App.Keys exposing (RawKey(..))
import Css exposing (..)
import Html.Styled as Html exposing (Attribute, Html, div, span, styled, text)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onClick)
import Time


type Intent
    = EndSession
    | StartSession


type GameState
    = Paused
    | Running
    | TimeboxElapsed


type KeystrokeRating
    = Positive
    | Negative
    | Neutral


view ({ state } as model) =
    { title = "Learn How to Type"
    , body =
        (case state of
            Paused ->
                mainMenu model

            Running ->
                game model

            TimeboxElapsed ->
                game model
        )
    }


mainMenu _ =
    [ button [ onClick StartSession ] [ text "Start New Session" ]
    ]


game { current, inputs, sessionStart, sessionTime, sessionTimebox, state, targets } =
    let
        lastTarget =
            case targets of
                last :: _ ->
                    last

                _ ->
                    RawKey "Escape"

        inputCount =
            List.length inputs

        correctCount =
            (List.foldl
                (\{ isMatch } acc ->
                    if isMatch then
                        acc + 1
                    else
                        acc
                )
                0
                inputs
            )
            |> toFloat

        elapsedSeconds =
            (Time.posixToMillis sessionTime) - (Time.posixToMillis sessionStart)
                |> \millis -> millis // 1000
                |> (+) 1

        elapsedMinutes =
            Basics.round (toFloat elapsedSeconds / 60.0)

        successPerMinute =
            Basics.round ((correctCount / toFloat elapsedSeconds) * 60.0)

        timebox =
            (Time.posixToMillis sessionTimebox) // 1000

        timeboxLeft =
            timebox - elapsedSeconds

    in
    [ styled div
        [ margin2 zero auto
        , width (em 40)
        ]
        []
        [ div []
            [ infoBox 
                ("Timebox " ++ String.fromInt timebox ++ "s")
                (String.fromInt timeboxLeft ++ "s - " ++ (String.fromInt elapsedSeconds) ++ "s elapsed")
            ]
        , div []
            [ infoBox "Accuracy"
                ((String.fromInt (Basics.round (100 * correctCount / max 1 (toFloat inputCount)))) ++ "%"
                    ++ " aka " ++ String.fromInt (Basics.round correctCount) ++ " of " ++ String.fromInt inputCount
                )
            , keyboardKey Neutral current
            ]
        , div []
            [ infoBox "Accurate Keystrokes Per Minute"
                (String.fromInt successPerMinute ++ " in " ++ String.fromInt elapsedMinutes ++ "min"
                )
            , case inputs of
                { input } :: _ ->
                    if input == lastTarget then
                        keyboardKey Positive input
                    else
                        keyboardKey Negative input

                _ ->
                    keyboardKey Neutral (RawKey " ")
            ]
        ]
    ]


-- Form stuff

button : List (Attribute msg) -> List (Html msg) -> Html msg
button =
    styled Html.button []


keyboardKey : KeystrokeRating -> RawKey -> Html msg
keyboardKey =
    keyboardKeyCustom [] (text "")


infoBox : String -> String -> Html msg
infoBox description label =
    keyboardKeyCustom
        [ minWidth (em 15)
        ]
        (styled div
            [ fontSize (pt 13)
            , left (em 0.5)
            , position absolute
            , top (em 0.25)
            ]
            []
            [ text description ]
        )
        Neutral
        (RawKey label)


keyboardKeyCustom : List Style -> Html msg -> KeystrokeRating -> RawKey -> Html msg
keyboardKeyCustom styles description rating (RawKey key) =
    let
        ( keyColor, bgColor ) =
            case rating of
                Positive ->
                    ( hex "fff", hex "0f0" )

                Neutral ->
                    ( hex "000", hex "fff" )

                Negative ->
                    ( hex "fff", hex "f00" )

        label =
            case key of
                " " ->
                    text "[___]"

                "" ->
                    text "[___]"

                _ ->
                    text key
    in
    styled div
        ([ backgroundColor bgColor
        , border3 (px 1) solid keyColor
        , borderRadius (em 0.4)
        , boxSizing borderBox
        , color keyColor
        , display inlineBlock
        , fontSize (pt 20)
        , fontWeight bold
        , margin4 (em 1) (em 1) zero zero
        , minWidth (em 3)
        , padding4 (em 1) (em 0.5) (em 0.5) (em 1.75)
        , position relative
        , textAlign right
        ] ++ styles)
        []
        [ label
        , description
        ]
