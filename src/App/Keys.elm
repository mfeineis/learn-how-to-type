module App.Keys exposing (RawKey(..), rollKey, subscriptions)

import Browser.Events
import Char
import Json.Decode
import Random


type RawKey
    = RawKey String


rollKey : (RawKey -> msg) -> Cmd msg
rollKey toMsg =
    Random.weighted
        (1, codeToKey 32)
        (List.range 33 126 |> List.map (\c -> ( 1, codeToKey c)))
            |> Random.generate toMsg


codeToKey : Int -> RawKey
codeToKey code =
    Char.fromCode code
        |> String.fromChar
        |> RawKey


subscriptions toKeyDown toKeyUp _ =
    Sub.batch
        [ downs toKeyDown
        , ups toKeyUp
        ]


-- Utilities


eventKeyDecoder : Json.Decode.Decoder RawKey
eventKeyDecoder =
    Json.Decode.field "key" (Json.Decode.string |> Json.Decode.map RawKey)


downs : (RawKey -> msg) -> Sub msg
downs toMsg =
    Browser.Events.onKeyDown (eventKeyDecoder |> Json.Decode.map toMsg)


ups : (RawKey -> msg) -> Sub msg
ups toMsg =
    Browser.Events.onKeyUp (eventKeyDecoder |> Json.Decode.map toMsg)

