module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode
import Fuzz exposing (Fuzzer, int, list, string)
import PhotoGroove
import Test exposing (..)

decoderTest : Test
decoderTest =
    fuzz2 string int "title defaults to (untitle)" <|
        \url size ->
            [("url", Encode.string url)
            , ("size", Encode.int size )
            ]
            |> Encode.object
            |> decodeValue PhotoGroove.photoDecoder
            |> Result.map (\photo -> photo.title)
            |> Expect.equal
            (Ok "(untitled)") 

slidHueSetsHue : Test
slidHueSetsHue =
    fuzz int "SlidHue sets the hue" <|
        \amount ->
            PhotoGroove.initialModel
            |> PhotoGroove.update (PhotoGroove.SlidHue amount)
            |> Tuple.first
            |> .hue
            |> Expect.equal amount
