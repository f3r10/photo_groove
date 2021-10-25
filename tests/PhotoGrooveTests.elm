module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGroove
import Test exposing (..)


decoderTest : Test
decoderTest =
    fuzz2 string int "title defaults to (untitle)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
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


sliders : Test
sliders =
    describe "Sliders sets the desired field in the Model"
        [ testSlider "SlidHue" PhotoGroove.SlidHue .hue
        , testSlider "SlidRipple" PhotoGroove.SlidRipple .ripple
        , testSlider "SlidNoise" PhotoGroove.SlidNoise .noise
        ]


testSlider :
    String
    -> (Int -> PhotoGroove.Msg)
    -> (PhotoGroove.Model -> Int)
    -> Test
testSlider description toMsg amountFromModel =
    fuzz int description <|
        \amount ->
            PhotoGroove.initialModel
                |> PhotoGroove.update (toMsg amount)
                |> Tuple.first
                |> amountFromModel
                |> Expect.equal amount
