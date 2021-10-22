module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Json.Decode exposing (decodeString)
import Fuzz exposing (Fuzzer, int, list, string)
import PhotoGroove
import Test exposing (..)

decoderTest : Test
decoderTest =
    test "title defaults to (untitle)" 
        (\_ ->
            let
                jsonText = 
                    """
                    {"url": "fruits.com", "size": 5}
                    """
            in
            decodeString PhotoGroove.photoDecoder jsonText
            |> Result.map (\photo -> photo.title)
            |> Expect.equal
                (Ok "(untitled)")
            )
