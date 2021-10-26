module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes as Attr exposing (src)
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGroove exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag, text)


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


noPhotoNoThumbnails : Test
noPhotoNoThumbnails =
    test "No thumbnails render when there are no photos to render." <|
        \_ ->
            PhotoGroove.initialModel
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)


thumbnailsWork : Test
thumbnailsWork =
    fuzz (Fuzz.intRange 1 5) "URLs render as thumbnails" <|
        \urlCount ->
            let
                urls : List String
                urls =
                    List.range 1 urlCount
                        |> List.map (\num -> String.fromInt num ++ ".png")

                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
            { initialModel
                | status =
                    PhotoGroove.Loaded
                        (List.map
                            PhotoGroove.photoFromUrl
                            urls
                        )
                        ""
            }
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Expect.all thumbnailChecks


thumbnailRendered : String -> Query.Single msg -> Expectation
thumbnailRendered url query =
    query
        |> Query.findAll
            [ tag "img"
            , attribute
                (Attr.src
                    (PhotoGroove.urlPrefix
                        ++ url
                    )
                )
            ]
        |> Query.count (Expect.atLeast 1)
