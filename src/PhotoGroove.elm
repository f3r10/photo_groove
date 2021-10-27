port module PhotoGroove exposing
    ( Model
    , Msg(..)
    , Photo
    , Status(..)
    , initialModel
    , main
    , photoDecoder
    , photoFromUrl
    , update
    , urlPrefix
    , view
    )

import Browser
import Element exposing (Element, el, fill, layout, none, padding, px, rgb, rgb255, row, spacing, width)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Attribute, Html, canvas, node, text)
import Html.Attributes as Attr exposing (id, name)
import Html.Events exposing (on)
import Http
import Json.Decode exposing (Decoder, Value, at, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Random


port setFilters : FilterOptions -> Cmd msg


port activityChanges : (Value -> msg) -> Sub msg


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


type ThumnailSize
    = Small
    | Medium
    | Large


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


type alias Size =
    { size : ThumnailSize, selected : Bool }


type alias Model =
    { status : Status
    , activity : String
    , chosenSize : ThumnailSize
    , sizes : List Size
    , hue : Int
    , ripple : Int
    , noise : Int
    }


type Msg
    = ClickedPhoto String
    | ClickedSize Size
    | ClickedSurpireseMe
    | GotRandomPhoto Photo
    | GotActivity Value
    | GotPhotos (Result Http.Error (List Photo))
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int


initialModel : Model
initialModel =
    { status = Loading
    , activity = ""
    , chosenSize = Medium
    , sizes =
        [ { size = Small, selected = False }
        , { size = Medium, selected = True }
        , { size = Large, selected = False }
        ]
    , hue = 5
    , ripple = 5
    , noise = 5
    }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


view : Model -> Html Msg
view model =
    layout
        [ Bg.color <| rgb255 44 44 44
        , Font.color <| rgb255 250 250 250
        , Font.family [ Font.typeface "Verdana" ]
        ]
    <|
        Element.column
            [ width <| px 960

            -- , Element.padding 40
            , Element.height fill
            , Element.centerX
            , Element.centerY
            ]
        <|
            case model.status of
                Loaded photos selectedUrl ->
                    -- viewLoaded photos selectedUrl model
                    -- [el [] <| Element.text "Hello", el [] <| Element.text "world"]
                    viewLoaded photos selectedUrl model

                Loading ->
                    [ none ]

                Errored errorMessage ->
                    -- el [] [ text ("Error:" ++ errorMessage) ]
                    [ none ]


viewLoaded : List Photo -> String -> Model -> List (Element Msg)
viewLoaded photos selectedUrl model =
    [ Element.row
        [ Element.width Element.fill ]
        [ el [ Font.size 32, Font.color <| rgb255 96 181 204 ] <| Element.text "Photo Groove"
        , el [ Element.alignRight ] <| Element.text model.activity
        ]
    , Element.row
        [ Element.width Element.fill ]
        [ viewSizeChooser model
        , Element.row [ Element.alignRight ]
            [ Element.wrappedRow [ Element.width <| Element.px 318 ] <|
                [ viewFilter SlidHue "Hue" model.hue
                , viewFilter SlidRipple "Ripple" model.ripple
                , viewFilter SlidNoise "Noise" model.noise
                ]
            , el [] <|
                Input.button
                    [ Element.paddingXY 30 10
                    , Bg.color <| rgb255 96 181 204
                    , Font.color <| rgb255 44 44 44
                    , Font.size 24
                    , Border.width 0
                    ]
                    { onPress = Just ClickedSurpireseMe, label = Element.text "Surprise Me!" }
            ]
        ]
    , Element.row
        [ Element.height Element.fill, Element.width Element.fill ]
        [ Element.wrappedRow
            [ Element.alignTop
            , Element.spacingXY 10 10
            , Element.width <| Element.fillPortion 5
            ]
            (List.map (viewThumbnail selectedUrl model.chosenSize) photos)
        , Element.column
            [ Element.height Element.fill
            , Element.width <| Element.fillPortion 6
            ]
            [ el
                [ Element.alignRight, Element.width <| Element.px 500 ]
              <|
                Element.html <|
                    canvas [ id "main-canvas" ] []
            ]
        ]

    -- , div [] (List.map printTuple model.sizes)
    ]


printTuple : Size -> Html Msg
printTuple s =
    Html.p [] [ text (sizeToString s.size), text " -> ", text (Debug.toString s.selected) ]


sizeToClass : ThumnailSize -> String
sizeToClass s =
    sizeToString s


choosenSizeToPixel : ThumnailSize -> Int
choosenSizeToPixel size =
    case size of
        Small ->
            50

        Medium ->
            100

        Large ->
            200


selectedImageEffect : String -> String -> List (Element.Attribute msg)
selectedImageEffect url isSelected =
    if url == isSelected then
        [ Border.color <| rgb255 96 181 204
        , Border.solid
        , Border.width 6
        , Element.spacingXY 0 0
        ]

    else
        [ Border.color <| rgb255 255 255 255
        , Border.solid
        , Border.width 1
        ]


viewThumbnail : String -> ThumnailSize -> Photo -> Element Msg
viewThumbnail isSelected choosenSize thumb =
    Element.image
        ([ Element.width <| Element.px (choosenSizeToPixel choosenSize)
         , Events.onClick (ClickedPhoto thumb.url)
         ]
            ++ selectedImageEffect thumb.url isSelected
        )
        { src = urlPrefix ++ thumb.url
        , description =
            thumb.title ++ "[" ++ String.fromInt thumb.size ++ " KB]"
        }


viewSizeChooser : Model -> Element Msg
viewSizeChooser model =
    Input.radioRow
        [ Element.padding 10, Element.spacing 30 ]
        { onChange = \new -> ClickedSize { size = new, selected = True }
        , selected = Just model.chosenSize
        , label = Input.labelAbove [] (Element.text "Thumnail Size:")
        , options =
            List.map
                (\size ->
                    Input.option size.size
                        (Element.text <|
                            sizeToClass
                                size.size
                        )
                )
                model.sizes
        }


rangeSlider : List (Html.Attribute msg) -> List (Html msg) -> Element msg
rangeSlider attributes children =
    Element.html <| node "range-slider" attributes children


viewFilter : (Int -> Msg) -> String -> Int -> Element Msg
viewFilter toMsg name magnitude =
    Element.row []
        [ Element.el [] <| Element.text name
        , Element.el
            [ Element.width <| Element.px 120
            , Element.spacing 10
            ]
            (rangeSlider
                [ Attr.max "11"
                , Attr.property "val" (Encode.int magnitude)
                , onSlide toMsg
                ]
                []
            )
        ]


sizeToString : ThumnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    at [ "detail", "userSlidTo" ] int
        |> Json.Decode.map toMsg
        |> on "slide"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }

        ClickedSurpireseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored _ ->
                    ( model, Cmd.none )

        ClickedSize size ->
            let
                updateSizes =
                    List.map
                        (\s ->
                            if s.size == size.size then
                                size

                            else
                                { size = s.size
                                , selected = False
                                }
                        )
                        model.sizes
            in
            ( { model | chosenSize = size.size, sizes = updateSizes }, Cmd.none )

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        GotActivity activity ->
            case Json.Decode.decodeValue string activity of
                Ok data ->
                    ( { model | activity = data }, Cmd.none )

                Err error ->
                    ( { model | status = Errored "Server error" }, Cmd.none )

        GotPhotos (Ok responseList) ->
            case responseList of
                (firstPhoto :: _) as photos ->
                    applyFilters { model | status = Loaded photos firstPhoto.url }

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err httpError) ->
            ( { model | status = Errored "Server error" }, Cmd.none )

        SlidHue hue ->
            applyFilters { model | hue = hue }

        SlidRipple ripple ->
            applyFilters { model | ripple = ripple }

        SlidNoise noise ->
            applyFilters { model | noise = noise }


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded photos selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
            ( model, setFilters { url = url, filters = filters } )

        Loading ->
            ( model, Cmd.none )

        Errored errorMessage ->
            ( model, Cmd.none )


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (list photoDecoder)
        }


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored _ ->
            status


subscriptions : Model -> Sub Msg
subscriptions model =
    activityChanges GotActivity


init : Value -> ( Model, Cmd Msg )
init flags =
    case Json.Decode.decodeValue float flags of
        Ok data ->
            let
                activity =
                    "Initializing Pasta v" ++ String.fromFloat data
            in
            -- The second parameter, the command, will run when the page loads
            ( { initialModel | activity = activity }, initialCmd )

        Err error ->
            ( { initialModel | status = Errored "Server error" }, Cmd.none )


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
