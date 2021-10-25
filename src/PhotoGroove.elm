port module PhotoGroove exposing
    ( Model
    , Msg(..)
    , initialModel
    , main
    , photoDecoder
    , update
    )

import Browser
import Html exposing (Attribute, Html, button, canvas, div, h1, h3, img, input, label, node, text)
import Html.Attributes as Attr exposing (checked, class, classList, id, max, name, src, title, type_)
import Html.Events exposing (on, onCheck, onClick)
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



-- this can go for ever
-- photoDecoder : Decoder Photo
-- photoDecoder =
--     map3
--         (\url size title -> {url = url, size = size, title = title})
--         (field "url" string)
--         (field "url" int)
--         (field "title" string)


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
    -- <| is the same as $ on haskell
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error:" ++ errorMessage) ]


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
    [ h1 [] [ text "Photo Groove" ]
    , button
        [ onClick ClickedSurpireseMe ]
        [ text "Surprise Me!" ]
    , div [ class "activity" ] [ text model.activity ]
    , div [ class "filters" ]
        [ viewFilter SlidHue "Hue" model.hue
        , viewFilter SlidRipple "Ripple" model.ripple
        , viewFilter SlidNoise "Noise" model.noise
        ]
    , h3 [] [ text "Thumnail Size:" ]
    , div [ id "choosen-size" ]
        (List.map viewSizeChooser model.sizes)
    , div [ id "thumbnails", class (sizeToClass model.chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , canvas [ id "main-canvas", class "large" ] []

    -- , div [] (List.map printTuple model.sizes)
    ]


printTuple : Size -> Html Msg
printTuple s =
    Html.p [] [ text (sizeToString s.size), text " -> ", text (Debug.toString s.selected) ]


sizeToClass : ThumnailSize -> String
sizeToClass s =
    sizeToString s


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail isSelected thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , title (thumb.title ++ "[" ++ String.fromInt thumb.size ++ " KB]")
        , classList
            [ ( "selected"
              , thumb.url
                    == isSelected
              )
            ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


viewSizeChooser : Size -> Html Msg
viewSizeChooser size =
    label []
        [ input
            [ type_ "radio"
            , name "size"

            -- , onClick (ClickedSize size)
            , onCheck (\t -> ClickedSize { size | selected = t })

            -- , checked (if size.size == chosenSize then True else False)
            , checked size.selected
            ]
            []
        , text (sizeToString size.size)
        ]


rangeSlider : List (Html.Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
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



-- let
--     detailUserSlidTo : Decoder Int
--     detailUserSlidTo =
--         at [ "detail", "userSlidTo" ] int
--     msgDecoder : Decoder msg
--     msgDecoder =
--         Json.Decode.map toMsg detailUserSlidTo
-- in
-- on "slide" msgDecoder


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



-- case Json.Decode.decodeValue string activity of
--     Ok data ->
--         ( { model | activity = data }, Cmd.none )
--     Err error ->
--         ( { model | status = Errored "Server error" }, Cmd.none )


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
