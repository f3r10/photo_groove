port module PhotoGroove exposing
    ( Model
    , Msg(..)
    , Photo
    , Status(..)
    , initialModel
    , main
    , photoDecoder
    , update
    , urlPrefix
    , view
    , photoFromUrl
    )

import Browser
import Css
import Css.Global
import Html.Styled as Html exposing (Html)
import Html.Styled.Events as Events
import Html.Styled.Attributes as Attr
-- import Html exposing (Attribute, Html, button, canvas, div, h1, h3, img, input, label, node, text)
-- import Html.Attributes as Attr exposing (checked, class, classList, id, max, name, src, title, type_)
-- import Html.Events exposing (on, onCheck, onClick)
import Http
import Json.Decode exposing (Decoder, Value, at, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Random
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw


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
    { url = url, size = 0, title = ""}



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
    Html.div [ Attr.css [Tw.bg_gray_50] ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model

            Loading ->
                []

            Errored errorMessage ->
                [ Html.text ("Error:" ++ errorMessage) ]


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
    [ Html.h1 [] [ Html.text "Photo Groove" ]
    , Html.button
        [ Events.onClick ClickedSurpireseMe ]
        [ Html.text "Surprise Me!" ]
    , Html.div [  ] [ Html.text model.activity ]
    , Html.div [  ]
        [ viewFilter SlidHue "Hue" model.hue
        , viewFilter SlidRipple "Ripple" model.ripple
        , viewFilter SlidNoise "Noise" model.noise
        ]
    , Html.h3 [] [ Html.text "Thumnail Size:" ]
    , Html.div [  ]
        (List.map viewSizeChooser model.sizes)
    , Html.div [  ]
        (List.map (viewThumbnail selectedUrl) photos)
    , Html.canvas [ Attr.id "main-canvas", Attr.class "large" ] []

    -- , div [] (List.map printTuple model.sizes)
    ]


printTuple : Size -> Html Msg
printTuple s =
    Html.p [] [ Html.text (sizeToString s.size), Html.text " -> ", Html.text (Debug.toString s.selected) ]


sizeToClass : ThumnailSize -> String
sizeToClass s =
    sizeToString s


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail isSelected thumb =
    Html.img
        [ Attr.src (urlPrefix ++ thumb.url)
        , Attr.title (thumb.title ++ "[" ++ String.fromInt thumb.size ++ " KB]")
        , Attr.classList
            [ ( "selected"
              , thumb.url
                    == isSelected
              )
            ]
        , Events.onClick (ClickedPhoto thumb.url)
        ]
        []


viewSizeChooser : Size -> Html Msg
viewSizeChooser size =
    Html.label []
        [ Html.input
            [ Attr.type_ "radio"
            , Attr.name "size"

            -- , onClick (ClickedSize size)
            , Events.onCheck (\t -> ClickedSize { size | selected = t })

            -- , checked (if size.size == chosenSize then True else False)
            , Attr.checked size.selected
            ]
            []
        , Html.text (sizeToString size.size)
        ]


rangeSlider : List (Html.Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    Html.node "range-slider" attributes children


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    Html.div [ Attr.class "filter-slider" ]
        [ Html.label [] [ Html.text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , Html.label [] [ Html.text (String.fromInt magnitude) ]
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


onSlide : (Int -> msg) -> Html.Attribute msg
onSlide toMsg =
    at [ "detail", "userSlidTo" ] int
        |> Json.Decode.map toMsg
        |> Events.on "slide"



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
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
