module PhotoGroove exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick)
import Http
import Random


type ThumnailSize
    = Small
    | Medium
    | Large


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Photo =
    { url : String }


type alias Size =
    { size : ThumnailSize, selected : Bool }


type alias Model =
    { status : Status
    , chosenSize : ThumnailSize
    , sizes : List Size
    }


type Msg
    = ClickedPhoto String
    | ClickedSize Size
    | ClickedSurpireseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error String)


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    , sizes =
        [ { size = Small, selected = False }
        , { size = Medium, selected = True }
        , { size = Large, selected = False }
        ]
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
                viewLoaded photos selectedUrl model.chosenSize model.sizes

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error:" ++ errorMessage) ]


viewLoaded : List Photo -> String -> ThumnailSize -> List Size -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize sizes =
    [ h1 [] [ text "Photo Groove" ]
    , button
        [ onClick ClickedSurpireseMe ]
        [ text "Surprise Me!" ]
    , h3 [] [ text "Thumnail Size:" ]
    , div [ id "choosen-size" ]
        (List.map viewSizeChooser sizes)
    , div [ id "thumbnails", class (sizeToClass chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , div [] (List.map printTuple sizes)
    , img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ selectedUrl)
        ]
        []
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


sizeToString : ThumnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

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
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )

        GotPhotos (Ok responseStr) ->
            case String.split "," responseStr of
                (firstUrl :: _) as urls ->
                    let
                        photos =
                            List.map (\url -> Photo url) urls
                    in
                    ( { model | status = Loaded photos firstUrl }, Cmd.none )
                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )
        GotPhotos (Err httpError) -> 
            ( { model | status = Errored "Server error" }, Cmd.none )


initialCmd: Cmd Msg
initialCmd = 
    Http.get
        {
            url = "http://elm-in-action.com/photos/list",
            expect = Http.expectString GotPhotos -- (\result -> GotPhotos result)
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



-- () means no flags whatsoever. What flags will do?


main : Program () Model Msg
main =
    Browser.element
        -- The second parameter, the command, will run when the page loads
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
