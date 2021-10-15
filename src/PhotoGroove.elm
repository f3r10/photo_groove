module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick)
import Random


type ThumnailSize
    = Small
    | Medium
    | Large


type alias Photo =
    { url : String }


type alias Size =
    { size : ThumnailSize, selected : Bool }


type alias Model =
    { photos : List Photo
    , chosenSize : ThumnailSize
    , sizes : List Size
    , selectedUrl : String
    }


type Msg
    = ClickedPhoto String
    | ClickedSize Size
    | ClickedSurpireseMe
    | GotSelectedIndex Int


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , chosenSize = Medium
    , sizes =
        [ { size = Small, selected = False }
        , { size = Medium, selected = True }
        , { size = Large, selected = False }
        ]
    , selectedUrl = "1.jpeg"
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick ClickedSurpireseMe ]
            [ text "Surprise Me!" ]
        , h3 [] [ text "Thumnail Size:" ]
        , div [ id "choosen-size" ]
            (List.map viewSizeChooser model.sizes)
        , div [ id "thumbnails", class (sizeToClass model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , div [] (List.map printTuple model.sizes)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
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


getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            photo.url

        Nothing ->
            ""



-- TODO check appendix B for more information about elm packages


randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)


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
            ( { model | selectedUrl = url }, Cmd.none )

        ClickedSurpireseMe ->
            ( model, Random.generate GotSelectedIndex randomPhotoPicker )

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

        GotSelectedIndex index ->
            ( { model | selectedUrl = getPhotoUrl index }, Cmd.none )



-- () means no flags whatsoever. What flags will do?


main : Program () Model Msg
main =
    Browser.element
        -- The second parameter, the command, will run when the page loads
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
