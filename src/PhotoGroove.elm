module PhotoGroove exposing (main)

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Array exposing (Array)
import Html exposing (Html)
import Html exposing (button)
import Html exposing (label)
import Html exposing (input)
import Html exposing (h3)
import Html.Events exposing (onCheck)


type ThumnailSize
    = Small
    | Medium
    | Large

type alias Photo = {url : String}
type alias Size = {size: ThumnailSize, selected: Bool}
type alias Model =  
    { photos : List Photo
    , selectImg : String
    , chosenSize: ThumnailSize
    , sizes: List Size}
type Msg 
    = ClickedPhoto String
    | ClickedSize Size
    | ClickedSurpireseMe

initialModel : Model 
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
        , selectImg = "1.jpeg"
        , chosenSize = Medium
        , sizes = 
            [{size = Small, selected = False}
            , {size = Medium, selected = True}
            , {size = Large, selected = False}
            ]
    }

photoArray: Array Photo
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
            [onClick ClickedSurpireseMe]
            [text "Surprise Me!"]
        , h3 [] [text "Thumnail Size:"]
        , div [id "choosen-size"] 
            (List.map viewSizeChooser  model.sizes)
        , div [ id "thumbnails", class (sizeToClass model.chosenSize) ]
            (List.map (viewThumbnail model.selectImg) model.photos)
        , div [] (List.map printTuple model.sizes)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectImg)
            ]
            []
        ]

printTuple : Size -> Html Msg
printTuple s =
    Html.p [] [text (sizeToString s.size), text " -> ", text (Debug.toString s.selected)]

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
        [type_ "radio"
        , name "size"
        -- , onClick (ClickedSize size)
        , onCheck (\t -> ClickedSize {size | selected = t})
        -- , checked (if size.size == chosenSize then True else False)
        , checked size.selected] []
    , text (sizeToString size.size)
    ]

getPhotoUrl : Int -> String
getPhotoUrl index = 
    case Array.get index photoArray of
        Just photo -> photo.url
        Nothing -> ""


sizeToString : ThumnailSize -> String
sizeToString size =
    case size of
        Small -> "small"
        Medium -> "med"
        Large -> "large"



update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPhoto url -> 
            { model | selectImg = url }
        ClickedSurpireseMe -> 
            {model | selectImg = "2.jpeg"}
        ClickedSize size -> 
            let updateSizes =
                    List.map 
                        (\s -> 
                            if s.size == size.size then size  else {size = s.size,
                            selected = False}
                        ) model.sizes
            in 
            {model |  chosenSize = size.size, sizes = updateSizes}


main : Program () Model Msg
main =
    Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }
    
