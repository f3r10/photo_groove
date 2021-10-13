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
type alias Model =  
    { photos : List Photo
    , selectImg : String
    , chosenSize: (ThumnailSize, Bool) }
type Msg 
    = ClickedPhoto String
    | ClickedSize ThumnailSize Bool
    | ClickedSurpireseMe

initialModel : Model 
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectImg = "1.jpeg"
    , chosenSize = (Medium, True)
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
        -- verbose code refactored to a Map
        -- , div [id "choosen-size] [viewSizeChooser Small, viewSizeChooser Medium, viewSizeChooser Large]
        , div [id "choosen-size"] 
            (List.map 
                (\s ->  viewSizeChooser s model.chosenSize) [Small, Medium, Large]
            )
        , div [ id "thumbnails", class (sizeToClass model.chosenSize) ]
            -- (List.map (\url -> viewThumbnail model.selectImg url) model.photos)
            -- becuase of currying and partial application
            (List.map (viewThumbnail model.selectImg) model.photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectImg)
            ]
            []
        ]

sizeToClass : (ThumnailSize, Bool) -> String
sizeToClass size =
    let (s, _) = size 
    in
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

viewSizeChooser : ThumnailSize -> (ThumnailSize, Bool) -> Html Msg
viewSizeChooser size ( choosenSize, selected ) =
    label []
    [ input 
        [type_ "radio"
        , name "size"
        -- , onClick (ClickedSize size)
        , onCheck (\t -> ClickedSize size t)
        , checked (if size == choosenSize then selected else False )] []
    , text (sizeToString size)
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
        ClickedSize size b -> 
            {model |  chosenSize = (size, b)}


main : Program () Model Msg
main =
    Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }
    
