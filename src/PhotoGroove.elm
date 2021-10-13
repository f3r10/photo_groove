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


type ThumnailSize
    = Small
    | Medium
    | Large

type alias Photo = {url : String}
type alias Model =  { photos : List Photo, selectImg : String, chosenSize: ThumnailSize }
type alias Msg = {description: String, data: String}

initialModel : Model 
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectImg = "1.jpeg"
    , chosenSize = Large
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
            [onClick {description = "ClickedSurpireseMe", data = ""}]
            [text "Surprise Me!"]
        , h3 [] [text "Thumnail Size:"]
        -- verbose code refactored to a Map
        -- , div [id "choosen-size"] [viewSizeChooser Small, viewSizeChooser Medium, viewSizeChooser Large]
        , div [id "choosen-size"] (List.map viewSizeChooser [Small, Medium, Large])
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

sizeToClass : ThumnailSize -> String
sizeToClass size =
    sizeToString size


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
        , onClick { description = "ClickPhoto", data = thumb.url }
        ]
        []

viewSizeChooser : ThumnailSize -> Html Msg
viewSizeChooser size =
    label []
    [ input [type_ "radio", name "size"] []
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
    -- Refactored to a match case expression
    -- if msg.description == "ClickPhoto" then
    --     { model | selectImg = msg.data }
    -- else if msg.description == "ClickedSurpireseMe" then
    --     {model | selectImg = "2.jpeg"}
    -- else
    --     Model
    case msg.description of
        "ClickPhoto" -> 
            { model | selectImg = msg.data }
        "ClickedSurpireseMe" -> 
            {model | selectImg = "2.jpeg"}
        _ -> model


main : Program () Model Msg
main =
    Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }
    
