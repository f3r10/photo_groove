module PhotoGroove exposing (main)

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser


initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectImg = "1.jpeg"
    }


urlPrefix =
    "http://elm-in-action.com/"


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            -- (List.map (\url -> viewThumbnail model.selectImg url) model.photos)
            -- becuase of currying and partial application
            (List.map (viewThumbnail model.selectImg) model.photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectImg)
            ]
            []
        ]


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


update msg model =
    if msg.description == "ClickPhoto" then
        { model | selectImg = msg.data }

    else
        model


main =
    Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }
    
