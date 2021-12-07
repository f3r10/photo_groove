module PhotoFolders exposing (main)

import Browser
import Css
import Css.Global
import Dict exposing (Dict)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Html.Styled.Events as Events
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Tailwind.Utilities as Tw


type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos : Dict String Photo
    }


type alias Photo =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos = Dict.empty
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = "http://elm-in-action.com/folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
        }
    )


modelDecoder : Decoder Model
modelDecoder =
    Decode.succeed
        { selectedPhotoUrl = Just "trevi"
        , photos =
            Dict.fromList
                [ ( "trevi"
                  , { title = "Trevi"
                    , relatedUrls = [ "coli", "fresco" ]
                    , size = 34
                    , url = "trevi"
                    }
                  )
                , ( "fresco"
                  , { title = "Fresco"
                    , relatedUrls = [ "trevi" ]
                    , size = 46
                    , url = "fresco"
                    }
                  )
                , ( "coli"
                  , { title = "Coliseum"
                    , relatedUrls = [ "trevi", "fresco" ]
                    , size = 36
                    , url = "coli"
                    }
                  )
                ]
        }


type Msg
    = ClickedPhoto String
    | GotInitialModel (Result Http.Error Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        GotInitialModel (Ok newModel) ->
            ( newModel, Cmd.none )

        GotInitialModel (Err _) ->
            ( model, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    let
        photoByUrl : String -> Maybe Photo
        photoByUrl url =
            Dict.get url model.photos

        selectedPhoto : Html Msg
        selectedPhoto =
            case Maybe.andThen photoByUrl model.selectedPhotoUrl of
                Just photo ->
                    viewSelectedPhoto photo

                Nothing ->
                    text ""
    in
    div [ css [ Tw.flex, Tw.items_center ] ] <|
        [ div [ css [] ] [ selectedPhoto ] ]


viewSelectedPhoto : Photo -> Html Msg
viewSelectedPhoto photo =
    div [ css [] ] <|
        [ h2 [] [ text photo.title ]
        , img [ src (urlPrefix ++ "photos/" ++ photo.url ++ "/full") ] []
        , span [] [ text (String.fromInt photo.size ++ "KB") ]
        , h3 [] [ text "Related" ]
        , div [ css [] ] (List.map viewRelatedPhoto photo.relatedUrls)
        ]


viewRelatedPhoto : String -> Html Msg
viewRelatedPhoto url =
    img
        [ css []
        , Events.onClick (ClickedPhoto url)
        , src (urlPrefix ++ "photos/" ++ url ++ "/thumb")
        ]
        []


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }
