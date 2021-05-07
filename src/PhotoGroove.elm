module PhotoGroove exposing (main)

import Html exposing (Html, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (..)
import List
import Html.Events exposing (onClick)
import Browser
import Http

urlPrefix: String
urlPrefix = "http://elm-in-action.com/"

type alias Photo = { url: String }

type  Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | GotPhotos (Result Http.Error String)

type ThumbnailSize = Small | Medium | Large

sizeToString: ThumbnailSize -> String
sizeToString size =
    case size of
        Small -> "small"
        Medium -> "med"
        Large -> "large"

type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


viewSizeChooser: ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [type_ "radio", name "size", onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]


type alias Model =
    { status: Status
    , chosenSize : ThumbnailSize}


view: Model -> Html Msg
view model =
     div [ class "content"] <|
         case model.status of
             Loading ->
                 []

             -- todo implement loaded fn
             Loaded _ _ ->
                 [
                 h1 [ ] [text "photos loaded, todo"]
                 ]

             Errored errorMsg ->
                 [
                  h1 [ ] [text ("Got Error while loading" ++ errorMsg) ]
                 ]


selectUrl: String -> Status -> Status
selectUrl url status =
    case status of
        Loading ->
            status

        Loaded photos _ ->
            Loaded photos url

        Errored errorMsg ->
            status


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedPhoto url ->
            ({ model | status = selectUrl url  model.status}, Cmd.none)

        ClickedSize size ->
            ({ model | chosenSize = size }, Cmd.none)

        GotPhotos result ->
            case result of
                Ok successString ->
                   ({model | status = Errored ("Got string," ++ successString)}, Cmd.none)

                Err httpError ->
                    ( { model | status = Errored ("server error")}, Cmd.none )


viewThumbnail: String -> Photo -> Html Msg
viewThumbnail selectedUrl photo =
    img
        [
        classList [("selected", selectedUrl == photo.url)]
        , src (urlPrefix ++ photo.url)
        , onClick (ClickedPhoto photo.url)
        ]
        []

initialModel: Model
initialModel =
    {
        status = Loading
        , chosenSize = Medium
    }

initialCmd: Cmd Msg
initialCmd =
    Http.get {
        url = "http://elm-in-action.com/photos/list",
        expect = Http.expectString GotPhotos
    }

main : Program () Model Msg
main =
    Browser.element
    { init = \_ -> (initialModel, initialCmd)
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }