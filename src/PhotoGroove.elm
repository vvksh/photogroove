module PhotoGroove exposing (main)

import Html exposing (Html, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (class, classList, id, name, src, title, type_)
import List
import Html.Events exposing (onClick)
import Browser
import Http
import Json.Decode exposing (Decoder, bool, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)

urlPrefix: String
urlPrefix = "https://raw.githubusercontent.com/vvksh/photogroove/master/photos/"

type alias Photo =
    { url : String
     , size : Int
     , title : String
     }

type  Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | GotPhotos (Result Http.Error (List Photo))

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
     div [ class "content"]
         [ div [ id "choose-size"] (List.map viewSizeChooser [Small, Medium, Large])
          , div [ id "container"] <|
             case model.status of
                 Loading ->
                     []

                 Loaded photos selectedUrl ->
                     viewLoaded photos selectedUrl model.chosenSize

                 Errored errorMsg ->
                     [
                      h1 [ ] [text ("Got Error while loading" ++ errorMsg) ]
                     ]
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

        GotPhotos (Ok photos) ->
            case photos of
                firstPhoto :: _ ->
                      ({model | status = Loaded photos firstPhoto.url}, Cmd.none)
                [] ->
                    ( { model | status = Errored ("0 photos found")}, Cmd.none )

        GotPhotos (Err httpError) ->
             ( { model | status = Errored ("server error")}, Cmd.none )


viewLoaded: List Photo -> String -> ThumbnailSize-> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [
        div [ id "thumbnails", class (sizeToString chosenSize)] (List.map (viewThumbnail selectedUrl) photos)
        , img [ class "large", src (urlPrefix ++ selectedUrl)] []
    ]


viewThumbnail: String  -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [
        classList [("selected", selectedUrl == thumb.url)]
        , title (thumb.title ++ " [ " ++ String.fromInt thumb.size ++ " KB ]")
        , src (urlPrefix ++ thumb.url)
        , onClick (ClickedPhoto thumb.url)
        ]
        []

photoDecoder: Decoder Photo
photoDecoder =
    succeed Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "untitled"


initialModel: Model
initialModel =
    {
        status = Loading
        , chosenSize = Medium
    }

initialCmd: Cmd Msg
initialCmd =
    Http.get {
        url = "https://raw.githubusercontent.com/vvksh/photogroove/master/photos/list.json",
        expect = Http.expectJson GotPhotos (list photoDecoder)
    }

main : Program () Model Msg
main =
    Browser.element
    { init = \_ -> (initialModel, initialCmd)
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }