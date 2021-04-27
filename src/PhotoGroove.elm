module PhotoGroove exposing (main)

import Html exposing (Html, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (..)
import List
import Html.Events exposing (onClick)
import Browser

urlPrefix: String
urlPrefix = "http://elm-in-action.com/"

type alias Photo = { url: String }

type  Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize

type ThumbnailSize = Small | Medium | Large

sizeToString: ThumbnailSize -> String
sizeToString size =
    case size of
        Small -> "small"
        Medium -> "med"
        Large -> "large"



viewSizeChooser: ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [type_ "radio", name "size", onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]


type alias Model =
    { photos : List Photo
    , selectedUrl : String
    , chosenSize : ThumbnailSize}


view: Model -> Html Msg
view model =
     div [ class "content"]
         [ h1 [ ] [text "Photo Groove"]
         , h3 [] [text "Thumbnail size"]
         , div [ id "choose-size"] (List.map viewSizeChooser [Small, Medium, Large])
         , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
                (List.map (viewThumbnail model.selectedUrl) model.photos)
         , img
            [
            class "large"
            , src (urlPrefix ++ model.selectedUrl)
            ]
            []
         ]

update: Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPhoto url ->
            { model | selectedUrl = url }

        ClickedSize size ->
            { model | chosenSize = size }


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
        photos = [
            {url = "1.jpeg"}
            , {url = "2.jpeg"}
            , {url = "3.jpeg"}
        ]
        , selectedUrl = "1.jpeg"
        , chosenSize = Medium
    }

main =
    Browser.sandbox
    {init = initialModel
    , view = view
    , update = update
    }