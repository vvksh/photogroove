module PhotoGroove exposing (main)

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)
import List
import Html.Events exposing (onClick)
import Browser

urlPrefix = "http://elm-in-action.com/"

view model =
     div [ class "content"]
         [ h1 [ ] [text "Photo Groove"]
         , div [ id "thumbnails" ]
                (List.map (showThumbnails model.selectedUrl) model.photos)
         , img
            [
            class "large"
            , src (urlPrefix ++ model.selectedUrl)
            ]
            []
         ]

update message model =
    if message.description == "ClickedPhoto" then
        {model | selectedUrl = message.data }
    else
        model


showThumbnails selectedUrl imgRecord =
    img
        [
        classList [("selected", selectedUrl == imgRecord.url)]
        , src (urlPrefix ++ imgRecord.url)
        , onClick {description = "ClickedPhoto", data = imgRecord.url}
        ]
        []

initialModel =
    {
        photos = [
            {url = "1.jpeg"}
            , {url = "2.jpeg"}
            , {url = "3.jpeg"}
        ]
        , selectedUrl = "1.jpeg"
    }

main =
    Browser.sandbox
    {init = initialModel
    , view = view
    , update = update
    }