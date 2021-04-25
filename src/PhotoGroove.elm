module PhotoGroove exposing (main)

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)
import List

urlPrefix = "http://elm-in-action.com/"

view model =
     div [ class "content"]
         [ h1 [ ] [text "Photo Groove"]
         , div [ id "thumbnails" ] (List.map showThumbnails model)
         ]


showThumbnails imgRecord = img [src (urlPrefix ++ imgRecord.url)] []

initialModel = [
            {url = "1.jpeg"}
            , {url = "2.jpeg"}
            , {url = "3.jpeg"}
            ]

main =
    view initialModel