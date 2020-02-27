port module Main exposing (renderGraph)

import Browser
import Html exposing (Html, button, div, text, h1, h3, p, table, th, tr, td, thead, tbody, span, ul, li)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, id, class)
import List
import Json.Decode exposing (Decoder, map3, field, list, string, int, decodeString, Error)
import Data exposing (r14AgeGroupJson, r14BirthAgesJson)

type alias R14 =
    { age_group: String
    , count: Int
    , percent: Int }

type alias Model = { r14s: List R14 }

init : () -> (Model, Cmd Msg)
init _ = 
  case decodeJson of
    Ok r14List -> ({ r14s = r14List }, renderGraph r14BirthAgesJson)
    Err _ -> ({ r14s = [] }, Cmd.none)

type Msg = Loading | Success | Fail | RenderGraph

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Loading ->
      ({r14s = []}, Cmd.none)
    Success -> --TODO: Load JSON from url endpoint
      ({r14s = []}, Cmd.none)
    Fail ->
      ({r14s = []}, Cmd.none)
    RenderGraph ->
      (model, renderGraph r14BirthAgesJson)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

r14Decoder : Decoder (List R14)
r14Decoder =
  list (map3 R14
    (field "age_group" string)
    (field "count" int)
    (field "percent" int))

decodeJson : Result Error (List R14)
decodeJson =
  decodeString r14Decoder r14AgeGroupJson


renderTableRow : R14 -> Html Msg
renderTableRow r14 =
    let
      maybeBold = if r14.percent > 7 
        then style "font-weight" "bold" 
        else style "" ""

      percent = if r14.percent > 0 
        then r14.percent |> String.fromInt |> text 
        else "<1%" |> text
    in
      tr [] [ 
        td [ maybeBold
          , style "text-align" "left" ] [ r14.age_group |> text ]
        , td [ maybeBold
          , style "text-align" "right" ] [ percent ]
        , td [ style "text-align" "right" ] [ r14.count |> String.fromInt |> text ]
      ]

renderTable : Html Msg
renderTable =
  case decodeJson of
    Ok r14List ->
      div [ class "card bg-light px-3", id "main" ] [
        table [ class "table table-condensed table-extra-condensed"
        , style "clear" "both" 
        , style "width" "100%"
        , style "margin-top" "30px" ] [
          thead [] [
            tr [] [
              th [ style "width" "120px"
              , style "text-align" "left" ] [ text "Åldersgrupp" ]
              , th [ style "width" "120px"
              , style "text-align" "right"] [ text "Procent" ]
              , th [ style "width" "120px"
              , style "text-align" "right"] [ text "Antal" ]
            ]
          ]
          , tbody [] (List.map renderTableRow r14List)
        ]
      ]
    
    Err _ -> div [] [
        p [] [ text "Unable to parse JSON." ]
      ]
      
view : Model -> Html Msg
view model =
  div [id "bootstrap-override"]
    [ div [ id "DynamicContent"
        , class "container" 
      ] [ 
        renderTable
      ]
    ]

main =
   Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

port renderGraph : String -> Cmd msg
