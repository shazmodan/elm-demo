module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, h2, p, table, th, tr, td, thead, tbody, span, ul, li)
import Html.Events exposing (onClick)
import List
import Json.Decode exposing (Decoder, map3, field, list, string, int, decodeString, Error)
import Data exposing (r14Json)

type alias R14 =
    { age_group: String
    , count: Int
    , percent: Int }

type alias Model = String

init : () -> (Model, Cmd Msg)
init _ =
   ("We are live!", Cmd.none)

type Msg = Live | Down

update : Msg -> Model -> (Model, Cmd Msg)
update msg _ =
  case msg of
    Live ->
      ("We are live!", Cmd.none)
    Down ->
      ("We are down.", Cmd.none)

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
  decodeString r14Decoder r14Json


renderTableRow : R14 -> Html Msg
renderTableRow r14 =
    tr [] [ 
        td [] [ r14.age_group |> text ]
        , td [] [ r14.percent |> String.fromInt |> text ]
        , td [] [ r14.count |> String.fromInt |> text ]
     ]

renderTable : Html Msg
renderTable =
  case decodeJson of
    Ok r14List -> 
      div [] [
        table [] [
          thead [] [
            tr [] [
              th [] [ text "Åldersgrupp" ]
              , th [] [ text "Procent" ]
              , th [] [ text "Antal" ]
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
  div []
    [ 
      h2 [] [ text model ]
    , renderTable
    ]

main =
   Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
