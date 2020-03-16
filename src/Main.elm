port module Main exposing (renderGraph)

import Browser
import Html exposing (Html, a, div, p, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href, id, style)
import Http
import Json.Decode exposing (Decoder, field, int, list, map2, map3, string)
import Json.Encode as E
import List


type alias AgeGroup =
    { age_group : String
    , count : Int
    , percent : Int
    }


type alias BirthAge =
    { year : Int
    , count : Int
    }


type Model
    = Failure
    | Loading
    | Success (List AgeGroup)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, fetchAllData )


fetchAllData : Cmd Msg
fetchAllData =
    Cmd.batch
        [ fetchDataByAgeGroup
        , fetchDataByBirthAge
        ]


fetchDataByBirthAge : Cmd Msg
fetchDataByBirthAge =
    Http.get
        { url = "/FenixWeb/Data/R14/1137/"
        , expect = Http.expectJson LoadedBirthAges birthAgeDecoder
        }


fetchDataByAgeGroup : Cmd Msg
fetchDataByAgeGroup =
    Http.get
        { url = "/FenixWeb/Data/R14/1137/ByAgeGroup/"
        , expect = Http.expectJson LoadedAgeGroups ageGroupDecoder
        }


birthAgeDecoder : Decoder (List BirthAge)
birthAgeDecoder =
    list
        (map2 BirthAge
            (field "year" int)
            (field "count" int)
        )


ageGroupDecoder : Decoder (List AgeGroup)
ageGroupDecoder =
    list
        (map3 AgeGroup
            (field "age_group" string)
            (field "count" int)
            (field "percent" int)
        )


birthAgeEncoder : BirthAge -> E.Value
birthAgeEncoder birthAge =
    E.object
        [ ( "year", E.int birthAge.year )
        , ( "count", E.int birthAge.count )
        ]


birthAgesEncoder : List BirthAge -> E.Value
birthAgesEncoder r14List =
    E.list birthAgeEncoder r14List


type Msg
    = LoadedAgeGroups (Result Http.Error (List AgeGroup))
    | LoadedBirthAges (Result Http.Error (List BirthAge))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadedAgeGroups result ->
            case result of
                Ok r14AgeGroups ->
                    ( Success r14AgeGroups, fetchDataByBirthAge )

                Err _ ->
                    ( Failure, Cmd.none )

        LoadedBirthAges result ->
            case result of
                Ok r14BirthAges ->
                    ( model, renderGraph (birthAgesEncoder r14BirthAges) )

                Err _ ->
                    ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


renderTableRow : AgeGroup -> Html Msg
renderTableRow r14 =
    let
        maybeBold =
            if r14.percent > 7 then
                style "font-weight" "bold"

            else
                style "" ""

        percent =
            if r14.percent > 0 then
                (r14.percent |> String.fromInt) ++ "%" |> text

            else
                "<1%" |> text
    in
    tr []
        [ td
            [ maybeBold
            , style "text-align" "left"
            ]
            [ r14.age_group |> text ]
        , td
            [ maybeBold
            , style "text-align" "right"
            ]
            [ percent ]
        , td [ style "text-align" "right" ] [ r14.count |> String.fromInt |> text ]
        ]


renderTable : Model -> Html Msg
renderTable model =
    case model of
        Loading ->
            div []
                [ p [] [ text "Laddar data..." ]
                ]

        Failure ->
            div []
                [ p [] [ text "Unable to load data from server." ]
                ]

        Success ageGroups ->
            div []
                [ table
                    [ class "table table-condensed table-extra-condensed"
                    , style "clear" "both"
                    , style "width" "100%"
                    , style "margin-top" "30px"
                    ]
                    [ thead []
                        [ tr []
                            [ th
                                [ style "width" "120px"
                                , style "text-align" "left"
                                ]
                                [ text "Åldersgrupp" ]
                            , th
                                [ style "width" "120px"
                                , style "text-align" "right"
                                ]
                                []
                            , th
                                [ style "width" "120px"
                                , style "text-align" "right"
                                ]
                                [ text "Antal" ]
                            ]
                        ]
                    , tbody [] (List.map renderTableRow ageGroups)
                    ]
                ]


view : Model -> Html Msg
view model =
    div [ id "bootstrap-override" ]
        [ div []
            [ a [ class "pull-right btn btn-sm btn__svea--primary mb-4", href "../../Data/R14/1137/ByAgeGroup/?format=slk" ]
                [ span [] [ text "Hämta grupperad till Excel" ]
                ]
            ]
        , div []
            [ a [ class "pull-right btn btn-sm btn__svea--primary mb-4", href "../../Data/R14/1137/?format=slk" ]
                [ span [] [ text "Hämta till Excel" ]
                ]
            ]
        , renderTable model
        ]


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


port renderGraph : E.Value -> Cmd msg
