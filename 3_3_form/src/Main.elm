module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { input : String
    , memos : List String
    }


init : Model
init =
    { input = ""
    , memos = []
    }



-- UPDATE


type Msg
    = Input String
    | Submit
    | Delete Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model | input = input }

        Submit ->
            { model
                | input = ""
                , memos = model.input :: model.memos
            }

        Delete index ->
            { model
                | memos = removeMemo index model.memos
            }


removeMemo : Int -> List String -> List String
removeMemo index memos =
    List.take index memos ++ List.drop (index + 1) memos



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit Submit ]
            [ input [ value model.input, onInput Input ] []
            , button
                [ disabled (String.isEmpty <| String.trim model.input) ]
                [ text "Submit" ]
            ]
        , ul [] <| List.indexedMap viewMemo model.memos
        ]


viewMemo : Int -> String -> Html Msg
viewMemo index memo =
    li []
        [ span [] [ text memo ]
        , button [ onClick (Delete index) ] [ text "削除" ]
        ]
