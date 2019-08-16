module Main exposing (main)

import Browser
import Html exposing (Html)
import SortableTable


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { books : List String
    , tableState : SortableTable.Model
    }


type Msg
    = UpdateTable SortableTable.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [ "a book", "b book", "c book" ] SortableTable.init
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        UpdateTable tableMsg ->
            ( { model
                | tableState =
                    SortableTable.update tableMsg model.tableState
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.map UpdateTable <|
        SortableTable.view
            (Debug.log "model.tableState" model.tableState)
            (Debug.log "model.books" model.books)
