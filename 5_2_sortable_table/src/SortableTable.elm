module SortableTable exposing (Model, Msg, init, update, view)

import Html exposing (Html, li, text, ul)
import Html.Events exposing (onClick)


{-| テーブルの状態
-}
type alias Model =
    { sortColumn : Maybe String
    , reversed : Bool
    }



{- テーブルの状態を更新するメッセージ -}


type Msg
    = ClickColumn String



{- テーブルの状態を初期化する -}


init : Model
init =
    Model Nothing False



{- テーブルの状態を更新する -}


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickColumn columnName ->
            { model
                | sortColumn = Just columnName
                , reversed =
                    if Maybe.withDefault "" model.sortColumn == columnName then
                        not model.reversed

                    else
                        False
            }



{- テーブルを描画する -}


view : (Model -> msg) -> Model -> List String -> Html msg
view toMsg model items =
    Html.map (\msg -> toMsg (update msg model)) <|
        viewHelp model items


viewHelp : Model -> List String -> Html Msg
viewHelp model items =
    ul [] <|
        List.append
            viewHeader
            (viewItems model items)


viewHeader : List (Html Msg)
viewHeader =
    [ li [ onClick <| ClickColumn "book name" ] [ text "book name" ] ]


viewItems : Model -> List String -> List (Html Msg)
viewItems model items =
    items
        |> sortItems model
        |> List.map (\item -> li [] [ text item ])


sortItems : Model -> List String -> List String
sortItems model items =
    if model.reversed then
        List.reverse items

    else
        items
