module SortableTable exposing (Model, Msg, init, update, view)

import Html exposing (Html, li, text, ul)
import Html.Events exposing (onClick)


{-| テーブルの状態
-}
type Model
    = Model Internal


type alias Internal =
    { sortColumn : Maybe String
    , reversed : Bool
    }



{- テーブルの状態を更新するメッセージ -}


type Msg
    = ClickColumn String



{- テーブルの状態を初期化する -}


init : Model
init =
    Model <| Internal Nothing False



{- テーブルの状態を更新する -}


update : Msg -> Model -> Model
update msg (Model internal) =
    case msg of
        ClickColumn columnName ->
            Model
                { internal
                    | sortColumn = Just columnName
                    , reversed =
                        if Maybe.withDefault "" internal.sortColumn == columnName then
                            not internal.reversed

                        else
                            False
                }



{- テーブルを描画する -}


view : (Model -> msg) -> Model -> List String -> Html msg
view toMsg (Model internal) items =
    Html.map (\msg -> toMsg (update msg (Model internal))) <|
        viewHelp internal items


viewHelp : Internal -> List String -> Html Msg
viewHelp internal items =
    ul [] <|
        List.append
            viewHeader
            (viewItems internal items)


viewHeader : List (Html Msg)
viewHeader =
    [ li [ onClick <| ClickColumn "book name" ] [ text "book name" ] ]


viewItems : Internal -> List String -> List (Html Msg)
viewItems internal items =
    items
        |> sortItems internal
        |> List.map (\item -> li [] [ text item ])


sortItems : Internal -> List String -> List String
sortItems internal items =
    if internal.reversed then
        List.reverse items

    else
        items
