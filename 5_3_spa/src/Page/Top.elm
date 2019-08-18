module Page.Top exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Url.Builder



-- MODEL


type alias Model =
    { users : List String
    }


init : ( Model, Cmd Msg )
init =
    ( Model [ "elm", "evancz" ], Cmd.none )



-- UPDATE


type Msg
    = Init


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    ul [] <|
        List.map
            (viewLink << buildUrl)
            model.users


buildUrl : String -> String
buildUrl userName =
    Url.Builder.absolute (List.singleton userName) []


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
