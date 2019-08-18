module Page.User exposing (Model, Msg, init, update, view)

import Github exposing (Issue, Repo)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Url.Builder



-- MODEL


type alias Model =
    { userName : String
    , state : State
    }


type State
    = Init
    | Loaded (List Repo)
    | Error Http.Error


init : String -> ( Model, Cmd Msg )
init userName =
    -- ページ初期化
    ( Model userName Init
    , Github.getRepos GotRepos userName
    )



-- UPDATE


type Msg
    = GotRepos (Result Http.Error (List Repo))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRepos (Ok issues) ->
            ( { model | state = Loaded issues }, Cmd.none )

        GotRepos (Err err) ->
            ( { model | state = Error err }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.state of
        Init ->
            text "Loading..."

        Loaded repos ->
            viewRepo repos

        Error e ->
            text (Debug.toString e)


viewRepo : List Repo -> Html msg
viewRepo repos =
    ul []
        (repos
            |> List.map
                (\repo ->
                    viewLink (Url.Builder.absolute [ repo.owner, repo.name ] [])
                )
        )


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
