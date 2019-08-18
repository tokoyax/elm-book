module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Github exposing (Issue, Repo)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Repo
import Page.Top
import Page.User
import Route exposing (Route)
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound
    | ErrorPage Http.Error
    | TopPage Page.Top.Model
    | UserPage Page.User.Model
    | RepoPage Page.Repo.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    Model key (TopPage <| Page.Top.init)
        |> goTo (Route.parse url)



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | TopMsg Page.Top.Msg
    | RepoMsg Page.Repo.Msg
    | UserMsg Page.User.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TopPage topMsg ->
            case model.page of
                TopPage topModel ->
                    let
                        ( newTopModel, topCmd ) =
                            Page.Top.update topMsg topModel
                    in
                    ( { model | page = TopPage newTopModel }
                    , Cmd.map TopMsg topCmd
                    )

        UserPage userMsg ->
            case model.page of
                UserPage userModel ->
                    let
                        ( newUserModel, userCmd ) =
                            Page.User.update userMsg userModel
                    in
                    ( { model | page = UserPage newUserModel }
                    , Cmd.map UserMsg userCmd
                    )

        RepoPage repoMsg ->
            case model.page of
                RepoPage repoModel ->
                    let
                        ( newRepoModel, repoCmd ) =
                            Page.Repo.update repoMsg repoModel
                    in
                    ( { model | page = RepoPage newRepoModel }
                    , Cmd.map RepoMsg repoCmd
                    )

        _ ->
            ( model, Cmd.none )


{-| パスに応じて各ページを初期化する
-}
goTo : Maybe Route -> Model -> ( Model, Cmd Msg )
goTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Route.Top ->
            let
                ( topModel, topCmd ) =
                    Page.Top.init
            in
            ( { model | page = TopPage topModel }
            , Cmd.map TopMsg topCmd
            )

        Just (Route.User userName) ->
            let
                ( userModel, userCmd ) =
                    Page.User.init userName
            in
            ( { model | page = UserPage userModel }
            , Cmd.map UserMsg userCmd
            )

        Just (Route.Repo userName projectName) ->
            let
                ( repoModel, repoCmd ) =
                    Page.Repo.init userName projectName
            in
            ( { model | page = RepoPage repoModel }
            , Cmd.map RepoMsg repoCmd
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "My Github Viewer"
    , body =
        [ a [ href "/" ] [ h1 [] [ text "My Github Viewer" ] ]

        -- 場合分けしてページ表示
        , case model.page of
            NotFound ->
                viewNotFound

            ErrorPage error ->
                viewError error

            TopPage topPageModel ->
                Page.Top.view topPageModel
                    |> Html.map TopMsg

            UserPage userPageModel ->
                Page.User.view userPageModel
                    |> Html.map UserMsg

            RepoPage repoPageModel ->
                Page.Repo.view repoPageModel
                    |> Html.map RepoMsg
        ]
    }


{-| NotFound page
-}
viewNotFound : Html msg
viewNotFound =
    text "not found"


{-| Error page
-}
viewError : Http.Error -> Html msg
viewError error =
    case error of
        Http.BadBody message ->
            pre [] [ text message ]

        _ ->
            text (Debug.toString error)
