module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { input : String
    , userState : UserState
    }


type UserState
    = Init
    | Waiting
    | Loaded User
    | Failed Http.Error


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" Init
    , Cmd.none
    )



-- UPDATE


type Msg
    = Input String
    | Send
    | Receive (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input newInput ->
            ( { model | input = newInput }, Cmd.none )

        Send ->
            ( { model
                | input = ""
                , userState = Waiting
              }
            , Http.request
                { method = "GET"
                , headers = []
                , url = "http://api.github.com/users/" ++ model.input
                , body = Http.emptyBody
                , expect = expectJson Receive userDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        Receive (Ok user) ->
            ( { model | userState = Loaded user }, Cmd.none )

        Receive (Err e) ->
            ( { model | userState = Failed e }, Cmd.none )


expectJson : (Result Http.Error a -> msg) -> D.Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case D.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err
                                (Http.BadBody <| D.errorToString err)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit Send ]
            [ input
                [ onInput Input
                , autofocus True
                , placeholder "Github name"
                , Html.Attributes.value model.input
                ]
                []
            , button
                [ disabled
                    ((model.userState == Waiting)
                        || String.isEmpty (String.trim model.input)
                    )
                ]
                [ text "Submit" ]
            ]
        , case model.userState of
            Init ->
                text ""

            Waiting ->
                text "Waiting..."

            Loaded user ->
                a
                    [ href user.htmlUrl
                    , target "_blank"
                    ]
                    [ img [ src user.avatarUrl, width 200 ] []
                    , div [] [ text user.name ]
                    , div []
                        [ case user.bio of
                            Just bio ->
                                text bio

                            Nothing ->
                                text ""
                        ]
                    ]

            Failed error ->
                div []
                    [ text <|
                        case error of
                            Http.BadUrl msg ->
                                msg

                            Http.Timeout ->
                                "タイムアウトしました"

                            Http.NetworkError ->
                                "ネットワークエラーです"

                            Http.BadStatus num ->
                                "ステータス異常です : " ++ String.fromInt num

                            Http.BadBody msg ->
                                "なんかコンテンツがおかしい : " ++ msg
                    ]
        ]



-- DATA


type alias User =
    { login : String
    , avatarUrl : String
    , name : String
    , htmlUrl : String
    , bio : Maybe String
    }


userDecoder : Decoder User
userDecoder =
    D.map5 User
        (D.field "login" D.string)
        (D.field "avatar_url" D.string)
        (D.field "name" D.string)
        (D.field "html_url" D.string)
        (D.maybe <| D.field "bio" D.string)
