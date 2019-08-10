port module Main exposing(..)

import Time

-- Elm から Javascript 側に数値を送信するための関数

port tick : Int -> Cmd msg

main =
    Platform.worker
        { init = \_ -> (1, Cmd.none)
        , update = \_ model -> (model + 1, tick model)
        , subscriptions = \_ -> Time.every 1000 (\_ -> ())
            }
