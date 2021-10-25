module Server exposing (..)

import Url.Parser.Query
import Browser
import Http exposing (Response(..))
import Html exposing (..)
import Html.Events exposing (..)
import Browser.Navigation as Navigation
import Url
import Url.Builder as UrlBuilder
import Maybe as Maybe
import Html.Attributes exposing (..)
import Url.Parser

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        }

type alias Token = String

type alias Model = {
    token : Token,
    command : String,
    commandResponse : String,
    waiting : Bool
    }

type Msg = SendCmd String | SetCmd String | TokenRight Token| TokenWrong

init : () -> Url.Url -> Navigation.Key -> (Model, Cmd Msg)
init _ url _ =
    let token = Maybe.andThen (\x -> x) (Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string "token")) url)
    case token of
        Just t ->
            
