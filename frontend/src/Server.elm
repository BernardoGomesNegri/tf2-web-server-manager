module Server exposing (..)

import Browser.Navigation
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
import Http exposing (expectString)
import Result exposing (..)

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

type AppError = None | NoCmd

type alias Model = {
    key : Browser.Navigation.Key,
    token : Maybe Token,
    command : String,
    commandResponse : String,
    waiting : Bool,
    error : AppError
    }

type Msg = SendCmd | SetCmd String | TokenRight | TokenWrong | ChangePage Browser.UrlRequest | ChangeUrl Url.Url | CmdGood String | CmdBad

init : () -> Url.Url -> Navigation.Key -> (Model, Cmd Msg)
init _ url k =
    let token = Maybe.andThen (\x -> x) (Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string "token")) url) in
    case token of
        Just t ->
            (Model k (Just t) "" "" False None, Http.get ({url = UrlBuilder.absolute ["api", "validate", t] [], expect = expectString parseToken}))
        Nothing ->
            (Model k Nothing "" "" False None, Cmd.map (\_ -> TokenWrong) Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SendCmd ->
            case model.token of
                Just t ->
                    ({model | waiting = True}, Http.post ({url = UrlBuilder.absolute ["api", "runcmd", model.command] [UrlBuilder.string "token" t], body = Http.emptyBody,
                        expect = expectString parseCmdResponse}))
                Nothing -> wrapModel {model | error = NoCmd}
        SetCmd s -> wrapModel {model | command = s}
        TokenRight -> (model, Cmd.none)
        TokenWrong -> (model, Browser.Navigation.load (UrlBuilder.absolute ["/"] []))
        ChangePage req -> (model, (Browser.Navigation.load (reqToString req)))
        ChangeUrl u -> (model, (Browser.Navigation.pushUrl model.key (Url.toString u)))
        CmdGood str -> wrapModel {model | commandResponse = str}
        CmdBad -> wrapModel {model | error = NoCmd}


parseCmdResponse : Result Http.Error String -> Msg
parseCmdResponse res =
    case res of
        Ok r -> CmdGood r
        Err _ -> CmdBad


parseToken : Result Http.Error String -> Msg
parseToken res =
    case res of
        Err _ -> TokenWrong
        Ok s ->
            if s == "1" then TokenRight else TokenRight

reqToString req =
    case req of
        Browser.Internal u -> Url.toString u
        Browser.External e -> e

wrapModel m = (m, Cmd.none)
onUrlRequest = ChangePage
onUrlChange = ChangeUrl

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

nl = br [] []

view : Model -> Browser.Document Msg
view model =
    {title = "TF2 server manager",
    body = [
        label [for "cmdinput"] [text "Input your command"], 
        input [type_ "text", onInput SetCmd, id "cmdinput"] [], nl,
        text model.commandResponse, nl,
        if model.waiting then
            text "Waiting for server response"
        else
            text ""
        ,nl,
        text (case model.error of
            None -> ""
            NoCmd -> "It was not possible to run your command."
            )
    ]}
