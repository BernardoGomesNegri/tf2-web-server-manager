module Server exposing (..)

import Browser.Navigation
import Browser
import Http exposing (Response(..))
import Html exposing (..)
import Html.Events exposing (..)
import Browser.Navigation as Navigation
import Url
import Url.Builder as UrlBuilder
import Maybe as Maybe
import Html.Attributes exposing (..)
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

type AppError = None | NoCmd | TokenWrongErr

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
    let token = grabParam url "token" in
    case token of
        Just t ->
            (Model k (Just t) "" "" False None, Http.get ({url = UrlBuilder.absolute ["api", "validate", t] [], expect = expectString parseToken}))
        Nothing ->
            (Model k Nothing "" "" False None, Cmd.map (\_ -> TokenWrong) (Navigation.load (UrlBuilder.absolute [""] [])))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SendCmd ->
            case model.token of
                Just t ->
                    ({model | waiting = True}, Http.post ({url = UrlBuilder.absolute ["api", "runcmd", model.command] [UrlBuilder.string "token" t], body = Http.emptyBody,
                        expect = expectString parseCmdResponse}))
                Nothing -> wrapModel {model | error = TokenWrongErr}
        SetCmd s -> wrapModel {model | command = s}
        TokenRight -> (model, Cmd.none)
        TokenWrong -> wrapModel {model | error = TokenWrongErr}
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


view : Model -> Browser.Document Msg
view model =
    {title = "TF2 server manager",
    body = [
        label [for "cmdinput"] [text "Input your command"], nl,
        input [type_ "text", onInput SetCmd, id "cmdinput"] [], nl,
        button [onClick SendCmd] [text "Send command"],
        text model.commandResponse, nl,
        if model.waiting then
            text "Waiting for server response"
        else
            text ""
        ,nl,
        text (case model.error of
            None -> ""
            NoCmd -> "It was not possible to run your command."
            TokenWrongErr -> "The token was incorrect. Please log in again"
            )
    ]}

grabParam : Url.Url -> String -> Maybe String
grabParam url str =
    case url.query of
        Just q ->
            let chosen = grabWhile (\s -> case (index 0 (String.split "=" s), index 1 (String.split "=" s)) of
                    (Just n, _) -> n == str
                    _ -> False) (String.split "&" q) in
            Maybe.andThen (\s -> index 1 (String.split "=" s)) chosen
        Nothing -> Nothing

grabWhile : (a -> Bool) -> List a -> Maybe a
grabWhile p l =
    case (List.head l, List.tail l) of
        (Just h, Just t) -> if p h then Just h else grabWhile p t
        (Nothing, _) -> Nothing
        (Just h, Nothing) -> if p h then Just h else Nothing

index : Int -> List a -> Maybe a
index i l =
    snd (List.foldl (\e a -> case a of
        (_, Just _) -> a
        (ind, Nothing) -> if ind == i then (ind + 1, Just e) else (ind + 1, Nothing)) (0, Nothing) l)


snd t = case t of
    (_,s) -> s


nl : Html msg
nl = br [] []