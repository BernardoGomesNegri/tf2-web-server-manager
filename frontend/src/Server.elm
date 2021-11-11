port module Server exposing (..)

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
import Http exposing (expectString, expectWhatever)
import Result exposing (..)
import Time
import Json.Decode exposing (Decoder, field, string, int, map8, list)
import Browser.Events exposing (onKeyDown, onKeyUp)

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

port askConfirm : String -> Cmd msg
port askConfirmResult : (Bool -> msg) -> Sub msg

type alias Player = {
    userid : Int,
    name : String,
    steamid : String,
    time : Int,
    ping : Int,
    loss : Int,
    connectionStatus : String,
    playerAdress : String
    }

type AppError = NoneErr | NoCmd | TokenWrongErr

type alias Model = {
    key : Browser.Navigation.Key,
    token : Maybe Token,
    command : String,
    commandResponse : String,
    waiting : Bool,
    error : AppError,
    players : List Player,
    isPressingEnter : Bool,
    askingFor : Maybe BanKickPlayer
    }

type alias BanKickPlayer = {
    method : BanKick,
    player : String
    }

type BanKick = Ban | Kick

type Msg = SendCmd | SetCmd String | TokenRight | TokenWrong | ChangePage Browser.UrlRequest | ChangeUrl Url.Url | CmdGood String|
    CmdBad | UpdatePlayers | GotPlayers (List Player) | UnPressEnter | BanPlayer Player | KickPlayer Player | Confirm | None

init : () -> Url.Url -> Navigation.Key -> (Model, Cmd Msg)
init _ url k =
    let token = grabParam url "token" in
    case token of
        Just t ->
            (Model k (Just t) "" "" False NoneErr [] False Nothing, Http.get ({url = UrlBuilder.absolute ["api", "validate", t] [], expect = expectString parseToken}))
        Nothing ->
            (Model k Nothing "" "" False NoneErr [] False Nothing, Cmd.map (\_ -> TokenWrong) (Navigation.load (UrlBuilder.absolute [""] [])))

runCommand t exp cmd =
    Http.post ({url = UrlBuilder.absolute ["api", "runcmd", cmd] [UrlBuilder.string "token" t], body = Http.emptyBody,
        expect = exp})

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SendCmd ->
            case model.token of
                Just t ->
                    ({model | waiting = True, isPressingEnter = True}, runCommand t (expectString parseCmdResponse) model.command)
                Nothing -> wrapModel {model | error = TokenWrongErr}
        SetCmd s -> wrapModel {model | command = s}
        TokenRight -> (model, Cmd.none)
        TokenWrong -> wrapModel {model | error = TokenWrongErr}
        ChangePage req -> (model, (Browser.Navigation.load (reqToString req)))
        ChangeUrl u -> (model, (Browser.Navigation.pushUrl model.key (Url.toString u)))
        CmdGood str -> wrapModel {model | commandResponse = str, waiting = False, error = NoneErr, isPressingEnter = False}
        CmdBad -> wrapModel {model | error = NoCmd, waiting = False, isPressingEnter = False}
        GotPlayers plL -> wrapModel {model | players = plL, isPressingEnter = False}
        UpdatePlayers ->
            case model.token of
                Just t ->
                    (model, Http.get {url = UrlBuilder.absolute ["api", "getplayers"] [UrlBuilder.string "token" t],
                        expect = Http.expectJson (parseAnyResponse GotPlayers) playerDecoder})
                Nothing -> wrapModel {model | error = TokenWrongErr}
        UnPressEnter -> wrapModel {model | isPressingEnter = False}
        Confirm ->
             case model.token of
                Just t ->
                    case model.askingFor of
                        Just ak ->
                            case ak.method of
                                Ban -> ({model | waiting = True}, runCommand t (expectString parseCmdResponse) ("banid 0 " ++ ak.player))
                                Kick -> ({model | waiting = True}, runCommand t (expectString parseCmdResponse) ("kickid " ++ ak.player))
                        Nothing -> wrapModel model
                Nothing -> wrapModel {model | error = TokenWrongErr}
        KickPlayer u -> ({model | askingFor = Just (BanKickPlayer Kick (String.fromInt u.userid))},
            askConfirm ("Are you sure you wish to kick: " ++ u.name ++ "?"))
        BanPlayer u -> ({model | askingFor = Just (BanKickPlayer Ban (String.fromInt u.userid))},
            askConfirm ("Are you sure you wish to permanently ban: " ++ u.name ++ "?"))
        None -> wrapModel model

playerDecoder : Decoder (List Player)
playerDecoder =
    Json.Decode.list (map8 Player (field "userid" int) (field "name" string) (field "steamid" string) (field "time" int) (field "ping" int) (field "loss" int)
        (field "connectionStatus" string) (field "playerAdress" string))

parseCmdResponse : Result Http.Error String -> Msg
parseCmdResponse = parseAnyResponse CmdGood

parseAnyResponse : (a -> Msg) -> Result Http.Error a -> Msg
parseAnyResponse f res =
    case res of
        Ok r -> f r
        Err _ -> CmdBad

parseToken : Result Http.Error String -> Msg
parseToken res =
    case res of
        Err _ -> TokenWrong
        Ok s ->
            if s == "1" then TokenRight else TokenWrong

reqToString req =
    case req of
        Browser.Internal u -> Url.toString u
        Browser.External e -> e

wrapModel m = (m, Cmd.none)
onUrlRequest = ChangePage
onUrlChange = ChangeUrl

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [Time.every 2500 (\_ -> UpdatePlayers), onKeyDown (enterDecoder SendCmd), onKeyUp (enterDecoder UnPressEnter), askConfirmResult (\res ->
        if res then Confirm else None)]

enterDecoder msg = Json.Decode.map (\key -> case key of
    "Enter" -> msg
    _ -> None) (field "key" string)


view : Model -> Browser.Document Msg
view model =
    {title = "TF2 server manager",
    body = [
        label [for "cmdinput"] [text "Input your command"], nl,
        input ([type_ "text", onInput SetCmd, id "cmdinput"] ++ if model.isPressingEnter then [value ""] else []) [], nl,
        button [onClick SendCmd] [text "Send command"],nl,
        table [] [
            thead [] [
                tr [] [
                    th [] [text "Players on the server"]
                ],
                tr [] [
                    th [] [text "Name"],
                    th [] [text "User Id"],
                    th [] [text "Steam Id"],
                    th [] [text "Connected for X seconds"],
                    th [] [text "Ping (in milliseconds)"],
                    th [] [text "Loss"],
                    th [] [text "Connection status"],
                    th [] [text "IP adress"],
                    th [] [text "Kick player"],
                    th [] [text "Permanently Ban player"]
                ]
            ],
            tbody [] (List.map (\p -> tr [] [td [] [text p.name], td [] [text (String.fromInt p.userid)], td [] [text p.steamid],
                td [] [text (String.fromInt p.time)], td [] [text (String.fromInt p.ping)], td [] [text (String.fromInt p.loss)],
                td [] [text p.connectionStatus], td [] [text p.playerAdress],
                td [] [button [onClick (KickPlayer p)] [text ("Kick " ++ p.name)]],
                td [] [button [onClick (BanPlayer p)] [text ("Permaban " ++ p.name)]]]) model.players)
        ]] ++
        List.concatMap (\s -> [text s, nl]) (String.split "\n" model.commandResponse) ++ [nl,
        if model.waiting then
            text "Waiting for server response"
        else
            text ""
        ,nl,
        text (case model.error of
            NoneErr -> ""
            NoCmd -> "The server took a while to respond. The command may not have ran."
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