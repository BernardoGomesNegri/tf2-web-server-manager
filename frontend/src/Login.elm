module Login exposing (..)
import Browser
import Http exposing (Response(..))
import Html exposing (..)
import Html.Events exposing (..)
import Browser.Navigation as Navigation
import Url
import Url.Builder as UrlBuilder
import Maybe as Maybe
import Html.Attributes exposing (..)

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

onUrlRequest = ChangePage
onUrlChange = ChangeUrl

type alias Token = String

type AppError = NoApi | NoServer | BadPassword | BadPort

type alias Model = {
    key : Navigation.Key,
    token : Maybe Token,
    portNum : Maybe Int,
    adress : Maybe String,
    password : Maybe String,
    error : Maybe AppError
    }

type Msg = SetPort String | SetAdress String | SetPassword String | Login | ChangePage Browser.UrlRequest | ChangeUrl Url.Url | GotToken Token
    | TokenError AppError

init : () -> Url.Url -> Navigation.Key -> (Model, Cmd Msg)
init () _ key =
    (Model key Nothing Nothing Nothing Nothing Nothing,
    Cmd.none
    )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

update : Msg -> Model ->  ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPort prt ->
            case String.toInt prt of
                Just s ->
                    if s < 99999 && s > 0 then
                        ({model | portNum = Just s, error = Nothing}, Cmd.none)
                    else
                        ({model | error = Just BadPort}, Cmd.none)
                Nothing ->
                    ({model | error = Just BadPort}, Cmd.none)
        SetAdress adr ->
            ({model | adress = Just adr}, Cmd.none)
        SetPassword pwd ->
            ({model | password = Just pwd}, Cmd.none)
        Login ->
            case newReq model of
                Nothing ->
                    (model, Cmd.none)
                Just req ->
                    (model, Http.get {
                        url = req,
                        expect = Http.expectStringResponse apiResToMsg responseToResult
                    })
        ChangePage url ->
            case url of
                Browser.Internal i ->
                    (model, Navigation.load (Url.toString i))
                Browser.External e ->
                    (model, Navigation.load e)
        ChangeUrl _ -> (model, Cmd.none)
        GotToken t ->
            (model, Navigation.load (UrlBuilder.absolute ["server", ""] [UrlBuilder.string "token" t]))
        TokenError err ->
            ({model | error = (Just err)}, Cmd.none)

apiResToMsg : Result AppError Token -> Msg
apiResToMsg res =
    case res of
        Ok token -> GotToken token
        Err err -> TokenError err

responseToResult : Response String -> Result AppError Token
responseToResult r =
    case r of
        BadStatus_ meta _ ->
            case meta.statusCode of
                403 -> Err BadPassword
                503 -> Err NoServer
                _ -> Err NoApi
        GoodStatus_ _ s ->
            Ok s
        _ -> Err NoApi


newReq : Model -> Maybe String
newReq {portNum, adress, password} =
    Maybe.andThen (\prt -> Maybe.andThen (\adr -> Maybe.andThen (\pwd ->
        Just (UrlBuilder.absolute ["api","gettoken"] [UrlBuilder.int "port" prt, UrlBuilder.string "password" pwd,
        UrlBuilder.string "adress" adr])) password) adress) portNum

newLine = br [] [text ""]

view : Model -> Browser.Document Msg
view model =
    {title = "Login to your server",
    body = [
        div [] [
            label [for "adress"] [text "Server adress without port number. Can be either IP adress or domain"], newLine,
            input [type_ "text", id "adress", onInput SetAdress] [], newLine,
            label [for "port"] [text "Server port without adress. Just a plain number"], newLine,
            input [type_ "number", id "port", Html.Attributes.max "99999", Html.Attributes.min "0", onInput SetPort, placeholder "Port number, usually 27015"] [], newLine,
            label [for "password"] [text "Server RCON password, set using the cvar \"rcon_password\""], newLine,
            input [type_ "text", id "password", onInput SetPassword] [], newLine,
            button [onClick Login] [text "Login"], text "It may take a while for the server to respond, please be patient", newLine,
            div [] [text (case model.error of
                Just e ->
                    case e of
                        NoApi ->
                            "It was not possible to connect to the webserver. Make sure it is running."
                        NoServer ->
                            "It was not possible to connect to the server. Make sure the adress is right and try again."
                        BadPassword ->
                            """The RCON password you inserted was wrong. The RCON password is stored in the server's 'rcon_password' cvar, 
                            make sure it is not empty"""
                        BadPort ->
                            "Make sure the port you entered is a valid number between 0 and 99999"
                Nothing -> "")]
        ]
    ]}
