import Html exposing (..)
import Html.Attributes exposing (class)
import Html.App as App
import Http
import Json.Decode exposing (..)
import Json.Encode exposing (encode)
import Task
import Either exposing (Either (..))

type alias Model =
    { resource : String
    , result : Either String (List (History)) }

type alias Period =
    { whence : String
    , until  : String }

type alias Record =
    { applicability : Period
    , rpsl          : List String }

type alias History =
    { handle    : String
    , ipVersion : String
    , versions  : List Record }

type Msg
  = Nada
  | Error Http.Error
  | Success (List History)

init : String -> (Model, Cmd Msg)
init resource = (Model resource (Left "Loading"), search resource)

errMsg : Http.Error -> String
errMsg err =
    case err of
        Http.Timeout             -> "timeout"
        Http.NetworkError        -> "network error"
        Http.UnexpectedPayload s -> "unexpected payload " ++ s
        Http.BadResponse i s     -> "bad response " ++ s

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Nada      -> (model, Cmd.none)
        Error err -> (Model model.resource (Left (errMsg err)), Cmd.none)
        Success h -> (Model model.resource (Right h), Cmd.none)

view : Model -> Html Msg
view model =
    let
        body = case model.result of
            Left error      -> [ div [ class "error" ] [text error] ]
            Right history   -> viewHistory history
    in
        div [ class "main" ] body

viewHistory : List History -> List (Html Msg)
viewHistory = List.map viewObject

viewObject : History -> Html Msg
viewObject h =
    div [ class "object" ]
        [ span [ class "range" ] [ text h.handle ]
        , span [ class "count" ] [ text (toString (List.length h.versions) ++ " versions") ]
        ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

search : String -> Cmd Msg
search resource =
    let
        url = "http://localhost:8080/v4?range=" ++ resource
    in
        Task.perform Error Success (Http.get history url)

history : Decoder (List History)
history = list record

record : Decoder History
record = object3 History
    ("handle"    := string)
    ("ipVersion" := string)
    ("versions"  := list version)

version : Decoder Record
version = object2 Record
    ("applicability" := tuple2 Period string string)
    ("rpsl"          := list string)

prettyPrint : Decoder String
prettyPrint = map (encode 2) value

main : Program Never
main = App.program
    { init = init "0.0.0.0-255.255.255.255"
    , view = view
    , update = update
    , subscriptions = subscriptions }
