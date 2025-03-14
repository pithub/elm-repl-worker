port module Repl.Client.IO exposing (main)

import Browser
import Extra.System.IO as IO
import Extra.System.IO.Port as Port
import Extra.Type.Lens as Lens
import Extra.Type.List exposing (TList)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Repl.Api as Api



-- APP


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- MODEL


type Model
    = Model
        { replState : ReplState
        , workerCont : Maybe (Port.SyncCont Model Api.WorkerResponse)
        }


type ReplState
    = ReplHidden (Maybe String)
    | ReplShown ReplShownModel


type alias ReplShownModel =
    { input : String
    , prefill : Maybe String
    , output : List String
    }


initialModel : Model
initialModel =
    Model
        { replState = ReplHidden Nothing
        , workerCont = Nothing
        }


lensReplState : Lens.Lens Model ReplState
lensReplState =
    { getter = \(Model model) -> model.replState
    , setter = \replState (Model model) -> Model { model | replState = replState }
    }


lensWorkerCont : Port.SyncLens Model Api.WorkerResponse
lensWorkerCont =
    { getter = \(Model model) -> model.workerCont
    , setter = \workerCont (Model model) -> Model { model | workerCont = workerCont }
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    update initialMsg initialModel



-- MSG


type alias Msg =
    IO.IO Model ()


initialMsg : Msg
initialMsg =
    IO.noOp



-- WORKER API


port sendToWorkerPort : Port.SendPort msg Api.ClientRequestWire


port receiveFromWorkerPort : Port.ReceivePort msg Api.WorkerResponseWire


clientToWorkerRequester : Port.SyncRequester Model Api.ClientRequest Api.WorkerResponse
clientToWorkerRequester =
    Api.clientToWorkerApi.requesterFun lensWorkerCont sendToWorkerPort receiveFromWorkerPort



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    clientToWorkerRequester.respond model



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg model of
        ( IO.Pure (), newModel ) ->
            ( newModel, Cmd.none )

        ( IO.ImpureCmd cmd, newModel ) ->
            ( newModel, cmd )

        ( IO.ImpureCont cont, newModel ) ->
            update (cont identity) newModel



-- VIEW


view : Model -> Html Msg
view (Model model) =
    case model.replState of
        ReplHidden maybeError ->
            viewReplHidden maybeError

        ReplShown shownModel ->
            viewReplShown shownModel


viewReplHidden : Maybe String -> Html Msg
viewReplHidden maybeError =
    Html.div [ Html.Attributes.class "hidden" ]
        ([]
            ++ viewMaybeError maybeError
            ++ viewShowButton
        )


viewMaybeError : Maybe String -> List (Html Msg)
viewMaybeError maybeError =
    [ Html.div [ Html.Attributes.class "stopped-message" ]
        [ Html.text (Maybe.withDefault "REPL stopped" maybeError) ]
    ]


viewShowButton : List (Html Msg)
viewShowButton =
    [ Html.button
        [ Html.Attributes.class "show-button"
        , Html.Events.onClick showButtonClicked
        ]
        [ Html.text "Show REPL" ]
    ]


viewReplShown : ReplShownModel -> Html Msg
viewReplShown shownModel =
    Html.div [ Html.Attributes.class "container" ]
        ([]
            ++ viewOutput shownModel.output
            ++ viewInput shownModel.prefill shownModel.input
        )


viewOutput : List String -> List (Html Msg)
viewOutput output =
    [ Html.div [ Html.Attributes.class "output" ]
        (List.map viewOutputEntry output)
    ]


viewOutputEntry : String -> Html Msg
viewOutputEntry entry =
    Html.pre [ Html.Attributes.class "entry" ]
        [ Html.text (adjustTrailingNewLines entry) ]


viewInput : Maybe String -> String -> List (Html Msg)
viewInput maybePrefill input =
    [ Html.form
        [ Html.Attributes.class "controls"
        , Html.Events.onSubmit formSubmitted
        ]
        [ Html.span []
            [ Html.text (promptString maybePrefill) ]
        , Html.input
            [ Html.Attributes.class "input"
            , Html.Attributes.value input
            , Html.Events.onInput inputChanged
            ]
            []
        ]
    ]


promptString : Maybe String -> String
promptString maybePrefill =
    case maybePrefill of
        Just _ ->
            "|\u{00A0}"

        Nothing ->
            ">\u{00A0}"


adjustTrailingNewLines : String -> String
adjustTrailingNewLines text =
    if String.endsWith "\n" text then
        String.trimRight text ++ "\n\n"

    else
        text



-- UI ACTIONS


showButtonClicked : Msg
showButtonClicked =
    IO.sequence
        [ IO.putLens lensReplState
            (ReplShown
                { input = ""
                , prefill = Nothing
                , output = []
                }
            )
        , callWorker ""
        ]


inputChanged : String -> Msg
inputChanged newInput =
    setInput newInput


formSubmitted : Msg
formSubmitted =
    IO.bind (IO.getLens lensReplState) <|
        \replState ->
            case replState of
                ReplHidden _ ->
                    IO.noOp

                ReplShown shownModel ->
                    IO.sequence
                        [ addOutput [ promptString shownModel.prefill ++ shownModel.input ]
                        , callWorker shownModel.input
                        ]


callWorker : String -> Msg
callWorker input =
    IO.bind (clientToWorkerRequester.request { userInput = input }) <|
        \workerResponse ->
            case workerResponse.workerState of
                Api.WorkerStateRunning maybePrefill ->
                    IO.sequence
                        [ setInput (Maybe.withDefault "" maybePrefill)
                        , setPrefill maybePrefill
                        , addOutput workerResponse.messages
                        ]

                Api.WorkerStateStopped maybeError ->
                    IO.putLens lensReplState (ReplHidden maybeError)



-- HELPER


setInput : String -> Msg
setInput input =
    modifyShownModel <|
        \shownModel ->
            { shownModel | input = input }


setPrefill : Maybe String -> Msg
setPrefill prefill =
    modifyShownModel <|
        \shownModel ->
            { shownModel | prefill = prefill }


addOutput : TList String -> Msg
addOutput newOutput =
    modifyShownModel <|
        \shownModel ->
            { shownModel | output = newOutput ++ shownModel.output }


modifyShownModel : (ReplShownModel -> ReplShownModel) -> Msg
modifyShownModel fun =
    IO.bind (IO.getLens lensReplState) <|
        \replState ->
            case replState of
                ReplShown shownModel ->
                    IO.putLens lensReplState (ReplShown (fun shownModel))

                ReplHidden _ ->
                    IO.noOp
