port module Repl.Client.TEA exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Platform.Cmd as Cmd
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
    = ReplHidden (Maybe String)
    | ReplShown ReplShownModel


type alias ReplShownModel =
    { input : String
    , prefill : Maybe String
    , output : List String
    }


initialModel : Model
initialModel =
    ReplHidden Nothing



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, initialCmd )



-- MSG


type Msg
    = ShowButtonClicked
    | InputChanged String
    | FormSubmitted
    | WorkerResponseReceived Api.WorkerResponseWire


initialCmd : Cmd Msg
initialCmd =
    Cmd.none



-- WORKER API


port sendToWorkerPort : Api.ClientRequestWire -> Cmd msg


port receiveFromWorkerPort : (Api.WorkerResponseWire -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveFromWorkerPort WorkerResponseReceived



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowButtonClicked ->
            showButtonClicked model

        InputChanged newInput ->
            inputChanged newInput model

        FormSubmitted ->
            formSubmitted model

        WorkerResponseReceived workerResponseWire ->
            workerResponseReceived workerResponseWire model



-- VIEW


view : Model -> Html Msg
view model =
    case model of
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
        , Html.Events.onClick ShowButtonClicked
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
        , Html.Events.onSubmit FormSubmitted
        ]
        [ Html.span []
            [ Html.text (promptString maybePrefill) ]
        , Html.input
            [ Html.Attributes.class "input"
            , Html.Attributes.value input
            , Html.Events.onInput InputChanged
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


showButtonClicked : Model -> ( Model, Cmd Msg )
showButtonClicked _ =
    ( ReplShown
        { input = ""
        , prefill = Nothing
        , output = []
        }
    , callWorker ""
    )


inputChanged : String -> Model -> ( Model, Cmd Msg )
inputChanged newInput model =
    ( setInput newInput model
    , Cmd.none
    )


formSubmitted : Model -> ( Model, Cmd Msg )
formSubmitted model =
    case model of
        ReplHidden _ ->
            ( model, Cmd.none )

        ReplShown shownModel ->
            ( addOutput [ promptString shownModel.prefill ++ shownModel.input ] model
            , callWorker shownModel.input
            )


callWorker : String -> Cmd Msg
callWorker input =
    sendToWorkerPort (Api.clientRequestCodec.encode { userInput = input })


workerResponseReceived : Api.WorkerResponseWire -> Model -> ( Model, Cmd Msg )
workerResponseReceived workerResponseWire model =
    handleWorkerResponse (Api.workerResponseCodec.decode workerResponseWire) model


handleWorkerResponse : Api.WorkerResponse -> Model -> ( Model, Cmd Msg )
handleWorkerResponse workerResponse model =
    case workerResponse.workerState of
        Api.WorkerStateRunning maybePrefill ->
            ( model
                |> setInput (Maybe.withDefault "" maybePrefill)
                |> setPrefill maybePrefill
                |> addOutput workerResponse.messages
            , Cmd.none
            )

        Api.WorkerStateStopped maybeError ->
            ( ReplHidden maybeError
            , Cmd.none
            )



-- HELPER


setInput : String -> Model -> Model
setInput input =
    modifyShownModel <|
        \shownModel ->
            { shownModel | input = input }


setPrefill : Maybe String -> Model -> Model
setPrefill prefill =
    modifyShownModel <|
        \shownModel ->
            { shownModel | prefill = prefill }


addOutput : List String -> Model -> Model
addOutput newOutput =
    modifyShownModel <|
        \shownModel ->
            { shownModel | output = newOutput ++ shownModel.output }


modifyShownModel : (ReplShownModel -> ReplShownModel) -> Model -> Model
modifyShownModel fun model =
    case model of
        ReplShown shownModel ->
            ReplShown (fun shownModel)

        ReplHidden _ ->
            model
