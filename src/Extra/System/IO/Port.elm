module Extra.System.IO.Port exposing
    ( AckPort
    , AsyncAck
    , AsyncApi
    , AsyncCont
    , AsyncLens
    , AsyncReceiver
    , AsyncReceiverFun
    , AsyncSend
    , AsyncSender
    , AsyncSenderFun
    , Codec
    , ReceivePort
    , SafeCodec
    , SendPort
    , SyncApi
    , SyncCont
    , SyncLens
    , SyncRequest
    , SyncRequester
    , SyncRequesterFun
    , SyncResponder
    , SyncResponderFun
    , SyncRespond
    , UnsafeCodec
    , asyncApi
    , syncApi
    )

import Extra.System.IO as IO
import Extra.Type.Lens as Lens



-- CODEC


type alias Codec valueOut valueIn wire =
    { encode : valueOut -> wire
    , decode : wire -> valueIn
    }


type alias SafeCodec value wire =
    Codec value value wire


type alias UnsafeCodec value wire =
    Codec value (Maybe value) wire



-- PORTS


type alias SendPort msg wire =
    wire -> Cmd msg


type alias ReceivePort msg wire =
    (wire -> msg) -> Sub msg


type alias AckPort msg =
    (() -> msg) -> Sub msg



-- SYNC REQUESTER


type alias SyncCont s responseIn =
    responseIn -> IO.IO s ()


type alias SyncLens s responseIn =
    Lens.Lens s (Maybe (SyncCont s responseIn))


type alias SyncRequest s requestOut responseIn =
    requestOut -> IO.IO s responseIn


syncRequest :
    Codec requestOut requestIn requestWire
    -> SyncLens s responseIn
    -> SendPort (IO.IO s ()) requestWire
    -> SyncRequest s requestOut responseIn
syncRequest requestCodec lens requestPort requestOut =
    IO.liftCont <|
        \cont ->
            IO.sequence
                [ IO.putLens lens (Just cont)
                , IO.liftCmdIO (requestPort (requestCodec.encode requestOut))
                ]


type alias SyncRespond s =
    s -> Sub (IO.IO s ())


syncResponse :
    Codec responseOut responseIn responseWire
    -> SyncLens s responseIn
    -> ReceivePort (IO.IO s ()) responseWire
    -> SyncRespond s
syncResponse responseCodec lens responsePort state =
    responsePort <|
        \responseWire ->
            case lens.getter state of
                Just cont ->
                    IO.sequence
                        [ IO.putLens lens Nothing
                        , cont (responseCodec.decode responseWire)
                        ]

                Nothing ->
                    IO.noOp


type alias SyncRequester s requestOut responseIn =
    { request : SyncRequest s requestOut responseIn
    , respond : SyncRespond s
    }


type alias SyncRequesterFun s requestOut requestWire responseIn responseWire =
    SyncLens s responseIn
    -> SendPort (IO.IO s ()) requestWire
    -> ReceivePort (IO.IO s ()) responseWire
    -> SyncRequester s requestOut responseIn


syncRequesterFun :
    Codec requestOut requestIn requestWire
    -> Codec responseOut responseIn responseWire
    -> SyncRequesterFun s requestOut requestWire responseIn responseWire
syncRequesterFun requestCodec responseCodec lens sendPort receivePort =
    { request = syncRequest requestCodec lens sendPort
    , respond = syncResponse responseCodec lens receivePort
    }



-- SYNC RESPONDER


type alias SyncResponder s requestIn responseOut =
    (requestIn -> IO.IO s responseOut) -> Sub (IO.IO s ())


type alias SyncResponderFun s requestIn requestWire responseOut responseWire =
    ReceivePort (IO.IO s ()) requestWire
    -> SendPort (IO.IO s ()) responseWire
    -> SyncResponder s requestIn responseOut


syncResponderFun :
    Codec requestOut requestIn requestWire
    -> Codec responseOut responseIn responseWire
    -> SyncResponderFun s requestIn requestWire responseOut responseWire
syncResponderFun requestCodec responseCodec receivePort sendPort callback =
    receivePort <|
        \requestWire ->
            IO.bind (callback (requestCodec.decode requestWire)) <|
                \response ->
                    IO.liftCmdIO (sendPort (responseCodec.encode response))



-- SYNC API


type alias SyncApi s requestOut requestIn requestWire responseOut responseIn responseWire =
    { requesterFun : SyncRequesterFun s requestOut requestWire responseIn responseWire
    , responderFun : SyncResponderFun s requestIn requestWire responseOut responseWire
    }


syncApi :
    Codec requestOut requestIn requestWire
    -> Codec responseOut responseIn responseWire
    -> SyncApi s requestOut requestIn requestWire responseOut responseIn responseWire
syncApi requestCodec responseCodec =
    { requesterFun = syncRequesterFun requestCodec responseCodec
    , responderFun = syncResponderFun requestCodec responseCodec
    }



-- ASYNC SENDER


type alias AsyncCont s =
    () -> IO.IO s ()


type alias AsyncLens s =
    Lens.Lens s (Maybe (AsyncCont s))


type alias AsyncSend s valueOut =
    valueOut -> IO.IO s ()


asyncSend :
    Codec valueOut valueIn wire
    -> AsyncLens s
    -> SendPort (IO.IO s ()) wire
    -> AsyncSend s valueOut
asyncSend codec lens sendPort valueOut =
    IO.liftCont <|
        \cont ->
            IO.sequence
                [ IO.putLens lens (Just cont)
                , IO.liftCmdIO (sendPort (codec.encode valueOut))
                ]


type alias AsyncAck s =
    s -> Sub (IO.IO s ())


asyncAck :
    AsyncLens s
    -> AckPort (IO.IO s ())
    -> AsyncAck s
asyncAck lens ackPort state =
    ackPort <|
        \() ->
            case lens.getter state of
                Just cont ->
                    IO.sequence
                        [ IO.putLens lens Nothing
                        , cont ()
                        ]

                Nothing ->
                    IO.noOp


type alias AsyncSender s valueOut =
    { send : AsyncSend s valueOut
    , ack : AsyncAck s
    }


type alias AsyncSenderFun s valueOut wire =
    AsyncLens s
    -> SendPort (IO.IO s ()) wire
    -> AckPort (IO.IO s ())
    -> AsyncSender s valueOut


asyncSenderFun :
    Codec valueOut valueIn wire
    -> AsyncSenderFun s valueOut wire
asyncSenderFun codec lens sendPort ackPort =
    { send = asyncSend codec lens sendPort
    , ack = asyncAck lens ackPort
    }



-- ASYNC RECEIVER


type alias AsyncReceiver s valueIn =
    (valueIn -> IO.IO s ()) -> Sub (IO.IO s ())


type alias AsyncReceiverFun s valueIn wire =
    ReceivePort (IO.IO s ()) wire
    -> AsyncReceiver s valueIn


asyncReceiverFun :
    Codec valueOut valueIn wire
    -> AsyncReceiverFun s valueIn wire
asyncReceiverFun codec receivePort callback =
    receivePort <|
        \wire ->
            callback (codec.decode wire)



-- ASYNC API


type alias AsyncApi s valueOut valueIn wire =
    { senderFun : AsyncSenderFun s valueOut wire
    , receiverFun : AsyncReceiverFun s valueIn wire
    }


asyncApi :
    Codec valueOut valueIn wire
    -> AsyncApi s valueOut valueIn wire
asyncApi codec =
    { senderFun = asyncSenderFun codec
    , receiverFun = asyncReceiverFun codec
    }
