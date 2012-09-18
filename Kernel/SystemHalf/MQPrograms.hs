-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module MQPrograms
    ( mqOpenProgram    -- Process the mq_open system request
    , mqCloseProgram   -- Process the mq_close system request
    , mqUnlinkProgram  -- Process the mq_unlink system request
    , mqSendProgram    -- Process the mq_send system request
    , mqReceiveProgram -- Process the mq_receive system request
    ) where

----------------------------------------------------------------------
-- The POSIX message queue system calls
----------------------------------------------------------------------

-- Haskell imports
import Monad
import Maybe
import qualified FiniteMap as FM
-- Actor imports
import qualified ActorThread as AT
-- Posix imports
import qualified SystemCall as SC
import qualified OpenFlags as OF
import qualified Errno as ER
import qualified MessageQueueData as MQD
-- Osker imports
import qualified SystemHalfActor as SHA
import qualified OskerMessage as OM
import qualified MQData as MQDAT
import qualified MQ as MQ

----------------------------------------------------------------------
-- The mq_open system call
----------------------------------------------------------------------
mqOpenProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the program
    MQDAT.MqdMap         -> -- Message queue data
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram   -- In the system program monad
mqOpenProgram b mqdmap pay =
  if elem OF.O_CREAT (SC.mqorFlags (OM.getSysReq pay))
  then mqCreate b mqdmap pay
  else mqOpen b mqdmap pay

-- First segment of mqOpen
mqOpen ::
    SHA.SystemHalfActorC -> -- Bounds on the program
    MQDAT.MqdMap         -> -- Message queue data
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram   -- In the system program monad
mqOpen b mqdmap pay =
  do { let sysreq = OM.getSysReq pay
     ; ( SHA.putStrLn b ) ("mqOpen")
     ; pn <- SHA.processName
     ; let openMQ = OM.OpenMQ
                    { OM.omqProcessName = pn
                    , OM.omqMQName      = MQD.makeGlobal
                                            (MQD.Name pn)
                                            (SC.mqorName sysreq)
                    , OM.omqFlags       = SC.mqorFlags sysreq
                    , OM.omqAttr        = SC.mqorAttr sysreq
                    , OM.omqMode        = SC.mqorMode sysreq
                    }
     ; ( SHA.toKernelCore b ) openMQ
     ; kcrsp <- SHA.getFromChan b
     ; case OM.getKernelCoreRsp kcrsp of
         OM.OpenMQResponse { OM.omqrErr   = merr
                           , OM.omqrName  = mglobalName
                           , OM.omqrAttr  = attr
                           , OM.omqrFlags = flags
                           } ->
           case merr of
             Nothing ->
               do { let globalName = fromJust mglobalName
                  ; ( SHA.putStrLn b ) ("mqOpen, mqname = " ++ show globalName)
                  ; if ( MQDAT.nullMq mqdmap )
                    then do { ( nextDesc, mqdmap' ) <-
                                 mqCreateDescriptor
                                   globalName flags attr mqdmap
                            ; mqRsp (Just mqdmap')
                                    SC.noError
                                    (SC.MqOpenRsp nextDesc attr)
                            }
                    else mqRsp Nothing
                               (Just ER.EMFILE)
                               (SC.MqOpenRsp MQD.errMQDesc attr)
                  }
             Just err -> mqRsp Nothing
                               (Just err)
                               (SC.MqOpenRsp MQD.errMQDesc attr)
         _otherwise -> error ("mqOpen: Bad response to Open: " ++ show pay)
     }

-- First segment of createMQ
mqCreate ::
    SHA.SystemHalfActorC -> -- Bounds on the program
    MQDAT.MqdMap         -> -- Message queue data
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram   -- In the system program monad
mqCreate b mqdmap pay =
  do { let sysreq = OM.getSysReq pay
     ; ( SHA.putStrLn b ) ("mqCreate")
     ; pn <- SHA.processName
     ; let createMQ = OM.CreateMQ
                      { OM.cmqProcessName = pn
                      , OM.cmqLocalName   = MQD.makeGlobal
                                               (MQD.Name pn)
                                               (SC.mqorName sysreq)
                      , OM.cmqFlags       = SC.mqorFlags sysreq
                      , OM.cmqMode        = SC.mqorMode sysreq
                      , OM.cmqAttr        = SC.mqorAttr sysreq
                      }
     ; ( SHA.toKernelCore b ) createMQ
     ; fsrsp <- SHA.getFromChan b
     ; case OM.getKernelCoreRsp fsrsp of
              OM.CreateMQResponse { OM.cmqrName  = mmqName
                                  , OM.cmqrAttr  = attr
                                  , OM.cmqrFlags = flags
                                  } ->
                 case mmqName of
                   Just mqName ->
                     do { if ( MQDAT.nullMq mqdmap )
                          then do { ( nextDesc, mqdmap') <-
                                      mqCreateDescriptor
                                        mqName flags attr mqdmap
                                  ; mqRsp (Just mqdmap')
                                          SC.noError
                                          (SC.MqOpenRsp nextDesc attr)
                                  }
                          else mqRsp Nothing
                                     (Just ER.EMFILE)
                                     (SC.MqOpenRsp MQD.errMQDesc attr)
                        }
                   Nothing ->
                     mqRsp Nothing
                           (Just ER.EEXIST)
                           (SC.MqOpenRsp MQD.errMQDesc MQD.NullAttr)
              _otherwise -> error ("mqCreate2.3: " ++ show pay)
     }

----------------------------------------------------------------------
-- The mq_close system call
----------------------------------------------------------------------
mqCloseProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the program
    MQDAT.MqdMap         -> -- Message queue map from process state
    OM.OskerPay          -> -- Payload of system request message
    SHA.SystemHalfProgram   -- In the system half program monad
mqCloseProgram b mqdmap pay =
  do { pn <- SHA.processName
     ; let mmqd = MQDAT.lookupMq mqdmap (SC.mqcrDesc (OM.getSysReq pay))
       in case mmqd of
            Nothing -> mqRsp Nothing (Just ER.EINVAL) (SC.MqCloseRsp (-1))
            Just mqd ->
              let mqdmap' = MQDAT.deleteMq
                             mqdmap (SC.mqcrDesc (OM.getSysReq pay))
                  closeMQ = OM.CloseMQ
                            { OM.clmqProcessName = pn
                            , OM.clmqMQName      = MQ.mqdName mqd
                            }
              in do { ( SHA.toKernelCore b ) closeMQ
                    ; fsrsp <- SHA.getFromChan b
                    ; case OM.getKernelCoreRsp pay of
                        OM.CloseMQResponse True ->
                           mqRsp (Just mqdmap')
                                 SC.noError
                                 (SC.MqCloseRsp 0)
                        OM.CloseMQResponse False ->
                           mqRsp (Just mqdmap')
                                 (Just ER.EINVAL)
                                 (SC.MqCloseRsp (-1))
                        _otherwise ->
                           error ("mqClose: Bad rsp to Close: " ++ show pay)
                    }
     }

-- Create a message queue descriptor, first segment
mqCreateDescriptor ::
    MQD.MQName   ->  -- Global name of the descriptor
    OF.OpenFlags ->  -- Flags to place in the descriptor
    MQD.MQAttr   ->  -- Attributes to place in the descriptor
    MQDAT.MqdMap ->  -- Message queue map
    SHA.SystemHalfActor ( MQD.MQDesc, MQDAT.MqdMap )
mqCreateDescriptor globalName flags attrs mqdmap =
  do { let desc = MQ.MessageQueueDescriptor
                  { MQ.mqdName  = globalName
                  , MQ.mqdFlags = flags
                  , MQ.mqdAttrs = attrs
                  }
           nextDesc = MQDAT.mqdNext mqdmap
           currMap  = MQDAT.mqdMap mqdmap
           -- Later, need a check on replicated descriptor numbers
           mqdmap' = MQDAT.MqdMap
                     { MQDAT.mqdNext = nextDesc + 1
                     , MQDAT.mqdMap  =
                         FM.addToFM currMap nextDesc desc
                     }
    ; return ( nextDesc, mqdmap' )
    }

----------------------------------------------------------------------
-- The mq_unlink system call
----------------------------------------------------------------------
mqUnlinkProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the program
    MQDAT.MqdMap         -> -- Message queue map from process state
    OM.OskerPay          -> -- Payload of system request message
    SHA.SystemHalfProgram   -- In the system half program monad
mqUnlinkProgram b mqdmap pay =
  do { globalName <- getGlobalName (SC.mqurLocalName (OM.getSysReq pay))
     ; pn <- SHA.processName
     ; removeFromMqdMap globalName mqdmap
     ; let unlinkMQ = OM.UnlinkMQ { OM.umqMQName = globalName
                                  , OM.umqProcessName = pn
                                  }
     ; ( SHA.toKernelCore b ) unlinkMQ
     ; fsrsp <- SHA.getFromChan b
     ; case OM.getKernelCoreRsp fsrsp of
         OM.UnlinkMQResponse True ->
           mqRsp Nothing SC.noError (SC.MqUnlinkRsp SC.passFlag)
         OM.UnlinkMQResponse False ->
           mqRsp Nothing (Just ER.EEXIST) (SC.MqUnlinkRsp SC.failFlag)
         _otherwise -> error ("mqUnlink: Bad response to Create: " ++ show pay)
     }

-- Remove an element from the message queue descriptor map
removeFromMqdMap ::
    MQD.MQName -> MQDAT.MqdMap -> SHA.SystemHalfActor MQDAT.MqdMap
removeFromMqdMap globalName mqdmap =
  if MQD.isLocal globalName
  then error "removeFromMqdMap: Local name passed in"
  else do { let finder :: MQD.MQDesc -> MQ.MessageQueueDescriptor -> Bool
                finder _desc mqd = MQ.mqdName mqd == globalName
                mqdmap' = MQDAT.filterMq mqdmap finder
          ; return ( mqdmap' )
          }

-- Get the global name, from a local or a global name
-- This assumes that the local name passed in is local to the calling
-- process
getGlobalName :: MQD.MQName -> SHA.SystemHalfActor MQD.MQName
getGlobalName name@(MQD.MQGlobal _gname _lname) = return name
getGlobalName (MQD.MQLocal lname) =
  do { pn <- SHA.processName
     ; return ( MQD.MQGlobal
                { MQD.globalPart = MQD.Name pn
                , MQD.localPart  = lname
                }
              )
     }

----------------------------------------------------------------------
-- The mq_send system call
----------------------------------------------------------------------
mqSendProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the system half program
    MQDAT.MqdMap         -> -- Message queue data map
    OM.OskerPay          -> -- Payload of message with system request
    SHA.SystemHalfProgram   -- In the system half program monad
mqSendProgram b mqdmap pay =
  do { let sysreq = OM.getSysReq pay
     ; pn <- SHA.processName
     ; let mmqd = MQDAT.lookupMq mqdmap (SC.mqsrDesc sysreq)
     ; case mmqd of
         Nothing ->
           mqRsp Nothing (Just ER.EEXIST) (SC.MqSendRsp SC.failFlag)
         Just mqd ->
           let sendMQ = OM.SendMQ
                        { OM.smqProcessName = pn
                        , OM.smqMQName      = MQ.mqdName mqd
                        , OM.smqMessage     = SC.mqsrMessage sysreq
                        }
           in do { ( SHA.toKernelCore b ) sendMQ
                 ;  case OM.getKernelCoreRsp pay of
                           OM.SendMQResponse True ->
                             mqRsp Nothing
                                   SC.noError
                                   (SC.MqSendRsp { SC.mqsrspFlag = 0 })
                           OM.SendMQResponse False ->
                             mqRsp Nothing
                                   (Just ER.EEXIST)
                                   (SC.MqSendRsp SC.failFlag)
                           _otherwise ->
                             error ("mqSend: Bad rsp to Send: " ++ show pay)
                 }
     }

----------------------------------------------------------------------
-- The mq_receive system call
----------------------------------------------------------------------
mqReceiveProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the program
    MQDAT.MqdMap         -> -- Message queue data map
    OM.OskerPay          -> -- Payload of message with system request
    SHA.SystemHalfProgram   -- In the system half program monad
mqReceiveProgram b mqdmap pay =
  do { let sysreq = OM.getSysReq pay
     ; pn <- SHA.processName
     ; let mmqd = MQDAT.lookupMq mqdmap (SC.mqrrDesc sysreq)
     ; case mmqd of
         Nothing ->
           mqRsp Nothing
                 ( Just ER.EEXIST )
                 ( SC.MqReceiveRsp { SC.mqrrspMessage = Nothing
                                   , SC.mqrrsp = SC.failFlag
                                   }
                 )
         Just mqd ->
           let receiveMQ = OM.ReceiveMQ
                           { OM.rmqProcessName = pn
                           , OM.rmqMQName      = MQ.mqdName mqd
                           }
           in do { ( SHA.toKernelCore b ) receiveMQ
                 ; case OM.getKernelCoreRsp pay of
                     OM.ReceiveMQResponse { OM.rmqrErr = merr
                                          , OM.rmqrMsg = mmsg } ->
                       case merr of
                         Nothing ->
                           mqRsp
                            Nothing
                            SC.noError
                            ( SC.MqReceiveRsp { SC.mqrrspMessage = mmsg
                                              , SC.mqrrsp = SC.passFlag
                                              }
                            )
                         Just err ->
                           mqRsp
                            Nothing
                            (Just err)
                            ( SC.MqReceiveRsp { SC.mqrrspMessage = Nothing
                                              , SC.mqrrsp = SC.failFlag
                                              }
                            )
                     _otherwise ->
                        error ("mqReceive: Bad rsp to Receive: " ++ show pay)
                 }
     }

----------------------------------------------------------------------
-- Actions useful to the message queue programs
----------------------------------------------------------------------
mqRsp ::
    Maybe MQDAT.MqdMap        -> -- Possibly return new MqdMap
    Maybe SC.Errno            -> -- Error return, if any
    SC.SpecificSystemResponse -> -- Response specific to the request
    SHA.SystemHalfProgram        -- In the system half program monad
mqRsp mmqdmap merr specific =
  return ( AT.MQResponse
           { AT.mmq = mmqdmap
           , AT.rsp = ( SC.SystemResponse { SC.srErrno    = merr
                                          , SC.srSpecific = specific
                                          }
                      )
           }
         )

