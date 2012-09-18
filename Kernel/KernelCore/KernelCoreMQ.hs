-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module KernelCoreMQ
    ( createMQ    -- Create a message queue
    , openMQ      -- Open a message queue that already exists
    , unlinkMQ    -- Unlink a message queue
    , sendMQ      -- Send to a message queue that exists
    , closeMQ     -- Close a message queue
    , receiveMQ   -- Receive from a message queue
    ) where

----------------------------------------------------------------------
-- Requests for the kernel core portion of timers, i.e. the global
-- resource allocation
----------------------------------------------------------------------

-- Haskell imports
import qualified FiniteMap as FM
import List
import Maybe
-- Posix imports
import qualified Mode as M
import qualified Errno as ER
import qualified OpenFlags as OF
import qualified MessageQueueData as MQD
import qualified DomainGraph as DG
-- Graph imports
import qualified Deep as D
-- Domain model imports
import qualified GlobalResource as GR
-- Osker imports
import qualified KernelCoreActor as KCA
import qualified UnsafeAS as UAS
import qualified GlobalPartitionedStateKey as GPSK
import qualified GlobalPartitionedState as GPS
import qualified OskerMessage as OM
import qualified MQ as MQ

-- Create a message queue
createMQ :: KCA.KernelCoreSegment r
createMQ pay =
  do { GPS.GfseMessageQueueMap mqm <-
          KCA.getHardVanillaElt (GPSK.GPSK (Right GPSK.GPSKMessageQueueMap))
     ; let kcr = OM.getKernelCoreReq pay
     ; case kcr of
        OM.CreateMQ pn mqName flags attr mode ->
          let mmq = FM.lookupFM mqm mqName
          in case mmq of
               Nothing -> -- Add the message queue to the map
                 let senders = if M.isCreatorUserWrite mode
                               then [pn]
                               else []
                     receivers = if M.isCreatorUserRead mode
                                 then [pn]
                                 else []
                     mq = MQ.MessageQueue
                          { MQ.mqName      = mqName
                          , MQ.mqSenders   = senders
                          , MQ.mqReceivers = receivers
                          , MQ.mqMessages  = []
                          , MQ.mqMode      = mode
                          , MQ.mqFlags     = flags
                          , MQ.mqAttrs     = attr
                          }
                     mqm' = FM.addToFM
                              mqm
                              mqName
                              mq
                 in do { KCA.updateVanillaElt
                          (GPSK.GPSK (Right GPSK.GPSKMessageQueueMap))
                          (GPS.GfseMessageQueueMap mqm')
                       ; UAS.unsafeASIO ("Updated MQM")
                       ; KCA.shRsp ( OM.CreateMQResponse
                                     { OM.cmqrName  = Just mqName
                                     , OM.cmqrAttr  = attr
                                     , OM.cmqrFlags = flags
                                     } ) pn
                       }
               Just _mq ->
                 -- An error return, already exists
                 KCA.shRsp ( OM.CreateMQResponse
                             { OM.cmqrName  = Nothing
                             , OM.cmqrAttr  = MQD.initMQAttrs
                             , OM.cmqrFlags = OF.nullFlags
                             } ) pn
        _otherwise ->
          error ("createMQ: Bad message: " ++ show kcr)
     }

-- Open a message queue that already exists
openMQ :: KCA.KernelCoreSegment r
openMQ pay =
  do { -- Access the message queue map
     ; GPS.GfseMessageQueueMap mqm <-
         KCA.getHardVanillaElt (GPSK.GPSK (Right GPSK.GPSKMessageQueueMap))
       -- Access the process / domain model
     ; GPS.GfsePlatform graph <-
         KCA.getHardVanillaElt (GPSK.GPSK (Right GPSK.GPSKDomainModel))
     ; let kcr = OM.getKernelCoreReq pay
     ; case kcr of
        -- We assume that the name sent in is always the global name,
        -- not the local name
        OM.OpenMQ { OM.omqProcessName = pn
                  , OM.omqMQName      = mqName
                  , OM.omqFlags       = flags
                  , OM.omqAttr        = attrs
                  , OM.omqMode        = _mode
                  } ->
         let mmq = FM.lookupFM mqm mqName
             mnode = D.findNode pn graph
         in case mmq of
             Nothing ->
               -- An error return, does not exists
              do { UAS.unsafeASIO ("MQM = " ++ show mqm)
                 ; KCA.shRsp ( OM.nullOpenMQRsp ER.EEXIST ) pn
                 }
             Just mq ->
              if OF.flagRead flags
              then
                if OF.flagWrite flags
                then -- Read and write access
                  if M.isLocalUserRead (MQ.mqMode mq)
                  then -- Queue grants read
                    if M.isLocalUserWrite (MQ.mqMode mq)
                    then -- The queue grants write access
                         -- Add process as sender and receiver
                      let mq' = mq { MQ.mqSenders =
                                       pn:(MQ.mqSenders mq)
                                   , MQ.mqReceivers =
                                       pn:(MQ.mqReceivers mq)
                                   }
                          mqm' = FM.addToFM
                                   mqm
                                   mqName
                                   mq'
                      in if MQ.checkReadAccess mq graph pn &&
                            MQ.checkWriteAccess mq graph pn
                         then
                           do { KCA.updateVanillaElt
                                  (GPSK.GPSK
                                     (Right GPSK.GPSKMessageQueueMap))
                                  (GPS.GfseMessageQueueMap mqm')
                              ; KCA.shRsp ( OM.OpenMQResponse
                                            { OM.omqrErr   = Nothing
                                            , OM.omqrName  = Just mqName
                                            , OM.omqrAttr  = attrs
                                            , OM.omqrFlags = flags
                                            } ) pn
                              }
                         else
                           do { UAS.unsafeASIO ("Read Write")
                              ; KCA.shRsp ( OM.nullOpenMQRsp ER.EPERM ) pn
                              }
                    else -- The queue does not grant write access
                       KCA.shRsp ( OM.nullOpenMQRsp ER.EACCESS ) pn
                  else -- The queue does not grant read access
                    KCA.shRsp ( OM.nullOpenMQRsp ER.EACCESS ) pn
                else -- Read only access
                    if M.isLocalUserRead (MQ.mqMode mq)
                    then -- The queue grants read access
                      let mq' = mq { MQ.mqReceivers = pn:(MQ.mqReceivers mq) }
                          mqm' = FM.addToFM mqm mqName mq'
                      in if MQ.checkReadAccess mq graph pn 
                         then
                           do { KCA.updateVanillaElt
                                 (GPSK.GPSK
                                    (Right GPSK.GPSKMessageQueueMap))
                                 (GPS.GfseMessageQueueMap mqm')
                              ; KCA.shRsp ( OM.OpenMQResponse
                                            { OM.omqrErr   = Nothing
                                            , OM.omqrName  = Just mqName
                                            , OM.omqrAttr  = attrs
                                            , OM.omqrFlags = flags
                                            } ) pn
                              }
                         else
                           do { UAS.unsafeASIO
                                  ("Read only " ++
                                   show (M.isLocalUserRead (MQ.mqMode mq)) ++
                                   " " ++
                                   show (MQ.checkReadAccess mq graph pn)
                                   ++ " " ++ D.outNode 2 (fromJust mnode)
                                  )
                              ; KCA.shRsp ( OM.nullOpenMQRsp ER.EPERM ) pn
                              }
                    else -- The queue does not grant read access
                       KCA.shRsp (OM.nullOpenMQRsp ER.EACCESS) pn
              else if OF.flagWrite flags
                   then -- Write only access
                     if M.isLocalUserWrite (MQ.mqMode mq)
                     then -- The queue grants write access
                      let mq' = mq { MQ.mqSenders =
                                       pn:(MQ.mqSenders mq)
                                   }
                          mqm' = FM.addToFM
                                   mqm
                                   mqName
                                   mq'
                      in if MQ.checkWriteAccess mq graph pn
                         then do { KCA.updateVanillaElt
                                    (GPSK.GPSK
                                        (Right GPSK.GPSKMessageQueueMap))
                                    (GPS.GfseMessageQueueMap mqm')
                                 ; KCA.shRsp ( OM.OpenMQResponse
                                               { OM.omqrErr   = Nothing
                                               , OM.omqrName  = Just mqName
                                               , OM.omqrAttr  = MQD.initMQAttrs
                                               , OM.omqrFlags = OF.nullFlags
                                               } ) pn
                                 }
                         else
                           KCA.shRsp (OM.nullOpenMQRsp ER.EPERM) pn
                     else -- The queue does not grant write access
                        KCA.shRsp (OM.nullOpenMQRsp ER.EACCESS) pn
                   else KCA.shRsp (OM.nullOpenMQRsp ER.EACCESS) pn
        _otherwise ->
          error ("openMQ: Bad message: " ++ show kcr)
     }

-- Unlink a message queue that already exists
unlinkMQ :: KCA.KernelCoreSegment r
unlinkMQ pay =
  do { GPS.GfseMessageQueueMap mqm <-
         KCA.getHardVanillaElt (GPSK.GPSK (Right GPSK.GPSKMessageQueueMap))
     ; let kcr = OM.getKernelCoreReq pay
     ; case kcr of
        OM.UnlinkMQ { OM.umqMQName      = globalName
                    , OM.umqProcessName = pn
                    } ->
          let mmq = FM.lookupFM mqm globalName
          in case mmq of
               Nothing -> -- An error return, does not exists
                 KCA.shRsp (OM.UnlinkMQResponse False) pn
               Just _mq -> -- It exists, so kill it
                 let mqm' = FM.delFromFM mqm globalName
                 in do { KCA.updateVanillaElt
                          (GPSK.GPSK (Right GPSK.GPSKMessageQueueMap))
                          (GPS.GfseMessageQueueMap mqm')
                       ; UAS.unsafeASIO ("Deleted from MQM")
                       ; KCA.shRsp (OM.UnlinkMQResponse True) pn
                       }
        _otherwise ->
          error ("openMQ: Bad message: " ++ show kcr)
     }

-- Send a buffer to a message queue
sendMQ :: KCA.KernelCoreSegment r
sendMQ pay =
  do { GPS.GfseMessageQueueMap mqm <-
         KCA.getHardVanillaElt (GPSK.GPSK (Right GPSK.GPSKMessageQueueMap))
     ; let kcr = OM.getKernelCoreReq pay
     ; case kcr of
        OM.SendMQ { OM.smqProcessName = pn
                  , OM.smqMQName      = globalName
                  , OM.smqMessage     = message
                  } ->
          let mmq = FM.lookupFM mqm globalName
          in case mmq of
               Nothing ->
                 -- An error return, does not exists
                 KCA.shRsp (OM.SendMQResponse False) pn
               Just mq ->
                 -- The queue exists, so use it
                 let -- Add the message to the end of the queue
                     mq' = MQ.addEntry mq pn message
                     mqm' = FM.addToFM mqm globalName mq'
                 in do { KCA.updateVanillaElt
                           (GPSK.GPSK (Right GPSK.GPSKMessageQueueMap))
                           (GPS.GfseMessageQueueMap mqm')
                       ; KCA.shRsp (OM.SendMQResponse True) pn
                       }
        _otherwise ->
          error ("sendMQ: Bad message: " ++ show kcr)
     }

-- Close a message queue
closeMQ :: KCA.KernelCoreSegment r
closeMQ pay =
  do { UAS.unsafeASIO ("closeMQ")
     ; GPS.GfseMessageQueueMap mqm <-
         KCA.getHardVanillaElt (GPSK.GPSK (Right GPSK.GPSKMessageQueueMap))
     ; let kcr = OM.getKernelCoreReq pay
     ; case kcr of
        OM.CloseMQ { OM.clmqProcessName = pn
                   , OM.clmqMQName      = globalName
                   } ->
          let mmq = FM.lookupFM mqm globalName
          in case mmq of
               Nothing ->
                 -- An error return, does not exists
                 KCA.shRsp (OM.SendMQResponse False) pn
               Just mq ->
                 -- The queue exists, do use it
                 let mq' = mq { MQ.mqSenders =
                                   delete pn (MQ.mqSenders mq)
                              , MQ.mqReceivers =
                                   delete pn (MQ.mqSenders mq)
                              }
                     mqm' = FM.addToFM
                              mqm
                              globalName
                              mq'
                 in do { KCA.updateVanillaElt
                          (GPSK.GPSK (Right GPSK.GPSKMessageQueueMap))
                          (GPS.GfseMessageQueueMap mqm')
                       ; KCA.shRsp (OM.CloseMQResponse True) pn
                       }
        _otherwise ->
          error ("closeMQ: Bad message: " ++ show kcr)
     }

-- Receive a buffer from a message queue
receiveMQ :: KCA.KernelCoreSegment r
receiveMQ pay =
  do { -- Access the message queue map
     ; GPS.GfseMessageQueueMap mqm <-
         KCA.getHardVanillaElt (GPSK.GPSK (Right GPSK.GPSKMessageQueueMap))
       -- Access the process / domain model
     ; GPS.GfsePlatform graph <-
         KCA.getHardVanillaElt (GPSK.GPSK (Right GPSK.GPSKDomainModel))
       -- Get the incoming kernel core request
     ; let kcr = OM.getKernelCoreReq pay
     ; case kcr of
        OM.ReceiveMQ { OM.rmqProcessName = pn
                     , OM.rmqMQName      = globalName
                     } ->
         let mmq = FM.lookupFM mqm globalName
         in case mmq of
             Nothing -> -- An error return, msg queue does not exists
               KCA.shRsp
                 (OM.ReceiveMQResponse (Just ER.EEXIST) Nothing) pn
             Just mq ->
              -- The queue exists, so use it
              let -- Get the current messages
                  currentMessages = MQ.mqMessages mq
              in if null currentMessages
                 then -- No messages, so we fail
                   KCA.shRsp (OM.ReceiveMQResponse (Just ER.ENOENT) Nothing) pn
                 else
                   let mq' = mq { MQ.mqMessages =
                                   tail currentMessages }
                       mqm' = FM.addToFM mqm globalName mq'
                       receiveMsg = head currentMessages
                       source = MQ.msgSource receiveMsg
                       -- See if we have an edge
                       medge = D.findEdge graph source pn
                   in case medge of
                        -- No edge in the graph, so no communications
                        Nothing ->
                          KCA.shRsp (OM.ReceiveMQResponse (Just ER.EPERM) Nothing) pn
                        -- There is an edge in the graph, so apply the
                        -- filter, and return the filtrate
                        Just edge ->
                          do { KCA.updateVanillaElt
                                (GPSK.GPSK (Right GPSK.GPSKMessageQueueMap))
                                (GPS.GfseMessageQueueMap mqm')
                             ; KCA.shRsp (OM.ReceiveMQResponse
                                           Nothing
                                           (Just ((MQD.liftFilter
                                                     (DG.edgeFilter
                                                        (D.eFreight edge)))
                                                   (MQ.msgContents
                                                     receiveMsg))))
                                         pn
                             }
        _otherwise ->
          error ("receiveMQ: Bad message: " ++ show kcr)
     }
