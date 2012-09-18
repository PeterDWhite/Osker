-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module KernelCoreIOShell
    ( kernelCoreIoShell  -- The shell around the executive
    , ResponseAction     -- Response to a system input
    ) where

----------------------------------------------------------------------
-- The IO Shell converts the stream of exceptions into an unbounded
-- channel of messages
----------------------------------------------------------------------

-- Haskell imports
import Maybe
import Monad
-- Braid imports
import qualified BraidExternal as B
import qualified OskerConcurrent as C
-- Actor imports
import qualified DomainBraid as DB
import qualified DomainOut as DO
-- Posix imports
import qualified ProcessName as PN
-- Osker imports
import qualified OskerMessage as OM
import qualified Message as M
import qualified ExecutiveState as ES
import qualified ExecutiveParameters as EP
import qualified ExecutiveActor as EAD
import qualified KernelCoreActor as KCA
import qualified Executive as EX
import qualified ExecutiveMonad as ExM
import qualified IOSOut as IOO
import qualified ProcessMap as PrM
import qualified KernelCore as KC

-- A type to capture the response action to a system input
-- based only on the message.
type ResponseAction =
    OM.OskerChannel -> OM.OskerMsg OM.OskerChannel -> DB.DomainBraid ()

-- The shell receives the top level inputs, and turns them
-- into a channel to the executive. Inputs come in as exceptions.
-- Each time there is an exception, the Shell is restarted.
-- The executive is in another thread, and does not require any
-- restarting.
kernelCoreIoShell ::
    PrM.ProcessMap                    -> -- Mapping proc name to proc data
    KCA.KernelCoreParameters OM.OskerChannel -> -- Parameterize the executive
    OM.OskerChannel                   -> -- Inbound to the executive
    ResponseAction                    -> -- Action to output response
    DB.DomainBraid ()                    -- An IO action
kernelCoreIoShell pmap params inChan outAction =
  let initfs = EP.epLocalState params
      inites = ES.initExecutiveState initfs
      name = EP.epProcessName params
  in do { tid <- DB.lift (C.myThreadId)
        ; DO.niceOut name ( "Starting (" ++ "," ++ show tid ++ ")"  )
          -- Consume the next input message
        ; kernelCoreIoShell' pmap name inites params inChan outAction
        }

kernelCoreIoShell' ::
    PrM.ProcessMap                    -> -- Mapping proc name to proc data
    PN.ProcessName                    -> -- Name of thread
    KC.KernelCoreExecutiveState OM.OskerChannel -> -- Current executive state
    KCA.KernelCoreParameters OM.OskerChannel -> -- Parameterize the executive
    OM.OskerChannel                   -> -- Inbound to the executive
    ResponseAction                    -> -- Action to output response
    DB.DomainBraid ()                    -- An IO action
kernelCoreIoShell' pmap name state1 params inChan outAction =
  do { msg <- DB.lift (C.readChan (OM.projPC inChan))
     ; DO.niceOut name ("Receiving: " ++ show msg)
       -- Run the state machine on the message
     ; tid <- DB.lift ( C.myThreadId )
     ; let (state2, outMsgs) =
               ExM.run state1 (EX.systemHalfSync tid params msg)
       -- Transform the input messages to output messages
     ; pmap' <- processOut pmap inChan name outAction outMsgs
       -- Do some more
     ; kernelCoreIoShell' pmap' name state2 params inChan outAction
     }

-- Process the output messages from the executive
processOut ::
    PrM.ProcessMap                -> -- Mapping proc name to proc data
    OM.OskerChannel               -> -- Inbound to the executive
    PN.ProcessName                -> -- Name for this IO shell
    ResponseAction                -> -- Action to output response
    [OM.OskerMsg OM.OskerChannel] -> -- Unbounded list of messages
    DB.DomainBraid PrM.ProcessMap
-- In this version of Osker, without getChanContents, an
-- empty list can occur here
processOut pmap _inChan _name _outAction [] = return pmap
processOut pmap inChan name outAction (msg:msgs) =
  do { let outChan = OM.getChan msg
           dest    = OM.destOfM msg
     ; IOO.iosOut name ("=== " ++ (show dest) ++ "\n" ++ (show msg))
     ; case dest of
          -- Put the message right back in the input channel:
          EAD.Self    -> do { DB.lift (C.writeChan (OM.projPC inChan) msg)
                            ; processOut pmap inChan name outAction msgs
                            }
          EAD.Deferred action -> -- Work deffered to IO level
            do { mretpay <- action (OM.payOfM msg)
               ; DO.niceOut name ("=/=/=/=" ++ show mretpay)
               ; case mretpay of
                   Nothing -> processOut pmap inChan name outAction msgs
                   Just retpay ->
                     do { DB.lift (C.writeChan (OM.projPC inChan)
                                      (OM.updatePayload msg retpay))
                        ; -- For an update process, add the entry to the
                          -- IO shell level process map
                        ; let pmap' = PrM.addDeferred retpay pmap
                              from  = OM.getFromProcessControlReq retpay
                        ; DO.niceOut name ("/*/*/*/" ++ show retpay)
                        ; DO.niceOut name ("/*/*/*/" ++ show from)
                        ; processOut pmap' inChan name outAction msgs
                        }
               }
          EAD.NoDestination ->
            do { outAction outChan msg
               ; processOut pmap inChan name outAction msgs
               }
          EAD.KernelCore    ->
            do { outAction outChan msg
               ; processOut pmap inChan name outAction msgs
               }
          EAD.Device _devid ->
            do { outAction outChan msg
               ; processOut pmap inChan name outAction msgs
               }
          EAD.Response      ->
            do { outAction outChan msg
               ; processOut pmap inChan name outAction msgs
               }
          EAD.SystemHalf pn ->
            let sys2Chan = PrM.getSys2Chan pmap pn
            in do { outAction sys2Chan msg
                  ; processOut pmap inChan name outAction msgs
                  }
          _otherwise -> error ("processOut: Bad destn: " ++ show dest)
     }
