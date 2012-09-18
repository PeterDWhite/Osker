-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module StandardIODevice ( standardIODevice ) where

----------------------------------------------------------------------
-- The standard input output device driver
----------------------------------------------------------------------

-- Haskell imports
import Maybe
-- Utility imports
import qualified AssocWithList as A
-- Braid imprts
import qualified DomainBraid as DB
-- Posix imports
import qualified ProcessName as PN
import qualified SystemCall as SC
-- Local imports
import qualified DemoIF as DIF
import qualified StandardIOBd as B
-- Osker imports
import qualified OskerMessage as OM

-- The data maintained by the standard IO device driver
data SIOData =
    SIOData { inputReqList :: A.AssocWithList PN.ProcessName OM.OskerRspMsg
            , inputList    :: A.AssocWithList PN.ProcessName DIF.OskerInput
            } deriving (Show)

-- Insert a new IO Request
newIOReq :: SIOData -> OM.OskerRspMsg -> SIOData
newIOReq sioData sm =
  let msgPayload = OM.payOfM sm
  in case msgPayload of
       OM.ToStandardIO ioreq ->
         let inputReqList' = A.insert
                (inputReqList sioData)
                (OM.rsioProcessName ioreq)
                sm
         in sioData { inputReqList = inputReqList' }
       _otherwise ->
         error ("newIOReq: bad message: " ++ show sm)

-- Initialize the SIOData
initSIOData :: SIOData
initSIOData = SIOData { inputReqList = A.empty
                      , inputList    = A.empty
                      }

-- Resolve an IO request against the stored SIO data
resolveIORequest ::
    SIOData ->                        -- Current SIO data
    OM.IOReqSIO ->                    -- Incoming request
    (Maybe DIF.OskerCommand, SIOData) -- Resolution
resolveIORequest sioData ioreq =
  let (minput, inputList') =
        A.get (inputList sioData) (OM.rsioProcessName ioreq)
  in case minput of
       Nothing  -> ( Nothing, sioData )
       Just inp -> ( Just (DIF.input inp)
                   , sioData {inputList = inputList'}
                   )

-- Resolve an external input against the IO Requests in the SIO data
resolveInput ::
    SIOData ->                        -- Current SIO data
    DIF.OskerInput ->                 -- Incoming data
    (Maybe OM.OskerRspMsg, SIOData)   -- Resolution
resolveInput sioData incoming =
  let (msm, inputReqList') =
        A.get (inputReqList sioData) (DIF.target incoming)
  in case msm of
       Nothing -> ( Nothing, sioData )
       Just sm -> ( Just sm
                  , sioData { inputReqList = inputReqList' }
                  )

--------------------------------- -------------------------------------
-- The standard input output device driver is completely event driven
-- There are three channels:
--    The device channel, which lives in the resumption IO monad
--    The Osker input channel, which lives in the Haskell IO monad
--    The Osker output channel, which lives in the Haskell IO monad
--
-- The two input channels must be checked to see if there is any
-- input, this makes the device a little bit tricky.
----------------------------------------------------------------------
standardIODevice ::
    B.StandardIOBdC ->          -- Bounds on the IO
    B.StandardIOBd DB.DomainRet -- A resumption IO action, bounded
standardIODevice b = standardIODevice' initSIOData b

standardIODevice' ::
    SIOData         ->          -- Data maintained by the driver
    B.StandardIOBdC ->          -- Bounds on the IO
    B.StandardIOBd DB.DomainRet -- A resumption IO action, bounded
standardIODevice' sioData b =
 do { --pinEmpty  <- B.isExtEmpty $! b
--    ; let mpin = IOE.unsafePerformIO (B.checkExternal' b)
    ; mpin <- B.checkExternal b
    ; chanEmpty <- B.isChanEmpty b
    ; if chanEmpty
--      then if seq pinEmpty (const pinEmpty) pinEmpty
      then if isNothing mpin
           then -- No inputs from anyone
             do { B.yield b
                ; standardIODevice' sioData b
                }
           else -- External inputs (OskerInput) only
             do { --incoming <- B.fromExternal b
                ; sioData' <- processExternalInput sioData b (fromJust mpin)
                ; standardIODevice' sioData' b
                }
--      else if seq pinEmpty (const pinEmpty) pinEmpty
      else if isNothing mpin
           then -- Inputs from kernel (IO request) only
             do { sioData' <- processKernelInput sioData b
                ; standardIODevice' sioData' b
                }
           else -- Inputs from both external (OskerInput) and
                -- kernel (IO Request)
             do { --incoming <- B.fromExternal b
                  -- First insert the new input from externsl
                  sioData' <- processExternalInput sioData b (fromJust mpin)
                  -- Now process the incoming request
                ; sioData'' <- processKernelInput sioData' b
                ; standardIODevice' sioData'' b
                }
    }

-- Process an incoming external input
processExternalInput ::
    SIOData         ->          -- Initial state of driver data
    B.StandardIOBdC ->          -- Bounds on the IO
    DIF.OskerInput  ->          -- Osker input to process
    B.StandardIOBd SIOData      -- A resumption IO action, bounded
processExternalInput sioData b incoming =
  do { 
       -- Resolve the incoming input with IO requests
     ; let (msm, sioData') = resolveInput sioData incoming
       in case msm of
            Nothing ->
              let inputList' = A.insert
                                 (inputList sioData)
                                 (DIF.target incoming)
                                 incoming
              in do { sioOut b
                        ("\tExternal Input.2 = " ++ show incoming)
                    ; return ( sioData' { inputList = inputList' } )
                    }
            Just sm ->
              do { sioOut b
                     ("\tExternal Input.3 = " ++ show incoming)
                 ; (B.respond b) sm (mkResponsePayload (DIF.input incoming))
                 ; return sioData'
                 }
    }

-- Process an incoming IO Request.
processKernelInput ::
    SIOData         ->      -- Initial state of driver data
    B.StandardIOBdC ->      -- Bounds on the IO
    B.StandardIOBd SIOData  -- A resumption IO action, bounded
processKernelInput sioData b =
  do { sm <- B.getFromChan b
     ; let msgPayload = OM.payOfM sm
       in case msgPayload of
            OM.ToStandardIO ioreq ->
              case ioreq of
                OM.ReadSIO { OM.rsioProcessName = _pn } ->
                  let (msysreq, sioData') = resolveIORequest sioData ioreq
                  in case msysreq of
                       Nothing ->
                         do { sioOut b
                                ("Got ioreq/No resolution: " ++
                                 show ioreq)
                            ; let sioData'' = newIOReq sioData' sm
                            ; return sioData''
                            }
                       Just sysreq  ->
                         do { sioOut b
                                ("Got ioreq/Resolved: " ++
                                 show ioreq)
                            ; (B.respond b) sm (mkResponsePayload sysreq)
                            ; B.yield b
                              -- Format response and send it back
                              -- to the client kernel thread
                            ; return sioData'
                            }
                OM.WriteSIO { OM.wsioProcessName = pn
                            , OM.wsioData = sysrsp } ->
                  do { sioOut b ("Rsp to Demo " ++ (show sysrsp))
                       -- Send the output to the external channel
                     ; (B.toExternal b) pn sysrsp
                       -- Send response back to the system half
                     ; let iorsp = OM.FromStandardIO
                                     ( OM.WriteSIORsp { OM.wsioRsp = True } )
                     ; (B.respond b) sm iorsp
                     ; return sioData
                     }
            _otherwise -> error ("Bad msg to SIO: " ++ show sm)
     }

-- Construct the response message to a satisfied IO request.
mkResponsePayload :: DIF.OskerCommand -> OM.OskerRspPay
mkResponsePayload sysreq =
  let iorsp = OM.FromStandardIO (OM.ReadSIORsp {OM.rsiorSysReq = sysreq})
  in iorsp

-- Print outs from standard IO device driver
sioOut ::
    B.StandardIOBdC ->     -- Bounds on the IO
    String ->              -- String to print
    B.StandardIOBd ()      -- This is an IO action
sioOut b s = (B.putStrLn b) ("***[DD.SIO]...\t\t" ++ s)
