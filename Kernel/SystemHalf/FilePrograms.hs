-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module FilePrograms
    ( getsProgram         -- Actor for Posix gets()
    , getcharProgram      -- Actor for Posix getchar()
    , putsProgram         -- Actor for Posix puts()
    , putcharProgram      -- Actor for Posix putchar()
    ) where

----------------------------------------------------------------------
-- The POSIX file system calls
----------------------------------------------------------------------

-- POSIX imports
import qualified SystemCall as SC
import qualified Errno as ER
-- Osker imports
import qualified SystemHalfActor as SHA
import qualified OskerMessage as OM

----------------------------------------------------------------------
-- The gets system call:
-- Read until a newline or end of file condition. Newlines are
-- discarded.
----------------------------------------------------------------------
getsProgram ::
    SHA.SystemHalfActorC -> OM.OskerPay -> SHA.SystemHalfProgram
getsProgram b _pay =
  do { (SHA.putStrLn b) ("getsProgram")
       -- Bust out some system half state elements
     ; pn <- SHA.processName
       -- Send the IO request to the standard IO device driver
     ; ( SHA.toStandard b ) ( OM.ReadSIO { OM.rsioProcessName = pn } )
     ; fsrsp <- SHA.getFromChan b
     ; SHA.sysRsp SC.noError (SC.GetsRsp (Just (getOskerCommand fsrsp)))
     }

-- Get the system request from the message from the standard IO
getOskerCommand :: OM.OskerPay -> OM.OskerCommand
getOskerCommand sp =
  case sp of
    OM.FromStandardIO iorsp ->
       case iorsp of
         OM.ReadSIORsp pc -> pc
         _else -> error ("getIOResponse.1: " ++ show sp)
    _otherwise -> error ("getIOResponse.2: " ++ show sp)

----------------------------------------------------------------------
-- The getchar system call:
-- Return the next character from standard input, or an EOF (Nothing)
----------------------------------------------------------------------
getcharProgram :: SHA.SystemHalfActorC -> OM.OskerPay -> SHA.SystemHalfProgram
getcharProgram _b _pay = SHA.sysRsp SC.noError (SC.GetCharRsp Nothing)

----------------------------------------------------------------------
-- The puts system call:
-- Write a string to standard output.
----------------------------------------------------------------------
putsProgram :: SHA.SystemHalfActorC -> OM.OskerPay -> SHA.SystemHalfProgram
putsProgram b pay =
  let sysreq = OM.getSysReq pay
      SC.PutsReq { SC.putsRsp = sysrsp } = sysreq
  in do { -- Bust out some system half state elements
        ; pn <- SHA.processName
        ; let ioreq = OM.WriteSIO
                      { OM.wsioProcessName = pn
                      , OM.wsioData = sysrsp
                      }
        ; ( SHA.toStandard b ) ioreq
        ; ( SHA.putStrLn b ) ("Response from standardIO: " ++ show pay)
        ; case pay of
            OM.FromStandardIO iorsp ->
              case iorsp of
                OM.WriteSIORsp { OM.wsioRsp = rsp } ->
                  if rsp
                  then SHA.sysRsp SC.noError (SC.PutsRsp SC.passFlag)
                  else SHA.sysRsp (Just ER.ENOTTY) (SC.PutsRsp SC.failFlag)
                _otherwise -> error ("puts.1: bad response: " ++ show pay)
            _else -> error ("puts.2: bad response: " ++ show pay)
        }

----------------------------------------------------------------------
-- The putchar system call:
-- Write a character to standard output
-- Not implemented yet
----------------------------------------------------------------------
putcharProgram :: SHA.SystemHalfActorC -> OM.OskerPay -> SHA.SystemHalfProgram
putcharProgram _b _pay = SHA.sysRsp SC.noError (SC.PutCharRsp Nothing)
