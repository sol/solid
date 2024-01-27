module Terminal (
  withoutCursor
, withAlternativeScreenBuffer
, withRawMode
) where

import Data.Typeable (cast)

import GHC.IO.Handle.Types (Handle__(..))
import GHC.IO.Handle.Internals (withHandle_)
use GHC.IO.FD as GHC

import System.Posix.Types (Fd(..))
import System.Posix.Terminal

withoutCursor :: IO a -> IO a
withoutCursor = bracket_ (stdout.write "\ESC[?25l") (stdout.write "\ESC[?25h")

withAlternativeScreenBuffer :: IO a -> IO a
withAlternativeScreenBuffer = bracket_ (stdout.write "\ESC[?1049h") (stdout.write "\ESC[?1049l")

withRawMode :: Handle -> IO a -> IO a
withRawMode h = withoutBuffering . withRawTerminalAttributes
  where
    withoutBuffering :: IO a -> IO a
    withoutBuffering action = bracket h.getBuffering h.setBuffering $ \ _ -> do
      h.setBuffering IO.NoBuffering -- this discards all in-process buffered input
      action

    withRawTerminalAttributes :: IO a -> IO a
    withRawTerminalAttributes action = handleAsFd "withRawMode" h >>= \ case
      Just fd -> withRawAttributes fd action
      Nothing -> action

    withRawAttributes :: Fd -> IO a -> IO a
    withRawAttributes = modifyTerminalAttributes
      WhenFlushed -- this discards all pending out-of-process input
      setRawMode

    setRawMode :: TerminalAttributes -> TerminalAttributes
    setRawMode =
      -- according to https://viewsourcecode.org/snaptoken/kilo/02.enteringRawMode.html
        without EnableEcho          -- ECHO
      . without KeyboardInterrupts  -- ISIG
      . without StartStopOutput     -- IXON
      . without ExtendedFunctions   -- IEXTEN
      . without MapCRtoLF           -- ICRNL
      . without ProcessOutput       -- OPOST

      -- already set by setBuffering
      -- . without ProcessInput        -- ICANON

      -- probably not needed
      -- . without InterruptOnBreak    -- BRKINT
      -- . without CheckParity         -- INPCK
      -- . without StripHighBit        -- ISTRIP
      -- . flip withBits 8             -- CS8
      where
        without = flip withoutMode

modifyTerminalAttributes :: TerminalState -> (TerminalAttributes -> TerminalAttributes) -> Fd -> IO a -> IO a
modifyTerminalAttributes optional_actions f fd action = bracket get set $ \ attributes -> do
  set (f attributes)
  action
  where
    get = getTerminalAttributes fd
    set attributes = setTerminalAttributes fd attributes optional_actions

handleAsFd :: [Char] -> Handle -> IO (Maybe Fd)
handleAsFd name h = withHandle_ name h $ \ Handle__{haDevice} ->
  return $ Fd . GHC.fdFD <$> cast haDevice
