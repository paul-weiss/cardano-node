import           Cardano.Prelude

import           Control.Monad.Trans.Except.Exit (orDie)
import qualified Options.Applicative as Opt

import           Cardano.Command (opts, pref, renderCommandError, runCommand)
import           Cardano.TopHandler
import qualified System.Environment as Env
import qualified System.IO as IO


main :: IO ()
main = do
  IO.hPutStrLn stderr "locli main: check-in on stderr"
  IO.hPutStrLn stdout "locli main: entering"
  args <- Env.getArgs
  IO.hPutStrLn stdout $ intercalate " " args
  toplevelExceptionHandler $ do
    putByteString "locli main: entering toplevelExceptionHandler block\n"
    putByteString "locli main: about to parse command line\n"
    co <- Opt.customExecParser pref opts
    putByteString "locli main: finished parsing command line\n"

    putByteString "locli main: about to runCommand\n"
    orDie renderCommandError $ runCommand co
    putByteString "locli main: exiting toplevelExceptionHandler block\n"
  IO.hPutStrLn stdout "locli main: return from runCommand"
