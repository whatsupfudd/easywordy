module Commands.Help where

import qualified Options.Runtime as Rto

helpCmd :: Rto.RunOptions -> IO ()
helpCmd rtOpts =
  putStrLn "@[helpHu] starting."
