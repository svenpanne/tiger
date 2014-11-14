module Main ( main ) where

import DOT ( toDOT )
import Parser ( parser )
import System.Environment ( getArgs )
import System.IO ( hPutStrLn, stderr )

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              []  -> fmap (parser "<stdin>") getContents
              [f] -> fmap (parser f) (readFile f)
              _   -> error "expected max. 1 argument"
  either (hPutStrLn stderr) (putStrLn . toDOT) result
