module Main ( main ) where

import AbSyn ( Exp )
import ExpToDOT ( expToDOT )
import Parser ( parser )
import Semant ( ExpTy(..), semant )
import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )

pipeline :: FilePath -> String -> Either String (Exp, ExpTy)
pipeline f s = do
  expr <- parser f s
  trExpr <- semant expr
  return (expr, trExpr)

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
    []  -> fmap (pipeline "<stdin>") getContents
    [f] -> fmap (pipeline f) (readFile f)
    _   -> error "expected max. 1 argument"
  case result of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right (e, et) -> do
      putStrLn . expToDOT $ e
      hPutStrLn stderr . show . typeOf $ et
