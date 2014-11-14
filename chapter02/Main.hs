module Main where

import Lexer ( Token(..), TokenClass(..), unLex, runAlex', alexMonadScan' )
import System.Environment ( getArgs )

lexer :: FilePath -> String -> Either String [Token]
lexer = runAlex' $ unfoldWhileM (not . isEOF) alexMonadScan'

isEOF :: Token -> Bool
isEOF (Token _ EOF) = True
isEOF _ = False

unfoldWhileM :: Monad m => (a -> Bool) -> m a -> m [a]
unfoldWhileM p m = go id
 where go f = do x <- m
                 if p x
                   then go (f . (x:))
                   else return (f [])

renderToken :: Token -> String
renderToken (Token p c) = show p ++ ": " ++ unLex c

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              []  -> fmap (lexer "<stdin>") getContents
              [f] -> fmap (lexer f) (readFile f)
              _   -> error "expected max. 1 argument"
  either putStrLn (mapM_ (putStrLn . renderToken)) result
