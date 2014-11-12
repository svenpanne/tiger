{
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches
    -fno-warn-missing-signatures -fno-warn-name-shadowing -fno-warn-amp #-}
module Lexer (
  Token(..), Position(..), Line, Column, TokenClass(..), unLex,
  Alex, runAlex', alexMonadScan', alexError'
) where

import Prelude hiding ( Ordering(..) )
import Control.Monad ( when )
import Data.Char ( chr, showLitChar ) -- ord is imported by alex template
import Numeric ( readDec )
}

%wrapper "monadUserState"

$digit      = 0-9
$letter     = [A-Za-z]

tiger :-

<0>         while       { makeToken WHILE }
<0>         for         { makeToken FOR }
<0>         to          { makeToken TO }
<0>         break       { makeToken BREAK }
<0>         let         { makeToken LET }
<0>         in          { makeToken IN }
<0>         end         { makeToken END }
<0>         function    { makeToken FUNCTION }
<0>         var         { makeToken VAR }
<0>         type        { makeToken TYPE }
<0>         array       { makeToken ARRAY }
<0>         if          { makeToken IF }
<0>         then        { makeToken THEN }
<0>         else        { makeToken ELSE }
<0>         do          { makeToken DO }
<0>         of          { makeToken OF }
<0>         nil         { makeToken NIL }

<0>         ","         { makeToken COMMA }
<0>         ":"         { makeToken COLON }
<0>         ";"         { makeToken SEMICOLON }
<0>         "("         { makeToken LPAREN }
<0>         ")"         { makeToken RPAREN }
<0>         "["         { makeToken LBRACK }
<0>         "]"         { makeToken RBRACK }
<0>         "{"         { makeToken LBRACE }
<0>         "}"         { makeToken RBRACE }
<0>         "."         { makeToken DOT }
<0>         "+"         { makeToken PLUS }
<0>         "-"         { makeToken MINUS }
<0>         "*"         { makeToken TIMES }
<0>         "/"         { makeToken DIVIDE }
<0>         "="         { makeToken EQ }
<0>         "<>"        { makeToken NEQ }
<0>         "<"         { makeToken LT }
<0>         "<="        { makeToken LE }
<0>         ">"         { makeToken GT }
<0>         ">="        { makeToken GE }
<0>         "&"         { makeToken AND }
<0>         "|"         { makeToken OR }
<0>         ":="        { makeToken ASSIGN }
<0>         [$digit]+   { makeTokenWith (INT . read) }
<0>         $letter [$letter $digit _]*
                        { makeTokenWith ID }

<0>         \"          { startString }
<string>    \"          { endString }
<string>    \\n         { addCharToString '\n' }
<string>    \\t         { addCharToString '\t' }
<string>    \\\^[@-_]   { addControlCharToString }
<string>    \\$digit$digit$digit
                        { addAsciiToString }
<string>    \\\"        { addCharToString '"' }
<string>    \\\\        { addCharToString '\\' }
<string>    \\$white+\\ ;
<string>    \\          { \inp _ -> errorAction (const "illegal escape") inp 0  }
<string>    .           { addToString }

<0>         $white+     ;
<0,comment> "/*"        { incrementCommentDepthBy 1 }
<comment>   "*/"        { incrementCommentDepthBy (-1) }
<comment>   .|\n        ;

{
-- AlexInput helpers -----------------------------------------------------------

getAlexPosn :: AlexInput -> AlexPosn
getAlexPosn (p, _, _, _) = p

getString :: AlexInput -> Int -> String
getString (_, _, _, s) len = take len s

-- user state ------------------------------------------------------------------

data AlexUserState = AlexUserState {
  filePath :: FilePath,
  commentDepth :: Int,
  stringStart :: Position,
  stringContents :: String }

data Position = Position FilePath Line Column
type Line = Int
type Column = Int

instance Show Position where
  showsPrec _ (Position fp l c) =
    showString fp . showChar ':' . shows l . showChar ':' . shows c

makePosition :: FilePath -> AlexPosn -> Position
makePosition fp (AlexPn _ l c) = Position fp l c

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
  filePath = "<unknown>",
  commentDepth = 0,
  stringStart = makePosition (filePath alexInitUserState) alexStartPos,
  stringContents = "" }

getPosition :: AlexInput -> Alex Position
getPosition input = do
  us <- alexGetUserState
  return $ makePosition (filePath us) (getAlexPosn input)

setFilePath :: FilePath -> Alex ()
setFilePath fp = do
  us <- alexGetUserState
  alexSetUserState us{filePath = fp}

incCommentDepthBy :: Int -> Alex Int
incCommentDepthBy n = do
  us <- alexGetUserState
  let newCommentDepth = commentDepth us + n
  alexSetUserState us{commentDepth = newCommentDepth}
  return newCommentDepth

rememberStringStart :: Position -> Alex ()
rememberStringStart pos = do
  us <- alexGetUserState
  alexSetUserState us{stringStart = pos}

addToStringContents :: Char -> Alex ()
addToStringContents c = do
  us <- alexGetUserState
  alexSetUserState us{stringContents = c : stringContents us}

retrieveString :: Alex (Position, String)
retrieveString = do
  us <- alexGetUserState
  alexSetUserState us{stringStart = stringStart alexInitUserState,
                      stringContents = stringContents alexInitUserState}
  return (stringStart us, reverse (stringContents us))

-- tokens ----------------------------------------------------------------------

data Token = Token Position TokenClass
  deriving ( Show )

data TokenClass =
    WHILE
  | FOR
  | TO
  | BREAK
  | LET
  | IN
  | END
  | FUNCTION
  | VAR
  | TYPE
  | ARRAY
  | IF
  | THEN
  | ELSE
  | DO
  | OF
  | NIL
  | COMMA
  | COLON
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | DOT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EQ
  | NEQ
  | LT
  | LE
  | GT
  | GE
  | AND
  | OR
  | ASSIGN
  | INT Integer
  | ID String
  | STRING String
  | EOF
  deriving ( Show )

unLex :: TokenClass -> String
unLex WHILE = "while"
unLex FOR = "for"
unLex TO = "to"
unLex BREAK = "break"
unLex LET = "let"
unLex IN = "in"
unLex END = "end"
unLex FUNCTION = "function"
unLex VAR = "var"
unLex TYPE = "type"
unLex ARRAY = "array"
unLex IF = "if"
unLex THEN = "then"
unLex ELSE = "else"
unLex DO = "do"
unLex OF = "of"
unLex NIL = "nil"
unLex COMMA = ","
unLex COLON = ":"
unLex SEMICOLON = ";"
unLex LPAREN = "("
unLex RPAREN = ")"
unLex LBRACK = "["
unLex RBRACK = "]"
unLex LBRACE = "{"
unLex RBRACE = "}"
unLex DOT = "."
unLex PLUS = "+"
unLex MINUS = "-"
unLex TIMES = "*"
unLex DIVIDE = "/"
unLex EQ = "="
unLex NEQ = "<>"
unLex LT = "<"
unLex LE = "<="
unLex GT = ">"
unLex GE = ">="
unLex AND = "$"
unLex OR = "|"
unLex ASSIGN = ":="
unLex (INT i) = show i
unLex (ID s) = s
unLex (STRING s) = show s
unLex EOF = "<EOF>"

alexEOF :: Alex Token
alexEOF = do
  pos <- alexGetInput >>= getPosition
  return $ Token pos EOF

isEOF :: Token -> Bool
isEOF (Token _ EOF) = True
isEOF _ = False

-- grammar helpers -------------------------------------------------------------

makeToken :: TokenClass -> AlexAction Token
makeToken = makeTokenWith . const

makeTokenWith :: (String -> TokenClass) -> AlexAction Token
makeTokenWith f input len = do
  pos <- getPosition input
  return $ Token pos (f (getString input len))

startString :: AlexAction Token
startString input len = do
  pos <- getPosition input
  rememberStringStart pos
  alexSetStartCode string
  alexMonadScan'

endString :: AlexAction Token
endString _ _ = do
  (p, s) <- retrieveString
  alexSetStartCode 0
  return $ Token p (STRING s)

addToString :: AlexAction Token
addToString input len = do
  mapM_ addToStringContents (getString input len)
  alexMonadScan'

addCharToString :: Char -> AlexAction Token
addCharToString c _ _ = do
  addToStringContents c
  alexMonadScan'

addControlCharToString :: AlexAction Token
addControlCharToString input len =
  addCharToString (chr (ord (last (getString input len)) - ord '@')) input len

addAsciiToString :: AlexAction Token
addAsciiToString input len = do
  let val = fst . head . readDec . drop 1 $ getString input len
  when (val >= 256) $
    errorAction (\s -> "invalid ASCII escape '" ++ s ++ "'") input len
  addCharToString (chr val) input len

incrementCommentDepthBy :: Int -> AlexAction Token
incrementCommentDepthBy n input len = do
  cd <- incCommentDepthBy n
  alexSetStartCode (if cd == 0 then 0 else comment)
  alexMonadScan'

alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> case () of
      _ | sc == string  -> errorAction (const "string not terminated") inp 0
        | sc == comment -> errorAction (const "comment not terminated") inp 0
        | otherwise     -> alexEOF
    AlexError inp' ->
        errorAction (\c ->
          "lexical error at character '" ++ showLitChar (head c) "'") inp' 1
    AlexSkip inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

errorAction :: (String -> String) -> AlexAction a
errorAction f input len = do
  pos <- getPosition input
  alexError' pos (f (getString input len))

alexError' :: Position -> String -> Alex a
alexError' p msg = alexError (show p ++ ": " ++ msg)

runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}
