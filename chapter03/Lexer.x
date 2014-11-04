{
module Lexer ( Token(..), AlexPosn(..), TokenClass(..), lexer ) where

import Prelude hiding ( Ordering(..) )
import Control.Monad ( when )
import Data.Char ( chr, ord )
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
<string>    \\          { \_ _ -> alexError "illegal escape" }
<string>    .           { addToString }

<0>         $white+     ;
<0,comment> "/*"        { incrementCommentDepthBy 1 }
<comment>   "*/"        { incrementCommentDepthBy (-1) }
<comment>   .|\n        ;

{
-- AlexInput helpers -----------------------------------------------------------

getPosition :: AlexInput -> AlexPosn
getPosition (p, _, _, _) = p

getString :: AlexInput -> Int -> String
getString (_, _, _, s) len = take len s

-- user state ------------------------------------------------------------------

data AlexUserState = AlexUserState {
  commentDepth :: Int,
  stringStart :: AlexPosn,
  stringContents :: String }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
  commentDepth = 0,
  stringStart = alexStartPos,
  stringContents = "" }

incCommentDepthBy :: Int -> Alex Int
incCommentDepthBy n = do
  us <- alexGetUserState
  let newCommentDepth = commentDepth us + n
  alexSetUserState us{commentDepth = newCommentDepth}
  return newCommentDepth

rememberStringStart :: AlexPosn -> Alex ()
rememberStringStart p = do
  us <- alexGetUserState
  alexSetUserState us{stringStart = p}

addToStringContents :: Char -> Alex ()
addToStringContents c = do
  us <- alexGetUserState
  alexSetUserState us{stringContents = c : stringContents us}

retrieveString :: Alex (AlexPosn, String)
retrieveString = do
  us <- alexGetUserState
  alexSetUserState us{stringStart = alexStartPos, stringContents = ""}
  return (stringStart us, reverse (stringContents us))

-- tokens ----------------------------------------------------------------------

data Token = Token AlexPosn TokenClass
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

-- grammar helpers -------------------------------------------------------------

makeToken :: TokenClass -> AlexAction Token
makeToken = makeTokenWith . const

makeTokenWith :: (String -> TokenClass) -> AlexAction Token
makeTokenWith f =
  token (\input len -> Token (getPosition input) (f (getString input len)))

startString :: AlexAction Token
startString input len = do
  rememberStringStart (getPosition input)
  begin string input len

endString :: AlexAction Token
endString = (\_ _ -> do
  (p, s) <- retrieveString
  return (Token p (STRING s))) `andBegin` 0

addToString :: AlexAction Token
addToString input len = do
  mapM_ addToStringContents (getString input len)
  skip input len

addCharToString :: Char -> AlexAction Token
addCharToString c input len = do
  addToStringContents c
  skip input len

addControlCharToString :: AlexAction Token
addControlCharToString input len =
  addCharToString (chr (ord (last (getString input len)) - ord '@')) input len

addAsciiToString :: AlexAction Token
addAsciiToString  input len = do
  let val = fst . head . readDec . drop 1 $ getString input len
  when (val >= 256) $
    alexError ("invalid ASCII escape " ++ getString input len)
  addCharToString (chr val) input len

incrementCommentDepthBy :: Int -> AlexAction Token
incrementCommentDepthBy n input len = do
  cd <- incCommentDepthBy n
  begin (if cd == 0 then 0 else comment) input len

alexEOF :: Alex Token
alexEOF = return (Token undefined EOF)

isEOF :: Token -> Bool
isEOF (Token _ EOF) = True
isEOF _ = False

lexer :: String -> [Token]
lexer str =
  let loop = do
        t <- alexMonadScan
        if isEOF t
           then do sc <- alexGetStartCode
                   when (sc == comment) $ alexError "comment not closed at EOF"
                   return []
           else do ts <- loop
                   return (t : ts)
  in either error id $ runAlex str loop
}
