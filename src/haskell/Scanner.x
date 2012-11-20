{
module Scanner where
}

%wrapper "monad"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$graphic = $printable # $white

@char = \'[$graphic # \']\'
@string = \"(\\.|[^\"])*\"

tokens :-

  $white+                       { skip2 }
  "###".*                       { skip2 }
  "a"                           { mkT TKeyword }
  "Alice"                       { mkT TKeyword }
  "and"                         { mkT TDelimiter }
  "ate"                         { mkT TKeyword }
  "became"                      { mkT TKeyword }
  "because"                     { mkT TKeyword }
  "but"                         { mkT TDelimiter }
  "closed"                      { mkT TKeyword }
  "contained"                   { mkT TKeyword }
  "drank"                       { mkT TKeyword }
  "either"                      { mkT TKeyword }
  "enough"                      { mkT TKeyword }
  "eventually"                  { mkT TKeyword }
  "found"                       { mkT TKeyword }
  "glass"                       { mkT TKeyword }
  "had"                         { mkT TKeyword }
  "letter"                      { mkT TKeyword }
  "looking"                     { mkT TKeyword }
  "maybe"                       { mkT TKeyword }
  "number"                      { mkT TKeyword }
  "of"                          { mkT TKeyword }
  "or"                          { mkT TKeyword }
  "opened"                      { mkT TKeyword }
  "perhaps"                     { mkT TKeyword }
  "piece"                       { mkT TKeyword }
  "room"                        { mkT TKeyword }
  "s"                           { mkT TKeyword }
  "said"                        { mkT TKeyword }
  "sentence"                    { mkT TKeyword }
  "so"                          { mkT TKeyword }
  "spider"                      { mkT TKeyword }
  "spoke"                       { mkT TKeyword }
  "The"                         { mkT TKeyword }
  "then"                        { mkT TDelimiter }
  "times"                       { mkT TKeyword }
  "too"                         { mkT TKeyword }
  "unsure"                      { mkT TKeyword }
  "was"                         { mkT TKeyword }
  "what"                        { mkT TKeyword }
  "which"                       { mkT TKeyword }
  $digit+                       { mkT (TInt . read) }
  "."                           { mkT TDelimiter }
  "&&"                          { mkT TOperator }
  "||"                          { mkT TOperator }
  "!"                           { mkT TOperator }
  "~"                           { mkT TOperator }
  "+"                           { mkT TOperator }
  "-"                           { mkT TOperator }
  "*"                           { mkT TOperator }
  "/"                           { mkT TOperator }
  "<"                           { mkT TOperator }
  "<="                          { mkT TOperator }
  ">"                           { mkT TOperator }
  ">="                          { mkT TOperator }
  "!="                          { mkT TOperator }
  "%"                           { mkT TOperator }
  "=="                          { mkT TOperator }
  "("                           { mkT TSymbol }
  ")"                           { mkT TSymbol }
  $alpha[$alpha $digit \_]*     { mkT TIdent }
  @string                       { mkT (TString . init . tail) }
  @char                         { mkT (TChar . head . tail) }
  ","                           { mkT TDelimiter }
  "?"                           { mkT TSymbol }
  "^"                           { mkT TOperator }
  "|"                           { mkT TOperator }
  "&"                           { mkT TOperator }
  "'"                           { mkT TSymbol }
{


data Lexeme = L AlexPosn LexemeClass

-- The token type:
data LexemeClass =
     TKeyword String |    --Reserved keywords
     TOperator String |   --Operators
     TDelimiter String |  --Delimiters
     TString String |     --String literals
     TInt Int |           --Int literals
     TChar Char |         --Char literals
     TIdent String |      --Identifiers
     TSymbol String |     --Other symbols
     EOF                  --Alex EOF
     deriving (Eq, Show)

-- Make a token
mkT :: (String -> LexemeClass) -> AlexAction Lexeme
mkT ctor (p, _, _, input) len = return $ L p (ctor $ take len input)

alexEOF :: Alex Lexeme
alexEOF = return (L undefined EOF)

lexError s = do
  (p,c,rest,input) <- alexGetInput
  alexError $ "Lexical error at (" ++ showPosn p ++ "):\n" ++ take 2 input

showPosn (AlexPn off line col) =
  show line ++ ":" ++ show col

skip2 input len = alexMonadScan2

alexMonadScan2 = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError inp' -> lexError inp'
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan2
    AlexToken inp' len action -> do
        alexSetInput inp'
        action inp len

scanner str = runAlex str $ do
  loop []
  where  
    loop i = do
      l@(L _ c) <- alexMonadScan2
      if c == EOF
      then return i
      else do let i' = i++[l] in i' `seq` loop i'


scanString :: String -> [Lexeme]
scanString str =
  case (scanner str) of
    (Left s) -> error s
    (Right ls) -> ls

}