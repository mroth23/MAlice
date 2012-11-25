module MAlice.Parsing.Parser
       ( mparse
       , MParser (..)
       ) where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T

import MAlice.Parsing.ParserState
import MAlice.Language.AST
import MAlice.Language.SymbolTable
import MAlice.Language.Types
import MAlice.SemanticAnalysis.TypeChecker

mparse :: String -> String -> Either String Program
mparse code name = do
  case (runParser maliceParse initState name code) of
    Left err -> Left ("Parse error in " ++ show err)
    Right (ast, st) ->
      case (errorList st) of
        [] -> Right ast
        el -> Left $ "Error:\n" ++ concatMap ((++"\n\n") . show) el

maliceDef =
  emptyDef { T.commentStart = ""
           , T.commentEnd = ""
           , T.commentLine = "###"
           , T.identStart = letter
           , T.identLetter = alphaNum <|> oneOf "_"
           , T.reservedNames =
             ["a", "Alice", "and", "ate", "became", "because", "but", "closed",
              "contained", "drank", "either", "enough", "eventually", "found",
              "had", "letter", "looking-glass", "maybe", "number", "of",
              "or", "opened", "perhaps", "piece", "room", "'s", "said",
              "sentence", "so", "spider", "spoke", "The", "then", "times",
              "too", "unsure", "was", "what", "which", ".", ","]
           , T.reservedOpNames =
             ["+", "-", "*", "/", "%"
             , ">", ">=", "==", "<=", "<", "!="
             , "&&", "||", "!", "~", "^", "|", "&"]
           }

lexer :: T.TokenParser ParserState
lexer = T.makeTokenParser maliceDef

identifier = T.identifier    lexer -- parses an identifier
reserved   = T.reserved      lexer -- parses a reserved name
reservedOp = T.reservedOp    lexer -- parses an operator
parens     = T.parens        lexer -- parses surrounding brackets
intLit     = T.integer       lexer -- parses an integer
semi       = T.semi          lexer -- parses a semicolon
whiteSpace = T.whiteSpace    lexer -- parses whitespace
stringLit  = T.stringLiteral lexer -- parses string literal
charLit    = T.charLiteral   lexer -- parses char literal
commaSep   = T.commaSep      lexer -- parses a comma separated list
lexeme     = T.lexeme        lexer -- parses with a parser, ignoring whitespace

maliceParse :: MParser (Program, ParserState)
maliceParse = do
  ds <- (whiteSpace >> decls)
  eof
  finalState <- getState
  return $ (Program ds, finalState)

decls :: MParser Decls
decls = lexeme $ do
  ps <- many1 decl
  return $ DeclList ps


varDecl = try $
  do { var <- identifier
     ; (do { reserved "was"
           ; reserved "a"
           ; t <- vtype
           ; (do { terminator
                 ; insertSymbol var (Just t) IdVariable []
                 ; return $ VarDecl t var }) <|>
             (do { reserved "too"
                 ; terminator
                 ; insertSymbol var (Just t) IdVariable []
                 ; return $ VarDecl t var }) <|>
             (do { reserved "of"
                 ; e <- expr
                 ; terminator
                 ; insertSymbol var (Just t) IdVariable []
                 ; return $ VAssignDecl t var e })
           }) <|>
       (do { reserved "had"
           ; e <- expr
           -- Array index must be an integer
           ; typecheckExpr Number e
           ; t <- vtype
           ; terminator
           ; insertSymbol var (Just . RefType $ t) IdVariable []
           ; return $ VArrayDecl t var e })
     }

methodDecl =
  do { reserved "The";
       (do { reserved "room"
           ; f <- identifier
           ; args <- formalParams
           ; reserved "contained"
           ; reserved "a"
           ; t <- vtype
           ; b <- body
           ; argTypes <- inferFParamTypes args
           ; insertSymbol f (Just t) IdFunction argTypes
           ; return $ FuncDecl f args t b }) <|>
       (do { reserved "looking-glass"
           ; f <- identifier
           ; args <- formalParams
           ; b <- body
           ; argTypes <- inferFParamTypes args
           ; insertSymbol f Nothing IdProcedure argTypes
           ; return $ ProcDecl f args b })
     }

decl :: MParser Decl
decl = lexeme $
       varDecl <|>
       methodDecl

formalParams :: MParser FormalParams
formalParams = try . lexeme $ do
  ps <- parens $ commaSep formalParam
  return $ FPList ps

formalParam :: MParser FormalParam
formalParam = try . lexeme $ do
  t <- vtype
  var <- identifier
  return $ Param t var

body :: MParser Body
body = try . lexeme $ do {
    reserved "opened"
  ; (do { reserved "closed"
        ; return EmptyBody })       <|>
    (try $
     (do { cs <- compoundStmt
         ; reserved "closed"
         ; return $ StmtBody cs })) <|>
    (do { ds <- decls
        ; cs <- compoundStmt
        ; reserved "closed"
        ; return $ DeclBody ds cs })
  }

compoundStmt :: MParser CompoundStmt
compoundStmt = try . lexeme $ do
  ss <- many stmt
  return $ CSList ss

bodyStmt :: MParser Stmt
bodyStmt = do
  b <- body
  return $ SBody b

nullStmt :: MParser Stmt
nullStmt = do
  reserved "."
  return SNull

idStmt :: MParser Stmt
idStmt = try $ do
   f <- identifier
   args <- actualParams
   terminator
   return $ SCall f args

exprStmt :: MParser Stmt
exprStmt = do {
  e1 <- expr;
  (do { reserved "became"
      ; e2 <- expr
      ; terminator
      ; return $ SAssign e1 e2 }) <|>
  (do { reserved "ate"
      ; terminator
      ; return $ SInc e1 })       <|>
  (do { reserved "drank"
      ; terminator
      ; return $ SDec e1 })       <|>
  (do { reserved "said"
      ; reserved "Alice"
      ; terminator
      ; return $ SPrint e1 })     <|>
  (do { reserved "spoke"
      ; terminator
      ; return $ SPrint e1 }) }

returnStmt :: MParser Stmt
returnStmt = do
  reserved "Alice"
  reserved "found"
  e <- expr
  reserved "."
  return $ SReturn e

inputStmt :: MParser Stmt
inputStmt = do
  reserved "what"
  reserved "was"
  var <- expr
  reserved "?"
  return $ SInput var

loopStmt :: MParser Stmt
loopStmt = do
  reserved "eventually"
  e <- parens expr
  reserved "because"
  cond <- compoundStmt
  reserved "enough"
  reserved "times"
  return $ SLoop e cond

ifElseStmt :: MParser Stmt
ifElseStmt = do
  reserved "either"
  e <- parens expr
  reserved "so"
  c1 <- compoundStmt
  reserved "or"
  c2 <- compoundStmt
  reserved "because"; reserved "Alice"; reserved "was"
  reserved "unsure"; reserved "which"
  return $ SIf [(e, c1), (ENot e, c2)]

ifElseIfStmt :: MParser Stmt
ifElseIfStmt = do {
    reserved "perhaps"
  ; e <- parens expr
  ; reserved "so"
  ; cst <- compoundStmt
  ; let readElseIfs =
          (try . many $ try . lexeme $
           (do { reserved "or"
               ; reserved "maybe"
               ; e' <- parens expr
               ; reserved "so"
               ; cst' <- compoundStmt
               ; return (e', cst')}))
  ; es <- readElseIfs
  ; (do { reserved "because"; reserved "Alice"; reserved "was"
        ; reserved "unsure"; reserved "which"
        ; return . SIf $ (e, cst) : es })
    <|>
    (do { reserved "or"
        ; elsest <- compoundStmt
        ; reserved "because"; reserved "Alice"; reserved "was"
        ; reserved "unsure"; reserved "which"
        ; return $ SIf $ (e, cst) : es ++ [(EEq (EInt 0) (EInt 0), elsest)] }) }
       -- TODO: Remove this hack for else (make if clause data type?)

stmt :: MParser Stmt
stmt =
  bodyStmt   <|>
  nullStmt   <|>
  idStmt     <|>
  exprStmt   <|>
  returnStmt <|>
  inputStmt  <|>
  loopStmt   <|>
  ifElseStmt <|>
  ifElseIfStmt

vtype :: MParser Type
vtype = lexeme $ (
  (reserved "number" >> return Number)                    <|>
  (reserved "letter" >> return Letter)                    <|>
  (reserved "sentence" >> return Sentence)                <|>
  do { reserved "spider"; t <- vtype; return $ RefType t } <?>
  "valid type name" )

expr :: MParser Expr
expr =
  buildExpressionParser opTable exprTerm

opTable =
  [ [ prefix "-" (ENegate), prefix "+" (EPositive)]
  , [ prefix "~" (EInv)   , prefix "!" (ENot) ]
  , [ opL "||" (ELOr) ]
  , [ opL "&&" (ELAnd)]
  , [ opL "|"  (EBOr) ]
  , [ opL "^"  (EBXor)]
  , [ opL "&"  (EBAnd)]
  , [ opL "==" (EEq)  , opL "!=" (ENEq) ]
  , [ opL ">"  (EGT)  , opL ">=" (EGTE)
    , opL "<=" (ELTE) , opL "<" (ELT)
    ]
  , [ opL "+"  (EPlus), opL "-" (EMinus) ]
  , [ opL "*"  (EMult), opL "/" (EDiv)
    , opL "%"  (EMod)
    ]
  ]
  where
    prefix c f =
      Prefix (reservedOp c >> return f)
    op c f assoc =
      Infix (reservedOp c >> return f) assoc
    opL = flip flip AssocLeft . op

exprTerm = try . lexeme $
  parens expr <|>
  do { num <- intLit; return $ EInt num }       <|>
  do { var <- identifier
     ; (do { reserved "'s"
           ; ix <- expr
           ; reserved "piece"
           ; return $ EArrRef var ix })        <|>
       (do { args <- actualParams
           ; return $ ECall var args })        <|>
       (do { notFollowedBy $
             reserved "'s" <|> (actualParams >> return ())
           ; return $ EId var }) }             <|>
  do { str <- stringLit; return $ EString str } <|>
  do { char <- charLit; return $ EChar char }

actualParams :: MParser ActualParams
actualParams = try . lexeme $ do
  aps <- parens $ commaSep expr
  return $ APList aps

terminator :: MParser ()
terminator =
  reserved "."    <|>
  reserved ","    <|>
  reserved "and"  <|>
  reserved "but"  <|>
  reserved "then" <?>
  "statement terminator"
