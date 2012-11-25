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
      case errors . errorList $ st of
        [] -> Right ast
        el -> Left $ "Error(s):\n" ++ show (errorList st)

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

-- |Parses an alice source file and returns the AST and parser state.
-- Whitespace is ignored, the parser then proceeds to the EOF. The existence
-- of a program entry point, the procedure 'hatta', is also checked.
maliceParse :: MParser (Program, ParserState)
maliceParse = do
  ds <- (whiteSpace >> decls)
  eof
  checkEntryPoint
  finalState <- getState
  return $ (Program ds, finalState)

-- |Parses a list of at least one (valid) declaration
decls :: MParser Decls
decls = (lexeme $ do
  ps <- many1 decl
  return $ DeclList ps)
  <?> "valid declaration"

-- |Parses a variable declaration and adds the variable to the symbol table.
varDecl :: MParser Decl
varDecl = (try $
  do { var <- identifier
     ; (do { reserved "was"
           ; reserved "a"
           ; t <- vtype
             --Case 1: Variable declaration
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
                 ; checkExpr t e
                 ; insertSymbol var (Just t) IdVariable []
                 ; return $ VAssignDecl t var e })
           }) <|>
       (do { reserved "had"
           ; e <- expr
           -- Array index must be an integer
           ; checkExpr Number e
           ; t <- vtype
           ; terminator
           ; insertSymbol var (Just . RefType $ t) IdVariable []
           ; return $ VArrayDecl t var e })
     }) <?> "variable declaration"

methodDecl =
  (do { reserved "The";
        (do { reserved "room"
            ; f <- identifier
            ; args <- formalParams
            ; reserved "contained"
            ; reserved "a"
            ; t <- vtype
            ; enterMethod f (Just t) IdFunction args
            ; b <- body
            ; exitMethod
            ; return $ FuncDecl f args t b }) <|>
        (do { reserved "looking-glass"
            ; f <- identifier
            ; args <- formalParams
            ; enterMethod f Nothing IdProcedure args
            ; b <- body
            ; exitMethod
            ; return $ ProcDecl f args b })
      }) <?> "method declaration"

decl :: MParser Decl
decl = lexeme (
       varDecl <|>
       methodDecl <?>
       "declaration statement")

formalParams :: MParser FormalParams
formalParams = (try . lexeme $ do
  ps <- parens $ commaSep formalParam
  return $ FPList ps) <?> "formal parameter list"

formalParam :: MParser FormalParam
formalParam = (try . lexeme $ do
  t <- vtype
  var <- identifier
  return $ Param t var) <?> "formal parameter"

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
        ; return $ DeclBody ds cs })<?>
    "valid body block"
  }

compoundStmt :: MParser CompoundStmt
compoundStmt = (try . lexeme $ do
  ss <- many1 stmt
  return $ CSList ss) <?> "compound statement"

bodyStmt :: MParser Stmt
bodyStmt = (do
  enterBlock
  b <- body
  exitBlock
  return $ SBody b) <?> "block statement"

nullStmt :: MParser Stmt
nullStmt = do
  reserved "."
  return SNull <?> "null statement"

idStmt :: MParser Stmt
idStmt = (try $ do
   f <- identifier
   args <- actualParams
   terminator
   checkProcCall f args
   return $ SCall f args) <?> "procedure call statement"

exprStmt :: MParser Stmt
exprStmt = (do {
  e1 <- expr;
  (do { reserved "became"
      ; e2 <- expr
      ; terminator
      ; checkAssignment e1 e2
      ; return $ SAssign e1 e2 }) <|>
  (do { reserved "ate"
      ; terminator
      ; checkExpr Number e1
      ; return $ SInc e1 })       <|>
  (do { reserved "drank"
      ; terminator
      ; checkExpr Number e1
      ; return $ SDec e1 })       <|>
  (do { reserved "said"
      ; reserved "Alice"
      ; terminator
      ; return $ SPrint e1 })     <|>
  (do { reserved "spoke"
      ; terminator
      ; return $ SPrint e1 }) }) <?> "function statement"

returnStmt :: MParser Stmt
returnStmt = (do
  reserved "Alice"
  reserved "found"
  e <- expr
  reserved "."
  checkReturnType e
  return $ SReturn e) <?> "return statement"

inputStmt :: MParser Stmt
inputStmt = (do
  reserved "what"
  reserved "was"
  var <- expr
  checkInput var
  reserved "?"
  return $ SInput var) <?> "input statement"

loopStmt :: MParser Stmt
loopStmt = (do
  reserved "eventually"
  e <- parens expr
  checkExpr Boolean e
  reserved "because"
  cond <- compoundStmt
  reserved "enough"
  reserved "times"
  return $ SLoop e cond) <?> "loop block"

ifElseStmt :: MParser Stmt
ifElseStmt = (do
  reserved "either"
  e <- parens expr
  checkExpr Boolean e
  reserved "so"
  c1 <- compoundStmt
  reserved "or"
  c2 <- compoundStmt
  reserved "because"; reserved "Alice"; reserved "was"
  reserved "unsure"; reserved "which"
  return $ SIf [(e, c1), (ENot e, c2)]) <?> "if/else block"

ifElseIfStmt :: MParser Stmt
ifElseIfStmt = do {
    reserved "perhaps"
  ; e <- parens expr
  ; checkExpr Boolean e
  ; reserved "so"
  ; cst <- compoundStmt
  ; let readElseIfs =
          (try . many $ try . lexeme $
           (do { reserved "or"
               ; reserved "maybe"
               ; e' <- parens expr
               ; checkExpr Boolean e
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
  <?> "if/else if block"
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
  ifElseIfStmt <?>
  "statement"

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
  , [ opL "|"  (EBOr) ]
  , [ opL "^"  (EBXor)]
  , [ opL "&"  (EBAnd)]
  , [ opL "+"  (EPlus), opL "-" (EMinus) ]
  , [ opL "*"  (EMult), opL "/" (EDiv)
    , opL "%"  (EMod)
    ]
  , [ opL "==" (EEq)  , opL "!=" (ENEq) ]
  , [ opL ">"  (EGT)  , opL ">=" (EGTE)
    , opL "<=" (ELTE) , opL "<" (ELT)
    ]
  , [ opL "||" (ELOr) ]
  , [ opL "&&" (ELAnd)]
  ]
  where
    prefix c f =
      Prefix (reservedOp c >> return f)
    op c f assoc =
      Infix (reservedOp c >> return f) assoc
    opL = flip flip AssocLeft . op

exprTerm = try . lexeme $
  (parens expr <|>
  do { num <- intLit; return $ EInt num }       <|>
  do { var <- identifier
     ; (do { reserved "'s"
           ; ix <- expr
           ; checkExpr Number ix
           ; _ <- getArrayType var
           ; reserved "piece"
           ; return $ EArrRef var ix })        <|>
       (do { args <- actualParams
           ; checkFuncCall var args
           ; return $ ECall var args })        <|>
       (do { notFollowedBy $
             reserved "'s" <|> (actualParams >> return ())
           ; return $ EId var }) }             <|>
  do { str <- stringLit; return $ EString str } <|>
  do { char <- charLit; return $ EChar char }   <?>
  "expression atom")

actualParams :: MParser ActualParams
actualParams = (try . lexeme $ do
  aps <- parens $ commaSep expr
  return $ APList aps) <?> "method call argument list"

terminator :: MParser ()
terminator =
  reserved "."    <|>
  reserved ","    <|>
  reserved "and"  <|>
  reserved "but"  <|>
  reserved "then" <?>
  "statement terminator"
