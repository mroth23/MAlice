module MAlice.Parsing.Parser
       ( mparse
       , MParser (..)
       ) where

import Control.Monad
import Text.Parsec hiding (spaces)
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as T

import MAlice.Parsing.ParserState
import MAlice.Language.AST
import MAlice.Language.SymbolTable
import MAlice.Language.Types
import MAlice.SemanticAnalysis.TypeChecker

-- |Parses MAlice code and returns a list of errors or the AST
mparse :: String -> String -> Either String Program
mparse code name = do
  case (runParser maliceParse initState name code) of
    Left err -> Left ("Parse error in " ++ show err)
    Right (ast, st) ->
      case errors . errorList $ st of
        [] -> case warnings . warnList $ st of
                   [] -> Right ast
                   wl -> Left . show $ warnList st
        el -> Left . show $ errorList st

-- |Some basic definitions for the MAlice language
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

-- |The token parser created from the definition
lexer :: T.TokenParser ParserState
lexer = T.makeTokenParser maliceDef

-- |Some lexeme parsers that are used in the program
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

-- |Parses a MAlice source file and returns the AST and parser state.
-- Whitespace is ignored, the parser then proceeds to the EOF. The existence
-- of a program entry point (the procedure 'hatta') is also checked.
maliceParse :: MParser (Program, ParserState)
maliceParse = do
  whiteSpace
  ds <- decls
  eof
  checkEntryPoint
  finalState <- getState
  return $ (Program ds, finalState)

-- |Parses a list of at least one (valid) declaration
decls :: MParser Decls
decls = (lexeme $ do
  ps <- many1 decl
  return $ DeclList ps)

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
             --Case 2: Variable declaration with assignment
             (do { reserved "of"
                 ; e <- expr
                 ; terminator
                 ; checkExpr t e
                 ; insertSymbol var (Just t) IdVariable []
                 ; return $ VAssignDecl t var e })
           }) <|>
       --Case 3: Array / reference type declaration
       (do { reserved "had"
           ; e <- expr
           -- Check that array index is an integer expression
           ; checkExpr Number e
           ; t <- vtype
           ; terminator
           ; insertSymbol var (Just . RefType $ t) IdVariable []
           ; return $ VArrayDecl t var e })
     }) <?> "variable declaration"

-- |Parses a procedure or function declaration. If parsing succeeds, a new
-- symbol table is added to the hierarchy and the function arguments are added
-- to it, so the method body can be parsed successfully.
methodDecl :: MParser Decl
methodDecl =
  (do { reserved "The";
        (do { reserved "room"
            ; f <- identifier
            ; args <- formalParams
            ; reserved "contained"
            ; reserved "a"
            ; t <- vtype
            -- Set up the state for parsing of the body,
            -- in particular checking the return type
            ; enterMethod f (Just t) IdFunction args
            ; b <- body
            ; exitMethod
            ; checkReturnPath b f
            ; return $ FuncDecl f args t b }) <|>
        (do { reserved "looking-glass"
            ; f <- identifier
            ; args <- formalParams
            -- Same as in function declaration, except we don't
            -- have a return type
            ; enterMethod f Nothing IdProcedure args
            ; b <- body
            ; exitMethod
            ; return $ ProcDecl f args b })
      }) <?> "method declaration"

-- |Parses a method or variable declaration.
decl :: MParser Decl
decl = lexeme (
       varDecl <|>
       methodDecl <?>
       "declaration statement")

-- |Parses the parameters of a method as they appear in its declaration.
-- Parameters are a comma separated list of <type> <identifier> lexemes.
formalParams :: MParser FormalParams
formalParams = (try $ do
  ps <- parens $ commaSep formalParam
  return $ FPList ps) <?> "formal parameter list"

-- |A single parameter as it appears in the formal parameter list.
formalParam :: MParser FormalParam
formalParam = (lexeme $ do
  t <- vtype
  var <- identifier
  return $ Param t var) <?> "formal parameter"

-- |Parses the various kinds of allowed body blocks. A body block can either
-- be empty, contain a compound statement, or a number of declarations followed
-- by a compound statement.
body :: MParser Body
body = lexeme $ do {
    reserved "opened"
  ; (do { reserved "closed"
        ; return EmptyBody })        <|>
    (try $
     (do { cs <- compoundStmt
         ; reserved "closed"
         ; return $ StmtBody cs }))  <|>
    (do { ds <- decls
        ; cs <- compoundStmt
        ; reserved "closed"
        ; return $ DeclBody ds cs })
  }

-- |Parses a compound statement. Compound statements consist of at least one
-- 'statement'. Uses look-ahead because many1 is a primitive parser combinator.
compoundStmt :: MParser CompoundStmt
compoundStmt = (try . lexeme $ do
  ss <- many1 stmt
  return $ CSList ss) <?> "compound statement"

-- |Parses a body / block statement. This also creates a new scoping level so
-- already declared variables or functions can be re-declared locally.
bodyStmt :: MParser Stmt
bodyStmt = (do
  enterBlock
  b <- body
  exitBlock
  return $ SBody b) <?> "block statement"

-- |Parses the null statement.
nullStmt :: MParser Stmt
nullStmt = do
  reserved "."
  return SNull <?> "null statement"

-- |Parses a procedure call statement. Checks whether the called identifier
-- actually refers to a procedure. Uses look-ahead so no input is consumed
-- in case the parse fails, as identifiers can start off with expressions too.
idStmt :: MParser Stmt
idStmt = (try $ do
   f <- identifier
   args <- actualParams
   terminator
   checkProcCall f args
   return $ SCall f args) <?> "procedure call statement"

-- |Parses function statements. These can be: assignment, increment, decrement
-- and print. Also checks the types of expressions involved where necessary.
exprStmt :: MParser Stmt
exprStmt = (do {
  e1 <- expr;
  -- Assignment
  (do { reserved "became"
      ; e2 <- expr
      ; terminator
      ; checkAssignment e1 e2
      ; return $ SAssign e1 e2 }) <|>
  -- Increment
  (do { reserved "ate"
      ; terminator
      ; checkExpr Number e1
      ; return $ SInc e1 })       <|>
  -- Decrement
  (do { reserved "drank"
      ; terminator
      ; checkExpr Number e1
      ; return $ SDec e1 })       <|>
  -- Print #1
  (do { reserved "said"
      ; reserved "Alice"
      ; terminator
      ; return $ SPrint e1 })     <|>
  -- Print #2
  (do { reserved "spoke"
      ; terminator
      ; return $ SPrint e1 }) }) <?> "function statement"

-- |Parses a return statement. Checks that this is within a function and that
-- that function has a matching type to the expression returned.
returnStmt :: MParser Stmt
returnStmt = (do
  reserved "Alice"
  reserved "found"
  e <- expr
  reserved "."
  checkReturnType e
  return $ SReturn e) <?> "return statement"

-- |Parses an input statement. Checks that the input is put into an array or a
-- valid (non-reference) type.
inputStmt :: MParser Stmt
inputStmt = (do
  reserved "what"
  reserved "was"
  var <- expr
  checkInput var
  reserved "?"
  return $ SInput var) <?> "input statement"

-- |Parses a loop statement and checks that the loop condition is a valid
-- boolean expression.
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

-- |Parses an if/else block and checks that the condition is a valid boolean
-- expression.
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
  return $ SIf [If e c1, Else c2]) <?> "if/else block"

-- |Parses an if/else if block and checks that every condition is a valid
-- boolean expression.
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
               ; return $ If e' cst'}))
  ; es <- readElseIfs
  ; elseClause <- option [] $ do {
        ; reserved "or"
        ; elsest <- compoundStmt
        ; return [Else elsest] }
  ; reserved "because"; reserved "Alice"; reserved "was"
  ; reserved "unsure"; reserved "which"
  ; return $ SIf $ (If e cst) : es ++ elseClause }
  <?> "if/else if block"

-- |Parses any of the statements above.
stmt :: MParser Stmt
stmt =
  bodyStmt     <|>
  nullStmt     <|>
  idStmt       <|>
  exprStmt     <|>
  returnStmt   <|>
  inputStmt    <|>
  loopStmt     <|>
  ifElseStmt   <|>
  ifElseIfStmt <?>
  "statement"

-- |Parses a type name.
vtype :: MParser Type
vtype = lexeme $ (
  (reserved "number"   >> return Number)   <|>
  (reserved "letter"   >> return Letter)   <|>
  (reserved "sentence" >> return Sentence) <|>
  (reserved "spider" >> vtype >>= return . RefType) <?>
  "valid type name")

-- |Builds the expression parser for arithmetic and logic expressions
expr :: MParser Expr
expr =
  buildExpressionParser opTable exprTerm

-- |The operator table that specifies the precedences and associativities
-- of the operators. It is represented internally as a list of lists of
-- operators with the same precedence.
opTable =
  [ [ prefix "-"  (ENegate)
    , prefix "+"  (EPositive) ]
  , [ prefix "~"  (EInv)
    , prefix "!"  (ENot)      ]
  , [ opL    "|"  (EBOr)      ]
  , [ opL    "^"  (EBXor)     ]
  , [ opL    "&"  (EBAnd)     ]
  , [ opL    "+"  (EPlus)
    , opL    "-"  (EMinus)    ]
  , [ opL    "*"  (EMult)
    , opL    "/"  (EDiv)
    , opL    "%"  (EMod)      ]
  , [ opL    "==" (EEq)
    , opL    "!=" (ENEq)      ]
  , [ opL    ">"  (EGT)
    , opL    ">=" (EGTE)
    , opL    "<=" (ELTE)
    , opL    "<"  (ELT)       ]
  , [ opL    "||" (ELOr)      ]
  , [ opL    "&&" (ELAnd)     ] ]
  where
    prefix c f =
      Prefix (reservedOp c >> return f)
    op c f assoc =
      Infix (reservedOp c >> return f) assoc
    opL = flip flip AssocLeft . op

-- |An atom used in expressions.
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

-- |
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
