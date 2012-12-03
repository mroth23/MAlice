module MAlice.Parser.Parser
       ( mparse
       ) where

import Control.Monad
import Text.Parsec hiding (spaces)
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as T

import MAlice.Parser.ParserState
import MAlice.Language.AST
import MAlice.Language.SymbolTable
import MAlice.Language.Types
import MAlice.SemanticAnalysis.ConstructChecker
import MAlice.SemanticAnalysis.ExprChecker
import MAlice.SemanticAnalysis.StmtChecker

-- |Parses MAlice code and returns a list of errors or the AST
mparse :: String -> String -> Either String (Program, ParserState)
mparse code name = do
  case (runParser maliceParse initState name code) of
    Left err -> Left ("Parse error in " ++ show err)
    Right (ast, st) ->
      case errors . errorList $ st of
        [] -> case warnings . warnList $ st of
                   [] -> Right (ast, st)
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
-- <decls> = <decl> | <decls> <decl>
decls :: MParser Decls
decls = (lexeme $ do
  ps <- many1 decl
  return $ DeclList ps)

-- |Parses a variable declaration and adds the variable to the symbol table.
-- <varDecl> = <id> <cont>
-- <cont>    = was a <type> <cont'> | had <expr> <type> <terminator>
-- <cont'>   = <terminator> | too <terminator> | of <expr> <terminator>
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
                 ; recordPosition
                 ; e <- expr
                 ; terminator
                 ; checkExpr t e (show $ VAssignDecl t var e)
                 ; clearPosition
                 ; insertSymbol var (Just t) IdVariable []
                 ; return $ VAssignDecl t var e })
           }) <|>
       --Case 3: Array / reference type declaration
       (do { reserved "had"
           ; recordPosition
           ; e <- expr
           ; t <- vtype
           ; terminator
           -- Check that array index is an integer expression
           ; checkExpr Number e (show $ VArrayDecl t var e)
           ; clearPosition
           ; insertSymbol var (Just . RefType $ t) IdVariable []
           ; return $ VArrayDecl t var e })
     }) <?> "variable declaration"

-- |Parses a procedure or function declaration. If parsing succeeds, a new
-- symbol table is added to the hierarchy and the function arguments are added
-- to it, so the method body can be parsed successfully.
-- <methodDecl> = The <cont>
-- <cont> = room <id> <formalParams> contained a <type> <body> |
--          looking-glass <id> <formalParams> <body>
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
-- <decl> = <methodDecl> | <varDecl>
decl :: MParser Decl
decl = lexeme (
       varDecl <|>
       methodDecl <?>
       "declaration statement")

-- |Parses the parameters of a method as they appear in its declaration.
-- Parameters are a comma separated list of <type> <identifier> lexemes.
-- <formalParams> = ( [<formalParam>] )
formalParams :: MParser FormalParams
formalParams = (try $ do
  ps <- parens $ commaSep formalParam
  return $ FPList ps) <?> "formal parameter list"

-- |A single parameter as it appears in the formal parameter list.
-- <formalParam> = <type> <id>
formalParam :: MParser FormalParam
formalParam = (lexeme $ do
  t <- vtype
  var <- identifier
  return $ Param t var) <?> "formal parameter"

-- |Parses the various kinds of allowed body blocks. A body block can either
-- be empty, contain a compound statement, or a number of declarations followed
-- by a compound statement.
-- <body> = opened closed | opened <compoundStmt> closed |
--          opened <decls> <compoundStmt> closed
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
-- <compoundStmt> = stmt | compoundStmt stmt
compoundStmt :: MParser CompoundStmt
compoundStmt = (try . lexeme $ do
  ss <- many1 stmt
  return $ CSList ss) <?> "compound statement"

-- |Parses a body / block statement. This also creates a new scoping level so
-- already declared variables or functions can be re-declared locally.
-- <bodyStmt> = <body>
bodyStmt :: MParser Stmt
bodyStmt = (do
  enterBlock
  b <- body
  exitBlock
  return $ SBody b) <?> "block statement"

-- |Parses the null statement.
-- <nullStmt> = .
nullStmt :: MParser Stmt
nullStmt = do
  reserved "."
  return SNull <?> "null statement"

-- |Parses a procedure call statement. Checks whether the called identifier
-- actually refers to a procedure. Uses look-ahead so no input is consumed
-- in case the parse fails, as identifiers can start off with expressions too.
-- <idStmt> = <id> <actualParams> <terminator>
idStmt :: MParser Stmt
idStmt = (try $ do
   f <- identifier
   recordPosition
   args <- actualParams
   terminator
   checkProcCall f args
   clearPosition
   return $ SCall f args) <?> "procedure call statement"

-- |Parses function statements. These can be: assignment, increment, decrement
-- and print. Also checks the types of expressions involved where necessary.
-- <exprStmt> = <expr> <cont>
-- <cont> = became <expr> <terminator> | ate <terminator> | drank <terminator>
--          said Alice <terminator> | spoke <terminator>
exprStmt :: MParser Stmt
exprStmt = (do {
  e1 <- expr;
  recordPosition;
  -- Assignment
  (do { reserved "became"
      ; e2 <- expr
      ; terminator
      ; checkAssignment e1 e2
      ; clearPosition
      ; return $ SAssign e1 e2 }) <|>
  -- Increment
  (do { reserved "ate"
      ; terminator
      ; checkExpr Number e1 (show e1 ++ " ate")
      ; clearPosition
      ; return $ SInc e1 })       <|>
  -- Decrement
  (do { reserved "drank"
      ; terminator
      ; checkExpr Number e1 (show e1 ++ " drank")
      ; clearPosition
      ; return $ SDec e1 })       <|>
  -- Print #1
  (do { reserved "said"
      ; reserved "Alice"
      ; terminator
      ; clearPosition
      ; return $ SPrint e1 })     <|>
  -- Print #2
  (do { reserved "spoke"
      ; terminator
      ; clearPosition
      ; return $ SPrint e1 }) }) <?> "function statement"

-- |Parses a return statement. Checks that this is within a function and that
-- that function has a matching type to the expression returned.
-- <returnStmt> = Alice found <expr> .
returnStmt :: MParser Stmt
returnStmt = (do
  reserved "Alice"
  reserved "found"
  recordPosition
  e <- expr
  reserved "."
  checkReturnType e
  clearPosition
  return $ SReturn e) <?> "return statement"

-- |Parses an input statement. Checks that the input is put into an array or a
-- valid (non-reference) type.
-- <inputStmt> = what was <expr> ?
inputStmt :: MParser Stmt
inputStmt = (do
  reserved "what"
  reserved "was"
  recordPosition
  var <- expr
  reserved "?"
  checkInput var
  clearPosition
  return $ SInput var) <?> "input statement"

-- |Parses a loop statement and checks that the loop condition is a valid
-- boolean expression.
-- <loopStmt> = eventually (<expr>) because <compoundStmt> enough times
loopStmt :: MParser Stmt
loopStmt = (do
  reserved "eventually"
  recordPosition
  e <- parens expr
  checkExpr Boolean e ("Loop condition: " ++ show e)
  clearPosition
  reserved "because"
  cond <- compoundStmt
  reserved "enough"
  reserved "times"
  return $ SLoop e cond) <?> "loop block"

-- |Parses an if/else block and checks that the condition is a valid boolean
-- expression.
-- <ifElseStmt> = either (<expr>) so <compoundStmt> or <compoundStmt>
ifElseStmt :: MParser Stmt
ifElseStmt = (do
  reserved "either"
  e <- parens expr
  checkExpr Boolean e ("'either' conditional: " ++ show e)
  reserved "so"
  c1 <- compoundStmt
  reserved "or"
  c2 <- compoundStmt
  reserved "because"; reserved "Alice"; reserved "was"
  reserved "unsure"; reserved "which"
  return $ SIf [If e c1, Else c2]) <?> "if/else block"

-- |Parses an if/else if block and checks that every condition is a valid
-- boolean expression.
-- <ifElseIfStmt> = perhaps (<expr>) so <compoundStmt> <cont>
-- <cont> = or maybe (<expr>) so <compoundStmt> <cont> | or <compoundStmt> | ε
ifElseIfStmt :: MParser Stmt
ifElseIfStmt = do {
    reserved "perhaps"
  ; recordPosition
  ; e <- parens expr
  ; checkExpr Boolean e ("'perhaps' conditional: " ++ show e)
  ; clearPosition
  ; reserved "so"
  ; cst <- compoundStmt
  ; let readElseIfs =
          (try . many $ try . lexeme $
           (do { reserved "or"
               ; reserved "maybe"
               ; e' <- parens expr
               ; recordPosition
               ; reserved "so"
               ; cst' <- compoundStmt
               ; checkExpr Boolean e ("'or maybe' conditional: " ++ show e')
               ; clearPosition
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
-- <stmt> = <bodyStmt> | <nullStmt> | <idStmt> | <exprStmt> | <returnStmt> |
--          <inputStmt> | <loopStmt> | <ifElseStmt> | <ifElseIfStmt>
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
-- <type> = number | letter | sentence| spider <type>
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
  [ [ prefix "-"  EUnOp
    , prefix "+"  EUnOp  ]
  , [ prefix "~"  EUnOp
    , prefix "!"  EUnOp  ]
  , [ opL    "|"  EBinOp ]
  , [ opL    "^"  EBinOp ]
  , [ opL    "&"  EBinOp ]
  , [ opL    "+"  EBinOp
    , opL    "-"  EBinOp ]
  , [ opL    "*"  EBinOp
    , opL    "/"  EBinOp
    , opL    "%"  EBinOp ]
  , [ opL    "==" EBinOp
    , opL    "!=" EBinOp ]
  , [ opL    ">"  EBinOp
    , opL    ">=" EBinOp
    , opL    "<=" EBinOp
    , opL    "<"  EBinOp ]
  , [ opL    "||" EBinOp ]
  , [ opL    "&&" EBinOp ] ]
  where
    prefix c f =
      Prefix (reservedOp c >> return (f c))
    op c f assoc =
      Infix (reservedOp c >> return (f c)) assoc
    opL = flip flip AssocLeft . op

-- |An atom used in expressions.
exprTerm = try . lexeme $
  (parens expr <|>
  do { num <- intLit; return $ EInt num }       <|>
  do { var <- identifier
     ; (do { reserved "'s"
           ; ix <- expr
           ; let arrRefExpr = EArrRef var ix
           ; checkExpr Number ix (show arrRefExpr)
           ; _ <- getArrayType var arrRefExpr
           ; reserved "piece"
           ; return arrRefExpr })              <|>
       (do { args <- actualParams
           ; return $ ECall var args })        <|>
       (do { notFollowedBy $
             reserved "'s" <|> (actualParams >> return ())
           ; return $ EId var }) }             <|>
  do { str <- stringLit; return $ EString str } <|>
  do { char <- charLit; return $ EChar char }   <?>
  "expression atom")

-- |Parses the parameters as they appear in the program, a list of expressions
-- <actualParams> = ( cont )
-- <cont> = ε | <expr> <cont>
actualParams :: MParser ActualParams
actualParams = (try . lexeme $ do
  aps <- parens $ commaSep expr
  return $ APList aps) <?> "method call argument list"

-- |Parses a terminator.
-- <terminator> = . | , | and | but | then
terminator :: MParser ()
terminator =
  reserved "."    <|>
  reserved ","    <|>
  reserved "and"  <|>
  reserved "but"  <|>
  reserved "then" <?>
  "statement terminator"
