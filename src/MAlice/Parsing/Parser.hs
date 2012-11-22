module MAlice.Parsing.Parser ( parseMAlice
                             ) where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T
import MAlice.Language.AST
import MAlice.Language.SymbolTable
import MAlice.SemanticAnalysis.TypeCheck

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

lexer :: T.TokenParser ()
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

maliceParse :: Parser Program
maliceParse = do
  ds <- (whiteSpace >> decls)
  eof
  return $ Program ds

decls :: Parser Decls
decls = lexeme $ do
  ps <- many1 decl
  return $ DeclList ps

decl :: Parser Decl
decl = lexeme $
  (try $
  do { var <- identifier
     ; (do { reserved "was"
           ; reserved "a"
           ; t <- vtype
           ; (do { terminator
                 ; return $ VarDecl t var }) <|>
             (do { reserved "too"
                 ; terminator
                 ; return $ VarDecl t var }) <|>
             (do { reserved "of"
                 ; e <- expr
                 ; terminator
                 ; return $ VAssignDecl t var e }) }) <|>
       (do { reserved "had"
           ; e <- expr
           ; t <- vtype
           ; terminator
           ; return $ VArrayDecl t var e })
     }) <|>
  do { reserved "The";
       (do { reserved "room"
           ; f <- identifier
           ; args <- formalParams
           ; reserved "contained"
           ; reserved "a"
           ; t <- vtype
           ; b <- body
           ; return $ FuncDecl f args t b }) <|>
       (do { reserved "looking-glass"
           ; f <- identifier
           ; args <- formalParams
           ; b <- body
           ; return $ ProcDecl f args b })
     }

formalParams :: Parser FormalParams
formalParams = try . lexeme $ do
  ps <- parens $ commaSep formalParam
  return $ FPList ps

formalParam :: Parser FormalParam
formalParam = try . lexeme $ do
  t <- vtype
  var <- identifier
  return $ Param t var

body :: Parser Body
body = try . lexeme $ do {
  reserved "opened";
  (do { reserved "closed"
      ; return EmptyBody }) <|>
  (try $
  (do { cs <- compoundStmt
      ; reserved "closed"
      ; return $ StmtBody cs })) <|>
  (do { ds <- decls
      ; cs <- compoundStmt
      ; reserved "closed"
      ; return $ DeclBody ds cs })
  }

compoundStmt :: Parser CompoundStmt
compoundStmt = try . lexeme $ do
  ss <- many stmt
  return $ CSList ss

stmt :: Parser Stmt
stmt = try . lexeme $
  do { b <- body; return $ SBody b } <|>
  do { reserved "."; return $ SNull } <|>
  (try $
  do { f <- identifier
     ; args <- actualParams
     ; terminator
     ; return $ SCall f args }) <|>
  do { e1 <- expr;
       (do { reserved "became"
           ; e2 <- expr
           ; terminator
           ; return $ SAssign e1 e2 }) <|>
       (do { reserved "ate"
           ; terminator
           ; return $ SInc e1 }) <|>
       (do { reserved "drank"
           ; terminator
           ; return $ SDec e1 }) <|>
       (do { reserved "said"
           ; reserved "Alice"
           ; terminator
           ; return $ SPrint e1 }) <|>
       (do { reserved "spoke"
           ; terminator
           ; return $ SPrint e1 })
     } <|>
  do { reserved "Alice"
     ; reserved "found"
     ; e <- expr
     ; reserved "."
     ; return $ SReturn e } <|>
  do { reserved "what"
     ; reserved "was"
     ; var <- expr
     ; reserved "?"
     ; return $ SInput var } <|>
  do { reserved "eventually"
     ; e <- parens expr
     ; reserved "because"
     ; cond <- compoundStmt
     ; reserved "enough"
     ; reserved "times"
     ; return $ SLoop e cond } <|>
  do { reserved "either"
     ; e <- parens expr
     ; reserved "so"
     ; c1 <- compoundStmt
     ; reserved "or"
     ; c2 <- compoundStmt
     ; reserved "because"; reserved "Alice"; reserved "was"
     ; reserved "unsure"; reserved "which"
     ; return $ SIf [(e, c1), (ENot e, c2)] } <|>
  do { reserved "perhaps"
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
           ; return . SIf $ (e, cst) : es }) <|>
       (do { reserved "or"
           ; elsest <- compoundStmt
           ; reserved "because"; reserved "Alice"; reserved "was"
           ; reserved "unsure"; reserved "which"
           ; return $ SIf $ (e, cst) : es ++ [(EEq (EInt 0) (EInt 0), elsest)]})
       -- TODO: Remove this ugly hack for else (make if clause data type?)
     }

vtype :: Parser Type
vtype = lexeme $ (
  (reserved "number" >> return Number) <|>
  (reserved "letter" >> return Letter) <|>
  (reserved "sentence" >> return Sentence) <|>
  do { reserved "spider"; t <- vtype; return $ RefType t } <?>
  "valid type name" )

expr :: Parser Expr
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
  do { num <- intLit; return $ EInt num } <|>
  do { var <- identifier
     ; (do { reserved "'s"
           ; ix <- expr
           ; reserved "piece"
           ; return $ EArrRef var ix }) <|>
       (do { args <- actualParams
           ; return $ ECall var args }) <|>
       (do { notFollowedBy $
             reserved "'s" <|> (actualParams >> return ())
           ; return $ EId var }) } <|>
  do { str <- stringLit; return $ EString str } <|>
  do { char <- charLit; return $ EChar char }

actualParams :: Parser ActualParams
actualParams = try . lexeme $ do
  aps <- parens $ commaSep expr
  return $ APList aps

terminator :: Parser ()
terminator =
  reserved "." <|>
  reserved "," <|>
  reserved "and" <|>
  reserved "but" <|>
  reserved "then" <?>
  "statement terminator"

parseMAlice :: String -> String -> String
parseMAlice code name = do
  case parse maliceParse name code of
    Left err -> "Parse error in " ++ show err
    Right ast -> "Success: " ++ show ast