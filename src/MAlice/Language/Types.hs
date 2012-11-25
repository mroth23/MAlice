module MAlice.Language.Types where

{- It's impossible to construct boolean types,
   but they are necessary for type checking -}
data Type =
  Number | Letter | Sentence | Boolean | RefType Type
  deriving (Eq)

instance Show Type where
  show Number = "Number"
  show Letter = "Letter"
  show Sentence = "Sentence"
  show Boolean = "Boolean"
  show (RefType t) = "Spider " ++ show t

-- Returns True if the type supports (==) comparison
isEq :: Type -> Bool
isEq Number = True
isEq Letter = True
isEq _      = False

-- Returns True if the type is numeric
isNum :: Type -> Bool
isNum Number = True
isNum _      = False

-- Returns True if the type supports relational comparison
isOrd :: Type -> Bool
isOrd Number = True
isOrd _      = False
