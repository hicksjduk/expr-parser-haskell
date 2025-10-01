{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

module Parser where

import Data.List
import Data.Char
import Text.Regex

parseExpression :: String -> Either String Expression
parseExpression str = ((`pushTokens` []) <$> (tokenize str >>= validateTokens)) >>= extractExpr
  where 
    extractExpr [ExprItem e _] = Right e
    extractExpr s = extractExpr $ pushRParen s

tokenize :: String -> Either String [Token]
tokenize "" = Right []
tokenize str = maybe (unmatched str) matched $ matchRegexAll regex str
  where
    regex = mkRegex "[0-9]+|[ \t\n\r\f\v]+|[-+*/()]"
    unmatched s = Left $ unwords ["Invalid character(s):", show s]
    matched ("", t, suffix, _) = tokenize suffix >>= (prependToken $ read t)
    matched (prefix, _, _, _) = unmatched prefix

validateTokens :: [Token] -> Either String [Token]
validateTokens [] = Left "No expression found"
validateTokens (RParenToken : _) = Left "Right parenthesis cannot start an expression"
validateTokens (OperatorToken{} : _) = Left "Operator cannot start an expression"
validateTokens ts = Right ts

prependToken :: Token -> [Token] -> Either String [Token]
prependToken WhitespaceToken s = Right s
prependToken LParenToken [] = Left "Left parenthesis cannot end an expression"
prependToken OperatorToken{} [] = Left "Operator cannot end an expression"
prependToken t [] = Right [t]
prependToken LParenToken (OperatorToken{}:_) = Left "Left parenthesis cannot precede an operator"
prependToken LParenToken (RParenToken:_) = Left "Left parenthesis cannot precede a right parenthesis"
prependToken OperatorToken{} (OperatorToken{}:_) = Left "Operator cannot precede an operator"
prependToken OperatorToken{} (RParenToken:_) = Left "Operator cannot precede a right parenthesis"
prependToken RParenToken (LParenToken:_) = Left "Right parenthesis cannot precede a left parenthesis"
prependToken RParenToken (NumberToken{}:_) = Left "Right parenthesis cannot precede a number"
prependToken NumberToken{} (LParenToken:_) = Left "Number cannot precede a left parenthesis"
prependToken NumberToken{} (NumberToken{}:_) = Left "Number cannot precede a number"
prependToken t ts = Right $ t : ts

pushTokens :: [Token] -> Stack -> Stack
pushTokens [] = id
pushTokens (t : ts) = (pushTokens ts) . (pushToken t)

pushToken :: Token -> Stack -> Stack
pushToken LParenToken = (LParenItem :)
pushToken RParenToken = pushRParen
pushToken (NumberToken n) = pushExpression (NumberExpression n) Nothing
pushToken (OperatorToken op) = pushOperator op

pushRParen :: Stack -> Stack
pushRParen [ExprItem e _] = [exprItem e $ Just Atomic]
pushRParen ((ExprItem e _) : _ : s) = pushExpression e (Just Atomic) s

pushExpression :: Expression -> Maybe Priority -> Stack -> Stack
pushExpression e p [] = [exprItem e p]
pushExpression e p s@(LParenItem : _) = (exprItem e p) : s
pushExpression rOp p (OpItem op : (ExprItem lOp _) : s) = 
  let newExpr = ArithmeticExpression lOp op rOp
  in pushExpression newExpr p s

pushOperator :: Operation -> Stack -> Stack
pushOperator op s@((ExprItem e pr) : theRest) = if priority op > pr
  then newOp : unpack e ++ theRest
  else newOp : s
  where
    newOp = OpItem op
    unpack (ArithmeticExpression lOp opr rOp) = 
      intersperse (OpItem opr) $ map (`exprItem` Nothing) [rOp, lOp]

exprItem :: Expression -> Maybe Priority -> StackItem
exprItem e Nothing = ExprItem e $ priority e
exprItem e (Just p) = ExprItem e p

data Priority = Low | High | Atomic deriving (Eq, Ord, Show)

class Prioritizable a where
  priority :: a -> Priority

comparePriority :: (Prioritizable a, Prioritizable b) => a -> b -> Ordering
comparePriority a b = compare (priority a) (priority b)

data Operation = Add | Subtract | Multiply | Divide deriving (Enum, Eq, Bounded)

instance Show Operation where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"

instance Read Operation where
  readsPrec _ "+" = [(Add, "")]
  readsPrec _ "-" = [(Subtract, "")]
  readsPrec _ "*" = [(Multiply, "")]
  readsPrec _ "/" = [(Divide, "")]
  readsPrec _ _ = []

eval :: Operation -> Int -> Int -> Int
eval Add = (+)
eval Subtract = (-)
eval Multiply = (*)
eval Divide = div

instance Prioritizable Operation where
  priority Add = Low
  priority Subtract = Low
  priority _ = High

commutative :: Operation -> Bool
commutative = (`elem` [Add, Multiply])

data Expression = NumberExpression Int | ArithmeticExpression Expression Operation Expression

value :: Expression -> Int
value (NumberExpression n) = n
value (ArithmeticExpression left op right) = eval op (value left) (value right)

instance Prioritizable Expression where
  priority (NumberExpression _) = Atomic
  priority (ArithmeticExpression _ op _) = priority op

instance Show Expression where
  show (NumberExpression n) = show n
  show e@(ArithmeticExpression left op right) =
    unwords $ intersperse (show op) operands
    where
      operands = zipWith parensIf [left, right] $ [leftParens e, rightParens e]

leftParens :: Expression -> Bool
leftParens (NumberExpression _) = False
leftParens (ArithmeticExpression left op _) = comparePriority left op == LT

rightParens :: Expression -> Bool
rightParens (NumberExpression _) = False
rightParens (ArithmeticExpression _ op right) =
  case comparePriority right op of
    LT -> True
    EQ -> not $ commutative op
    GT -> False

parensIf :: (Show a) => a -> Bool -> String
parensIf e False = show e
parensIf e True = intercalate (show e) ["(", ")"]

data Token = LParenToken | RParenToken | NumberToken Int | OperatorToken Operation | WhitespaceToken
  deriving (Show)

instance Read Token where
  readsPrec _ "(" = [(LParenToken, "")]
  readsPrec _ ")" = [(RParenToken, "")]
  readsPrec _ str@(ch:_)
    | isDigit ch = [(NumberToken $ read str, "")]
    | isSpace ch = [(WhitespaceToken, "")]
    | ch `elem` "+-*/" = [(OperatorToken $ read str, "")]
    | otherwise = []

data StackItem = LParenItem | ExprItem Expression Priority | OpItem Operation deriving (Show)

type Stack = [StackItem]