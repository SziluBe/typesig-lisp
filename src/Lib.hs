module Lib
    ( someFunc,
      Expr(..),
      Program,
      Token(..),
      Prim(..),
      Value(..),
      lexTokens,
      parse,
      eval
    ) where

import Data.String
import Data.Char -- isSpace, isDigit, isAlpha

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Expr = LInt Integer
          | LSym String
          | SExpr [Expr]
          deriving (Eq, Show)
type Program = [Expr]

data Token = TInt Integer
           | TSym String
           | TLParen
           | TRParen
           deriving (Eq, Show)

data Prim = Add | Sub | Mul | Div | Mod
          | Eq | Ne | Lt | Gt | Le | Ge
          | And | Or | Not
          | Cons | Car | Cdr
          | If | Let | Lambda
          deriving (Eq, Show)

data Value = VInt Integer
           | VSym String
           | VPrim Prim
           | VList [Value]
           | VClosure [String] Expr Env -- not yet implemented
           deriving (Eq, Show)

-- Construct AST ##############################################################

isValidSymbol :: Char -> Bool
isValidSymbol '(' = False
isValidSymbol ')' = False
isValidSymbol c = not (isSpace c)

lexTokens :: String -> [Token]
lexTokens [] = []
lexTokens ('(':xs) = TLParen : lexTokens xs
lexTokens (')':xs) = TRParen : lexTokens xs
lexTokens (x:xs) | isSpace x = lexTokens xs
lexTokens (x:xs) | isDigit x = TInt (read num) : lexTokens rest
  where (num, rest) = span isDigit (x:xs)
lexTokens (x:xs) | isValidSymbol x = TSym sym : lexTokens rest
  where (sym, rest) = span isValidSymbol (x:xs)
lexTokens _ = error "lexTokens: invalid input"

parse :: [Token] -> Expr
parse = fst . parseProgram

parseProgram :: [Token] -> (Expr, [Token])
parseProgram (TLParen:xs) = (SExpr exprs, rest)
  where (exprs, rest) = parseExprs xs
parseProgram _ = error "parseProgram: invalid input"

parseExprs :: [Token] -> ([Expr], [Token])
parseExprs (TRParen:xs) = ([], xs)
parseExprs xs = (expr:exprs, rest)
  where (expr, rest1) = parseExpr xs
        (exprs, rest) = parseExprs rest1

parseExpr :: [Token] -> (Expr, [Token])
parseExpr (TInt n:xs) = (LInt n, xs)
parseExpr (TSym s:xs) = (LSym s, xs)
parseExpr (TLParen:xs) = (SExpr exprs, rest)
  where (exprs, rest) = parseExprs xs
parseExpr _ = error "parseExpr: invalid input"

-- Evaluate ###################################################################

type Env = [(String, Value)]

-- Primitive functions

evalPrim :: Prim -> [Value] -> Value
-- Unary
evalPrim Not [VInt x] = VInt (if x == 0 then 1 else 0)
evalPrim Car [VList (x:_)] = x
evalPrim Cdr [VList (_:xs)] = VList xs
-- Binary
evalPrim Add [VInt x, VInt y] = VInt (x + y)
evalPrim Sub [VInt x, VInt y] = VInt (x - y)
evalPrim Mul [VInt x, VInt y] = VInt (x * y)
evalPrim Div [VInt x, VInt y] = VInt (x `div` y)
evalPrim Mod [VInt x, VInt y] = VInt (x `mod` y)
evalPrim Eq [VInt x, VInt y] = VInt (if x == y then 1 else 0)
evalPrim Ne [VInt x, VInt y] = VInt (if x /= y then 1 else 0)
evalPrim Lt [VInt x, VInt y] = VInt (if x < y then 1 else 0)
evalPrim Gt [VInt x, VInt y] = VInt (if x > y then 1 else 0)
evalPrim Le [VInt x, VInt y] = VInt (if x <= y then 1 else 0)
evalPrim Ge [VInt x, VInt y] = VInt (if x >= y then 1 else 0)
evalPrim And [VInt x, VInt y] = VInt (if x /= 0 && y /= 0 then 1 else 0)
evalPrim Or [VInt x, VInt y] = VInt (if x /= 0 || y /= 0 then 1 else 0)
evalPrim Cons [x] = VList [x] -- unary cons, for creating lists
evalPrim Cons [x, VList xs] = VList (x:xs)
-- Ternary
evalPrim If [VInt 0, _, z] = z
evalPrim If [_, y, _] = y
-- evalPrim Lambda [VList xs, y, _] = VClosure [x | VSym x <- xs] y [] -- broken

-- Evaluate expressions

eval :: Expr -> Env -> Value
-- Literals
eval (LInt n) _ = VInt n
-- Let bindings
eval (SExpr [LSym "let", SExpr [LSym x, e], body]) env = eval body ((x, eval e env):env)
-- Variables
eval (LSym s) env = case lookup s env of
  Just v -> v
  Nothing -> VSym s
-- Unary primitives
eval (SExpr [LSym s, x]) env = evalPrim (prim s) [eval x env]
  where prim "not" = Not
        prim "car" = Car
        prim "cdr" = Cdr
        prim "cons" = Cons
-- Binary primitives
eval (SExpr [LSym s, x, y]) env = evalPrim (prim s) [eval x env, eval y env]
  where prim "+" = Add
        prim "-" = Sub
        prim "*" = Mul
        prim "/" = Div
        prim "%" = Mod
        prim "==" = Eq
        prim "!=" = Ne
        prim "<" = Lt
        prim ">" = Gt
        prim "<=" = Le
        prim ">=" = Ge
        prim "&&" = And
        prim "||" = Or
        prim "cons" = Cons
-- Ternary primitives
eval (SExpr [LSym s, x, y, z]) env = evalPrim (prim s) [eval x env, eval y env, eval z env]
  where prim "if" = If

-- Test #######################################################################

test :: String -> Value
test s = eval (parse (lexTokens s)) []

runTests :: IO ()
runTests = print (test "(+ 1 2)") >>
           print (test "(let (x 1) (+ x 2))") >>
           print (test "(let (x 1) (let (y 2) (+ x y)))") >>
           print (test "(let (x 1) (let (y 2) (let (z 3) (+ x (+ y z)))))") >>
           print (test "(let (x 1) (let (y 2) (let (z 3) (if 0 x y))))") >> -- if false
           print (test "(let (x 1) (let (y 2) (let (z 3) (if 1 y x))))") >> -- if true
           -- List operations
           print (test "(car (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 ))))))") >>
           print (test "(cdr (cons 1 (cons 2 (cons 3 (cons 4 (cons 5))))))") >>
           print (test "(let (x (cons 1 (cons 2 (cons 3 (cons 4 (cons 5)))))) (car x))")
