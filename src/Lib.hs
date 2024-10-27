module Lib
    ( someFunc,
      Expr(..),
      Program,
      Token(..),
      Prim(..),
      Value(..),
      lexTokens,
      parse,
      evalStep,
      testStep
    ) where

import Data.String
import Data.Char -- isSpace, isDigit, isAlpha

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Expr = LInt Integer
          | LSym String
          | SExpr [Expr]
          | EV Value
          | EClosure [String] Expr StepEnv
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
          | If
          deriving (Eq, Show)

data Value = VInt Integer
           | VSym String
           | VPrim Prim
           | VList [Value]
           | VClosure [String] Expr Env
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
evalPrim Cons [x] = VList [x] -- unary cons, for creating lists
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
evalPrim Cons [x, VList xs] = VList (x:xs)
-- Ternary
evalPrim If [VInt 0, _, z] = z
evalPrim If [_, y, _] = y

prim :: String -> Prim
prim "not" = Not
prim "car" = Car
prim "cdr" = Cdr
prim "+" = Add
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
prim "if" = If

isValue :: Expr -> Bool
isValue (EV _) = True
isValue _ = False

-- Evaluate expressions (step by step) ########################################

type StepEnv = [(String, Expr)]
type Step = (Expr, StepEnv)

isDone :: Step -> Bool
isDone s = evalStep s == s
-- isDone (EV _, _) = True
-- isDone (EClosure _ _ _, _) = True
-- isDone _ = False

-- Proper lambda calculus this time
-- Valid terms:
-- 1. x
-- 2. \x.e
-- 3. e e
-- Function application is left associative
evalStep :: Step -> Step
-- 1. x
evalStep (EV v, env) = (EV v, env)
evalStep (EClosure [] body env', env) = (subst body env', env) -- Substitute bound variables
  where subst (LSym s) env = case lookup s env of
          Just e -> e
          Nothing -> LSym s
        subst (SExpr xs) env = SExpr (map (\x -> subst x env) xs)
        subst (EClosure names body env') env = EClosure names (subst body env) env'
        subst e _ = e
evalStep (EClosure names body env', env) = (EClosure names body env', env)
-- Literals
evalStep (LInt n, env) = (EV (VInt n), env)
-- Variables
evalStep (LSym s, env) = case lookup s env of
  Just v -> (v, env)
  Nothing -> (LSym s, env)
-- Let bindings
evalStep (SExpr [LSym "let", SExpr [LSym x, e], body], env) | isDone (e, env) = (body, (x, e):env)
evalStep (SExpr [LSym "let", SExpr [LSym x, e], body], env) = (body, (x, fst $ evalStep (e, env)):env)
-- 2. \x.e
-- Lambda
evalStep (SExpr [LSym "lambda", SExpr names, body], env) = (EClosure (map (\(LSym s) -> s) names) body [], env)
-- 3. e e
-- Function application
-- Separate first argument
evalStep (SExpr (f : x : y : xs), env) | isDone (f, env) && isDone (x, env) = (SExpr (f' : y : xs), env')
  where (f', env') = evalStep (SExpr [f, x], env)
-- Evaluate first argument
evalStep (SExpr (f : x : xs), env) | not $ isDone (x, env) = (SExpr (f : (fst $ evalStep (x, env)) : xs), env)
-- Evaluate function
evalStep (SExpr (f : x : xs), env) | not $ isDone (f, env) = (SExpr (fst (evalStep (f, env)) : x : xs), env)
-- Apply function
evalStep (SExpr [f, x], env) = case f of
  EClosure (name:names) body env' -> (EClosure names body ((name, x):env'), env)
  -- EV (VPrim p) -> (primToClosure p x, env)
  _ -> error "evalStep: invalid function application"

-- primToClosure :: Prim -> Expr -> Expr


test :: String -> Step
test s = eval (parse (lexTokens s), [])
  where eval x = if isDone x then x else eval (evalStep x)

-- Test #######################################################################

testStep :: String -> Step
testStep s = (parse (lexTokens s), [])
