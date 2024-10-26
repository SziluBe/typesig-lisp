module Lib
    ( someFunc,
      Expr(..),
      Program,
      Token(..),
      Prim(..),
      Value(..),
      lexTokens,
      parse,
      eval,
      test,
      runTests,
      evalStep
    ) where

import Data.String
import Data.Char -- isSpace, isDigit, isAlpha

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Expr = LInt Integer
          | LSym String
          | SExpr [Expr]
          | EV Value
          | EClosure [String] Expr
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

-- Evaluate expressions

eval :: Expr -> Env -> Value
-- Unwrap
eval (SExpr [e]) env = eval e env
-- Literals
eval (LInt n) _ = VInt n
-- Let bindings
eval (SExpr [LSym "let", SExpr [LSym x, e], body]) env = eval body ((x, eval e env):env)
-- Variables
eval (LSym s) env = case lookup s env of
  Just v -> v
  Nothing -> VSym s
-- -- Lambdas (with partial application)
-- eval (SExpr (LSym "lambda" : SExpr names : body : args)) env | length args < length names = VClosure names'' body env'
--                                                              | otherwise = eval (SExpr (body : restArgs)) (env' ++ env)
--   where env' = zip names' (map (`eval` env) immediateArgs)
--         (names', names'') = splitAt (length args) $ map (\(LSym s) -> s) names
--         (immediateArgs, restArgs) = splitAt (length names) args
-- Lambdas
eval (SExpr [LSym "lambda", SExpr names, body]) env = VClosure (map (\(LSym s) -> s) names) body env
-- Function application
eval (SExpr (LSym s : args)) env = case lookup s env of
  -- Closures
  Just (VClosure names body env') | length args < length names -> VClosure names'' body env'
                                  | otherwise -> eval (SExpr (body : restArgs)) (env' ++ env)
    where env' = zip names' (map (`eval` env) immediateArgs)
          (names', names'') = splitAt (length args) names
          (immediateArgs, restArgs) = splitAt (length names) args
  -- Primitives
  Nothing -> evalPrim (prim s) (map (`eval` env) args)
eval (SExpr ((SExpr [e]) : args)) env = eval (SExpr (e : args)) env
eval x env = error $ "eval: invalid input: '" ++ show x ++ "', env: " ++ show env

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

-- Step by step ###############################################################

type StepEnv = [(String, Expr)]
type Step = (Expr, StepEnv)

evalStep :: Step -> Step
-- Done, no-op
evalStep (EV v, env) = (EV v, env)
-- Literals
evalStep (LInt n, env) = (EV (VInt n), env)
-- Let bindings
evalStep (SExpr [LSym "let", SExpr [LSym x, e], body], env) | isValue e = (body, (x, e):env)
evalStep (SExpr [LSym "let", SExpr [LSym x, e], body], env) = (body, (x, fst $ evalStep (e, env)):env)
-- Variables
evalStep (LSym s, env) = case lookup s env of
  Just v -> (v, env)
  Nothing -> (EV $ VSym s, env)
-- Lambdas
evalStep (SExpr (LSym "lambda" : SExpr names : body : args), env) | all isValue immediateArgs = (SExpr $ body : restArgs, zip names' immediateArgs ++ env)
  where (immediateArgs, restArgs) = splitAt (length names) args
        names' = map (\(LSym s) -> s) names
evalStep (SExpr (LSym "lambda" : SExpr names : body : args), env) = (SExpr (LSym "lambda" : SExpr names : body : xyz), env)
  where xyz = map es immediateArgs ++ restArgs
        (immediateArgs, restArgs) = splitAt (length names) args
        es (EV v) = EV v
        es e = fst $ evalStep (e, env)
-- Primitive functions
evalStep (SExpr (LSym s : xs), env) | all isValue xs = (EV (evalPrim (prim s) (map (\(EV v) -> v) xs)), env)
evalStep (SExpr (LSym s : xs), env) = (SExpr (LSym s : xyz), env)
  where xyz = map es xs
        es (EV v) = EV v
        es e = fst $ evalStep (e, env)
-- -- Lambda application
-- evalStep (SExpr (f : xs), env) = case fst $ evalStep (f, env) of
--   EClosure ys body -> (body, zip ys xs ++ env)
--   v -> (SExpr (v : xyz), env)
--   where xyz = map es xs
--         es (EV v) = EV v
--         es e = fst $ evalStep (e, env)

-- Test #######################################################################

testStep :: String -> Step
testStep s = (parse (lexTokens s), [])
