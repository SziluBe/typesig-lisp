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
import Data.Bifunctor

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
          | Cons | Car | Cdr | LList
          | If | NONE
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

pretty :: Expr -> String
pretty (LInt n) = show n
pretty (LSym s) = s
pretty (SExpr xs) = "(" ++ unwords (map pretty xs) ++ ")"
pretty (EV v) = show v
pretty (EClosure names body env) = "(lambda (" ++ unwords names ++ ") " ++ pretty body ++ "<" ++ show env ++ ">)"

prettyp :: (Expr, StepEnv) -> (String, [(String, String)])
prettyp = Data.Bifunctor.bimap pretty (map (Data.Bifunctor.second pretty)) -- prettyp x = (pretty $ fst x, map (\y -> (fst y, pretty $ snd y)) $ snd x)

-- Evaluate ###################################################################

type Env = [(String, Value)]

-- Primitive functions

evalPrim :: Prim -> [Value] -> Value
-- Unary
evalPrim Not [VInt x] = VInt (if x == 0 then 1 else 0)
-- evalPrim Car [VList (x:_)] = x
-- evalPrim Cdr [VList (_:xs)] = VList xs
-- evalPrim LList [x] = VList [x]
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
-- evalPrim Cons [x, VList xs] = VList (x:xs)
-- -- Ternary
-- evalPrim If [VInt 0, _, z] = z
-- evalPrim If [_, y, _] = y
evalPrim p args = error ("evalPrim: invalid arguments: " ++ show p ++ "; " ++ show args)

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
prim _ = NONE

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

lookup'                  :: String -> [(String,Expr)] -> Maybe Expr
lookup' _key []          =  Nothing
lookup'  key ((x,y):xys)
    | key == x = case y of
                    LSym s -> lookup' s xys
                    _ -> Just y
    | otherwise          =  lookup key xys

-- Proper lambda calculus this time
-- Valid terms:
-- 1. x
-- 2. \x.e
-- 3. e e
-- Function application is left associative
evalStep :: Step -> Step
-- 1. x
evalStep (EV v, env) = (EV v, env)
-- Literals
evalStep (LInt n, env) = (EV (VInt n), env)
-- Variables
evalStep (LSym s, env) = case lookup' s env of -- Implicit shadowing of variable names (last binding is used) through list order (always prepend to env) and lookup
  Just v -> (v, env)
  -- Primitive functions
  Nothing | p /= NONE -> (EV $ VPrim p, env)
    where p = prim s
  Nothing -> (LSym s, env)
-- Let bindings
evalStep (SExpr [LSym "let", SExpr [LSym x, e], body], env) = (body, (x, e):env)
-- 2. \x.e
-- Lambda
evalStep (SExpr [LSym "lambda", SExpr names, body], env) = (EClosure names' body [], env)
  where names' = map unwrap names
        unwrap (LSym s) = s
        -- unwrap (EV (VSym s)) = s
        unwrap x = error $ "evalStep: invalid name: " ++ show x ++ ", in: " ++ show names
-- 3. e e
-- Unwrap SExpr
evalStep (SExpr [e], env) = (e, env)
-- Substitute into each term
-- evalStep (SExpr xs, env) | not $ all (isDone . (, env)) xs = (SExpr $ (x' : map (fst . evalStep . (, env)) xs), env')
--   where (x', env') = evalStep (x, env)
evalStep (SExpr xs, env) | not $ all (isSubbed . (, env)) xs = (SExpr $ map (subst . (, env)) xs, env)
  where subst (x, env) = case x of
          SExpr [LSym "lambda", SExpr names, body] -> EClosure (map (\(LSym s) -> s) names) body []
          LSym s -> case lookup' s env of
            Just v -> v
            Nothing -> x
          EClosure names body env' -> EClosure names (subst (body, env' ++ env)) env'
          SExpr xs -> SExpr (map (subst . (, env)) xs)
          _ -> x
        isSubbed (x, env) = (subst (x, env)) == x
-- Function application
evalStep (SExpr (f : args), env) | not (isDone (f, env)) = (SExpr $ f' : args, env')
  where (f', env') = evalStep (f, env)
evalStep (SExpr (f : args), env) = case f of
  EClosure names body env' | length args < length names -> (EClosure (drop (length args) names) body (zip names args ++ env'), env)
                           | length args >= length names -> (SExpr (body : drop (length names) args), zip names args ++ env' ++ env)
  EV (VPrim If) | [x, y, z] <- args -> if isDone (x, env)
                    then (if x == EV (VInt 0) then (z, env) else (y, env))
                    else (SExpr [EV (VPrim If), fst (evalStep (x, env)), y, z], env)
  EV (VPrim p) | all isValue args -> (EV (evalPrim p (map unwrap args)), env)
               | otherwise -> (SExpr (EV (VPrim p) : map (fst . evalStep . (, env)) args), env)
               where unwrap (EV v) = v
                     unwrap x = error $ "evalStep: invalid argument: " ++ show x
  _ -> error $ "evalStep: invalid function: " ++ show f ++ ", in: " ++ show (f : args)
evalStep s = s



primExprToClosure :: Prim -> Expr
primExprToClosure p | unary p = EClosure ["x"] (SExpr [EV (VPrim p), LSym "x"]) []
                    | binary p = EClosure ["x", "y"] (SExpr [EV (VPrim p), LSym "x", LSym "y"]) []
                    | ternary p = EClosure ["x", "y", "z"] (SExpr [EV (VPrim p), LSym "x", LSym "y", LSym "z"]) []
                    | otherwise = error "primToClosure: invalid primitive"
  where unary p = p `elem` [Not, Car, Cdr, LList]
        binary p = p `elem` [Add, Sub, Mul, Div, Mod, Eq, Ne, Lt, Gt, Le, Ge, And, Or, Cons]
        ternary p = p == If

test :: String -> Step
test s = eval (parse (lexTokens s), [])
  where eval x = if isDone x then x else eval (evalStep x)

-- Test #######################################################################

testStep :: String -> Step
testStep s = (parse (lexTokens s), [])
