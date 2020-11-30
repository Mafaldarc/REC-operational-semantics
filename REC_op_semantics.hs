type Var = String
type State = [(Var, Int)]               -- Name, Value

type FVar = String
type Functions = [(FVar, [Var], Term)]  -- Name, Parameters, Declaration

data Term = Num Int
        |   Var Var
        |   Add Term Term
        |   Sub Term Term
        |   Mult Term Term
        |   IfElse Term Term Term
        |   Function FVar [Term]        -- Name of function and Args
        deriving (Show, Eq)

lookupVar :: State -> Var -> Int
lookupVar [] _      = error "var x not defined"
lookupVar (s:y) x 
    | fst s == x    = snd s
    | otherwise     = lookupVar y x

lookupFunction :: Functions -> FVar -> [Term] -> ([Var],Term)
lookupFunction [] _ _                           = error "function f not defined or wrong number of arguments"
lookupFunction ((name,params,t):y) f args
    | name == f && length params == length args = (params, t)
    | otherwise                                 = lookupFunction y f args

-- Create a local state for a function
substitute :: [Var]-> [Term] -> State -> Functions -> State
substitute [] [] _ _ = []
substitute (p:ps) (a:as) s d = interp s d a `seq` (p, interp s d a) : substitute ps as s d


interp :: State -> Functions -> Term  -> Int
interp _ _ (Num n)              = n
interp s d (Add t1 t2)          = interp s d t1 + interp s d t2 
interp s d (Sub t1 t2)          = interp s d t1 - interp s d t2 
interp s d (Mult t1 t2)         = interp s d t1 * interp s d t2 
interp s d (Var x)              = interp s d (Num (lookupVar s x)) 
interp s d (IfElse t1 t2 t3)
    | interp s d t1 == 0        = interp s d t2
    | otherwise                 = interp s d t3
interp s d (Function f args)    = s' `seq` interp s' d t
    where
        (params,t) = lookupFunction d f args
        s' = substitute params args s d