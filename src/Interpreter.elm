module Interpreter exposing (Exp(..), Prog(..), lang)

type alias Env = (String -> Int)
zero : Env
zero = \ask -> 0

type Exp = Add  Exp Exp
         | Sub  Exp Exp
         | Mult Exp Exp
         | Div  Exp Exp
         | Mod  Exp Exp
         | Pow  Exp Exp
         | Neg  Exp
         | LessThan         Exp Exp
         | LessThanEqual    Exp Exp
         | GreaterThan      Exp Exp
         | GreaterThanEqual Exp Exp
         | EqualTo          Exp Exp
         | NotEqualTo       Exp Exp
         | And Exp Exp
         | Or  Exp Exp
         | Xor Exp Exp
         | Not Exp
         | Num Int
         | Var String

type Prog = Attr    String Exp
          | Seq     Prog Prog
          | IfElse Exp Prog Prog
          | While   Exp Prog

evalExp : Exp -> Env -> Int
evalExp exp env =
    case exp of
        -- Arithmetic operators
        Add exp1 exp2 ->
            (evalExp exp1 env) + (evalExp exp2 env)
        Sub exp1 exp2 ->
            (evalExp exp1 env) - (evalExp exp2 env)
        Mult exp1 exp2 ->
            (evalExp exp1 env) * (evalExp exp2 env)
        Div exp1 exp2 ->
            (evalExp exp1 env) // (evalExp exp2 env)
        Mod exp1 exp2 ->
            (evalExp exp1 env) % (evalExp exp2 env)
        Pow exp1 exp2 ->
            (evalExp exp1 env) ^ (evalExp exp2 env)
        Neg exp1 ->
            Basics.negate (evalExp exp1 env)

        -- Relational operators
        LessThan exp1 exp2 ->
            if (evalExp exp1 env) < (evalExp exp2 env) then 1 else 0
        LessThanEqual exp1 exp2 ->
            if (evalExp exp1 env) <= (evalExp exp2 env) then 1 else 0
        GreaterThan exp1 exp2 ->
            if (evalExp exp1 env) > (evalExp exp2 env) then 1 else 0
        GreaterThanEqual exp1 exp2 ->
            if (evalExp exp1 env) >= (evalExp exp2 env) then 1 else 0
        EqualTo exp1 exp2 ->
            if (evalExp exp1 env) == (evalExp exp2 env) then 1 else 0
        NotEqualTo exp1 exp2 ->
            if (evalExp exp1 env) /= (evalExp exp2 env) then 1 else 0

        -- Logical operators
        And exp1 exp2 ->
            if (evalExp exp1 env) /= 0 && (evalExp exp2 env) /= 0 then 1 else 0
        Or exp1 exp2 ->
            if (evalExp exp1 env) /= 0 || (evalExp exp2 env) /= 0 then 1 else 0
        Xor exp1 exp2 ->
            if Basics.xor ((evalExp exp1 env) /= 0) ((evalExp exp2 env) /= 0) then 1 else 0
        Not exp1 ->
            if (evalExp exp1 env) == 0 then 1 else 0

        Num  num       -> num
        Var  var       -> (env var)

evalProg : Prog -> Env -> Env
evalProg prog env =
    case prog of 
        Seq  s1  s2  -> (evalProg s2 (evalProg s1 env))

        Attr var exp ->
            let val = (evalExp exp env)
            in \ask -> if ask == var then val else (env ask)

        IfElse exp s1 s2 ->
            if (evalExp exp env) /= 0 then (evalProg s1 env) else (evalProg s2 env)

        While exp s1 ->
            if (evalExp exp env) /= 0 then (evalProg (While exp s1) (evalProg s1 env)) else env

lang : Prog -> Int
lang p = ((evalProg p zero) "ret")