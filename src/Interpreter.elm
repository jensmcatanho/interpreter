module Interpreter exposing (Exp(..), Prog(..), lang)

type alias Env = (String -> Int)
zero : Env
zero = \ask -> 0

type Exp = Add  Exp Exp
         | Sub  Exp Exp
         | Mult Exp Exp
         | Div  Exp Exp
         | Less_Than    Exp Exp
         | Greater_Than Exp Exp
         | Equal_To     Exp Exp
         | Num  Int
         | Var  String

type Prog = Attr    String Exp
          | Seq     Prog Prog
          | If_Else Exp Prog Prog
          | While   Exp Prog

evalExp : Exp -> Env -> Int
evalExp exp env =
    case exp of
        -- Mathematical expressions
        Add  exp1 exp2 ->
            (evalExp exp1 env) + (evalExp exp2 env)
        Sub  exp1 exp2 ->
            (evalExp exp1 env) - (evalExp exp2 env)
        Mult exp1 exp2 ->
            (evalExp exp1 env) * (evalExp exp2 env)
        Div  exp1 exp2 ->
            (evalExp exp1 env) // (evalExp exp2 env)

        -- Comparisons
        Less_Than exp1 exp2 ->
            if (evalExp exp1 env) < (evalExp exp2 env) then 1 else 0
        Greater_Than exp1 exp2 ->
            if (evalExp exp1 env) > (evalExp exp2 env) then 1 else 0
        Equal_To exp1 exp2 ->
            if (evalExp exp1 env) == (evalExp exp2 env) then 1 else 0

        Num  num       -> num
        Var  var       -> (env var)

evalProg : Prog -> Env -> Env
evalProg prog env =
    case prog of 
        Seq  s1  s2  -> (evalProg s2 (evalProg s1 env))

        Attr var exp ->
            let val = (evalExp exp env)
            in \ask -> if ask == var then val else (env ask)

        If_Else exp s1 s2 ->
            if (evalExp exp env) /= 0 then (evalProg s1 env) else (evalProg s2 env)

        While exp s1 ->
            if (evalExp exp env) /= 0 then (evalProg (While exp s1) (evalProg s1 env)) else env

lang : Prog -> Int
lang p = ((evalProg p zero) "ret")