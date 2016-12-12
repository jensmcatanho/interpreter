import Html exposing (text)

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

-- p1 returns x if it's been declared and 100 otherwise. 
p1 : Prog
p1 = Seq
        (Attr "x" (Add (Num 11) (Num 9)))
--        (Attr "y" (Add (Num 11) (Num 9)))

        (If_Else (Var "x")
            (Attr "ret" (Var "x"))
            (Attr "ret" (Num 100))
        )

-- p2 calculates the sum of the first n-1 natural numbers.
p2 : Prog
p2 = Seq
        (Seq
            (Attr "i" (Num 1))
            (Attr "n" (Num 10))
        )

        (While (Less_Than (Var "i") (Var "n"))
            (Seq
                (Attr "ret" (Add (Var "ret") (Var "i")))
                (Attr "i" (Add (Var "i") (Num 1)))
            )
        )

main = text (toString (lang p1))
