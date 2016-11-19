import Html exposing (text)

type Exp = Add  Exp Exp
         | Sub  Exp Exp
         | Mult Exp Exp
         | Num  Int

evalExp : Exp -> Int
evalExp exp =
    case exp of
        Add  exp1 exp2 -> (evalExp exp1) + (evalExp exp2)
        Sub  exp1 exp2 -> (evalExp exp1) - (evalExp exp2)
        Mult exp1 exp2 -> (evalExp exp1) * (evalExp exp2)
        Num  v         -> v

e1 : Exp
e1 = Mult (Num 10) (Num 10)

main = text (toString (evalExp e1))
