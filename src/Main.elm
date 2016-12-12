import Html exposing (text)
import Interpreter exposing (Exp(..), Prog(..), lang)

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
