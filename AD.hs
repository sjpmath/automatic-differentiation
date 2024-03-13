data Func = Const Double | Ident | Add Func Func | Mult Func Func | Scale Double Func -- | Apply Func Double
	deriving Show

data Expr = Val Double | Plus Expr Expr | Times Expr Expr | Apply Func Double
	deriving Show

data Cont = IdCont | AddCont1 Func Double Cont | AddCont2 Expr Cont | MultCont1 Func Func Double Cont | MultCont2 Func Func Double Expr Cont | ScaleCont Double Cont
	deriving Show

apply :: Func -> Double -> Expr
apply (Const r) a = Val r
apply Ident a = Val a
apply (Add f g) a = Plus (apply f a) (apply g a)
apply (Mult f g) a = Times (apply f a) (apply g a)
apply (Scale r f) a = Times (Val r) (apply f a)
--apply (Apply f r) a = apply f r

forwardDerivative :: Func -> (Double, Double) -> Expr
forwardDerivative Ident (a,a') = Val a'
forwardDerivative (Const r) (a,a') = Val 0.0
forwardDerivative (Add f g) (a,a') = Plus (forwardDerivative f (a,a')) (forwardDerivative g (a,a'))
forwardDerivative (Mult f g) (a,a') = Plus (Times (forwardDerivative f (a,a')) (apply g a)) (Times (forwardDerivative g (a,a')) (apply f a))
forwardDerivative (Scale r f) (a,a') = Times (Val r) (forwardDerivative f (a,a'))


backwardDerivative :: Func -> Double -> Cont -> Expr
backwardDerivative Ident a b' = applyCont b' (Val 1.0)
backwardDerivative (Const r) a b' = applyCont b' (Val 0.0)
backwardDerivative (Add f g) a b' = backwardDerivative f a (AddCont1 g a b')
backwardDerivative (Mult f g) a b' = backwardDerivative f a (MultCont1 f g a b')
backwardDerivative (Scale r f) a b' = backwardDerivative f a (ScaleCont r b')

applyCont :: Cont -> Expr -> Expr
applyCont IdCont e = e
applyCont (AddCont1 g a b') e = backwardDerivative g a (AddCont2 e b')
applyCont (AddCont2 e1 b') e = applyCont b' (Plus e1 e)
applyCont (MultCont1 f g a b') e = backwardDerivative g a (MultCont2 f g a e b')
applyCont (MultCont2 f g a e1 b') e = applyCont b' (Plus (Times (Apply f a) (e)) (Times (Apply g a) (e1)))
applyCont (ScaleCont r b') e = applyCont b' (Times (Val r) e)

eval :: Expr -> Double
eval (Val r) = r
eval (Plus r1 r2) = eval r1 + eval r2
eval (Times r1 r2) = eval r1 * eval r2
eval (Apply f r) = eval (apply f r)

main :: IO()
main = putStrLn ("Hello world")

func :: Func
func = Mult Ident Ident

func1 :: Func
func1 = Add (Mult Ident Ident) (Mult (Mult Ident Ident) Ident)

func2 :: Func
func2 = Add (Mult Ident Ident) (Scale 4.0 Ident)

toy1 :: Expr
toy1 = (forwardDerivative func1 (3.0, 1.0))

toyb :: Expr
toyb = backwardDerivative func 3.0 IdCont

toyb1 :: Expr
toyb1 = backwardDerivative func1 3.0 IdCont

toyb2 :: Expr
toyb2 = backwardDerivative func2 3.0 IdCont