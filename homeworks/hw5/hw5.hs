import Control.Monad.State
import qualified Data.Map as Map
import Data.Map(Map)

-- 1.Stack machine

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG

execInstr::Instr->State[Int]()
execInstr(PUSH x)=modify(x:)
execInstr POP=do
 s<-get
 case s of
  (_:xs)->put xs
  _->return()

execInstr DUP=do
 s<-get
 case s of
  (x:xs)->put(x:x:xs)
  _->return()

execInstr SWAP=do
 s<-get
 case s of
  (x:y:xs)->put(y:x:xs)
  _->return()

execInstr ADD=do
 s<-get
 case s of
  (x:y:xs)->put((x+y):xs)
  _->return()

execInstr MUL=do
 s<-get
 case s of
  (x:y:xs)->put((x*y):xs)
  _->return()

execInstr NEG=do
 s<-get
 case s of
  (x:xs)->put((-x):xs)
  _->return()

execProg::[Instr]->State[Int]()
execProg=mapM_ execInstr

runProg::[Instr]->[Int]
runProg p=execState(execProg p)[]

-- 2. Expression evaluator with variable bindings

data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr   -- bind the value of the expression to the name, return that value
  | Seq  Expr Expr       -- evaluate the left, then the right; return the value of the right

eval::Expr->State(Map String Int)Int
eval(Num n)=return n

eval(Var x)=do
 env<-get
 return(env Map.! x)

eval(Add a b)=do
 x<-eval a
 y<-eval b
 return(x+y)

eval(Mul a b)=do
 x<-eval a
 y<-eval b
 return(x*y)

eval(Neg e)=do
 x<-eval e
 return(-x)

eval(Assign n e)=do
 x<-eval e
 modify(Map.insert n x)
 return x

eval(Seq a b)=do
 eval a
 eval b

runEval::Expr->Int
runEval e=evalState(eval e)Map.empty

-- 3. Memoised edit (Levenshtein) distance

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j=do
 memo<-get
 case Map.lookup(i,j)memo of
  Just v->return v
  Nothing->do
   val<-
    if i==0 then return j
    else if j==0 then return i
    else if xs!!(i-1)==ys!!(j-1)
     then editDistM xs ys(i-1)(j-1)
     else do
      a<-editDistM xs ys(i-1)j
      b<-editDistM xs ys i(j-1)
      c<-editDistM xs ys(i-1)(j-1)
      return(1+minimum[a,b,c])

   modify(Map.insert(i,j)val)
   return val

editDistance::String->String->Int
editDistance xs ys=
 evalState(editDistM xs ys(length xs)(length ys))Map.empty