import qualified Data.Map as Map
import Data.Map(Map)
import Control.Monad
import Control.Monad.Writer
import Data.List(permutations)

-- 1.
type Pos=(Int,Int)
data Dir=N|S|E|W deriving(Eq,Ord,Show)
type Maze=Map Pos(Map Dir Pos)

-- 1a
move::Maze->Pos->Dir->Maybe Pos
move maze p d=Map.lookup p maze>>=Map.lookup d

-- 1b
followPath::Maze->Pos->[Dir]->Maybe Pos
followPath maze p ds=foldM(move maze)p ds

-- 1c
safePath::Maze->Pos->[Dir]->Maybe[Pos]
safePath maze p ds=go p ds[p]
 where
  go p[] acc=Just(reverse acc)
  go p(d:rest) acc=do
   p2<-move maze p d
   go p2 rest(p2:acc)

-- 2.
type Key=Map Char Char

decrypt::Key->String->Maybe String
decrypt key=traverse(\c->Map.lookup c key)

decryptWords::Key->[String]->Maybe[String]
decryptWords key=traverse(decrypt key)

-- 3. 
type Guest=String
type Conflict=(Guest,Guest)

seatings::[Guest]->[Conflict]->[[Guest]]
seatings guests conflicts=do
 s<-permutations guests
 guard(valid s)
 return s
 where
  bad a b=(a,b)`elem`conflicts||(b,a)`elem`conflicts
  pairs xs=zip xs(tail xs++[head xs])
  valid[] =True
  valid xs=all(\(a,b)->not(bad a b))(pairs xs)

-- 4.
data Result a=Failure String|Success a[String] deriving(Show)

-- 4a
instance Functor Result where
 fmap _ (Failure msg)=Failure msg
 fmap f (Success x ws)=Success(f x)ws

instance Applicative Result where
 pure x=Success x[]
 Failure msg<*>_=Failure msg
 _<*>Failure msg=Failure msg
 Success f ws1<*>Success x ws2=Success(f x)(ws1++ws2)

instance Monad Result where
 Failure msg>>=_=Failure msg
 Success x ws1>>=f=case f x of
  Failure msg->Failure msg
  Success y ws2->Success y(ws1++ws2)

-- 4b
warn::String->Result()
warn msg=Success()[msg]

failure::String->Result a
failure msg=Failure msg

-- 4c
validateAge::Int->Result Int
validateAge n
 |n<0=failure"negative age"
 |n>150=do
  warn"age above 150"
  return n
 |otherwise=return n

validateAges::[Int]->Result[Int]
validateAges=mapM validateAge

-- 5. Writer
data Expr=Lit Int|Add Expr Expr|Mul Expr Expr|Neg Expr deriving(Show,Eq)

simplify::Expr->Writer[String]Expr
simplify(Lit n)=return(Lit n)
simplify(Add a b)=do
 a'<-simplify a
 b'<-simplify b
 case(a',b')of
  (Lit 0,e)->tell["Add identity: 0 + e -> e"]>>return e
  (e,Lit 0)->tell["Add identity: e + 0 -> e"]>>return e
  (Lit x,Lit y)->tell["Constant folding: a + b"]>>return(Lit(x+y))
  _->return(Add a' b')
simplify(Mul a b)=do
 a'<-simplify a
 b'<-simplify b
 case(a',b')of
  (Lit 1,e)->tell["Mul identity: 1 * e -> e"]>>return e
  (e,Lit 1)->tell["Mul identity: e * 1 -> e"]>>return e
  (Lit 0,_)->tell["Zero absorption: 0 * e -> 0"]>>return(Lit 0)
  (_,Lit 0)->tell["Zero absorption: e * 0 -> 0"]>>return(Lit 0)
  (Lit x,Lit y)->tell["Constant folding: a * b"]>>return(Lit(x*y))
  _->return(Mul a' b')
simplify(Neg e)=do
 e'<-simplify e
 case e' of
  Neg x -> tell["Double negation: --e -> e"]>>return x
  _ -> return(Neg e')