newtype Reader r a = Reader { runReader :: r -> a }


-- ## The Reader Monad

-- 1. **Functor, Applicative, and Monad instances**

--    Implement the three standard instances for `Reader r`:
instance Functor (Reader r) where
--      -- fmap :: (a -> b) -> Reader r a -> Reader r b
fmap f(Reader g)=Reader(\env->f(g env))
--      fmap = undefined

instance Applicative (Reader r) where
--      -- pure   :: a -> Reader r a
--      pure   = undefined
pure x=Reader(\_->x)
--      -- liftA2 :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
--      liftA2 = undefined
liftA2 f(Reader g)(Reader h)=Reader(\env->f(g env)(h env))


instance Monad (Reader r) where
--      -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
--      (>>=) = undefined
 Reader g>>=f=Reader(\env->
  let x=g env
      Reader h=f x
  in h env)

--  2. **Primitive operations**

--    Implement the basic `Reader` primitives — these are the only "public" interface you
--    should need to write the rest of the code; once they are in place, prefer them (and
--    `do`-notation) over pattern-matching on the `Reader` constructor directly.
--    -- Retrieves the entire environment.
ask   :: Reader r r
ask=Reader(\env->env)

--    -- Retrieves a value derived from the environment by applying a projection,
--    -- e.g. `asks interestRate :: Reader BankConfig Double`.
asks  :: (r -> a) -> Reader r a
asks f=Reader(\env->f env)

--    -- Runs a subcomputation in a locally modified environment. The modification
--    -- is only visible inside the passed Reader — once it returns, the outer
--    -- environment is restored (conceptually; there is no mutable state, the
--    -- modified environment simply goes out of scope).
local :: (r -> r) -> Reader r a -> Reader r a
local f(Reader g)=Reader(\env->g(f env))

-- 3. **A practical example — banking system**

--    Consider a small banking system where the bank's configuration (interest rate,
--    fees, limits) is the read-only environment shared by every operation:
data BankConfig = BankConfig
  { interestRate   :: Double  -- annual interest rate (e.g. 0.05 for 5%)
  , transactionFee :: Int     -- flat fee charged per transaction
  , minimumBalance :: Int     -- minimum required balance on an account
  } deriving (Show)

data Account = Account
  { accountId :: String       -- account identifier
  , balance   :: Int          -- current balance
  } deriving (Show)

--    Implement the following four functions using the `Reader` monad. Prefer `ask` / `asks`
--    and `do`-notation over pattern-matching on the `Reader` constructor — this is what
--    makes the monadic style pay off:
--    -- Computes the interest accrued on the account, based on the configured rate.
--    -- The result should be an Int — round or truncate as you see fit, but be consistent.
calculateInterest   :: Account -> Reader BankConfig Int
calculateInterest acc=do
rate<-asks interestRate
return(floor(fromIntegral(balance acc)*rate))

--    -- Deducts the transaction fee from the account and returns the updated account.
--    -- The accountId should remain unchanged.
applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc=do
fee<-asks transactionFee
return acc{balance=balance acc-fee}

--    -- Checks whether the account balance meets the configured minimum.
checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc=do
minBal<-asks minimumBalance
return(balance acc>=minBal)

--    -- Runs the three operations above on a single account and combines their results.
--    -- The returned tuple contains:
--    --   * the account after the transaction fee has been applied,
--    --   * the interest computed from the ORIGINAL account,
--    --   * whether the ORIGINAL account meets the minimum balance requirement.
processAccount      :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc=do
 accAfterFee<-applyTransactionFee acc
 interest<-calculateInterest acc
 ok<-checkMinimumBalance acc
 return(accAfterFee,interest,ok)
