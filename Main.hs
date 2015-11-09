{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.Trans.Either
import Data.Aeson
import Data.List
import Data.Maybe
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Debug.Trace

import Evaluator


type API = "eval-monadic" :> Capture "expr" String :> Get '[JSON] String
      :<|> "eval-non-monadic" :> Capture "expr" String :> Get '[JSON] String
      :<|> "test" :> Capture "a" Int :> Capture "b" Int :> Get '[JSON] String
      :<|> "eulers" :> Capture "eulerNumber" Int :> Get '[JSON] String

-- | Euler 1
-- If we list all the natural numbers below 10 that are multiples of 3 or 5,
-- we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.
divisible n d = n `mod` d == 0
euler1 = sum $ filter (\x -> x `divisible` 3 || x `divisible` 5) [1..999]

-- | Euler 2
-- Each new term in the Fibonacci sequence is generated by adding the previous
-- two terms. By starting with 1 and 2, the first 10 terms will be:
-- 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
-- By considering the terms in the Fibonacci sequence whose values do not
-- exceed four million, find the sum of the even-valued terms.
fib 0 = 1
fib 1 = 2
fib n = fib (n - 2) + fib (n - 1)

condsum m fn n s
  | applied >= m = s
  | otherwise = condsum m fn (n+1) (s+appliedmod2)
  where applied = trace (show $ fn n) $ fn n
        appliedmod2 = if applied `mod` 2 == 0 then applied else 0

euler2 = condsum 4000000 fib 0 0

-- | Euler 3
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143?
isprime n = isprime' n 2
  where isprime' n d
          | d >= n = True
          | otherwise = (n `mod` d /= 0) && isprime' n (d + 1)

biggestprime n = biggestprime' n 1 n 0 2
  where biggestprime' o pr n b p -- original, product, current remainder to check, biggest prime, current prime
          | p * p >= o = b
          | o == pr = b
          | isprime p && n `mod` p == 0 = biggestprime' o (pr * p) (n `quot` p) p p
          | otherwise = biggestprime' o pr n b (p + 1)

euler3 = biggestprime 600851475143

-- | Euler 4
-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

ispalindrome n = take l2 nstr == reverse (drop (length nstr - l2) nstr)
  where nstr = show n
        l2 = length nstr `quot` 2

factorpool = [999,998..1] :: [Integer]
palindromeproducts = [x * y | x <- factorpool, y <- factorpool, ispalindrome (x * y)]
euler4 = maximum palindromeproducts


eulerList = [euler1, euler2, euler3, euler4]


server :: Server API
server = evalMonadic
    :<|> evalNonMonadic
    :<|> test
    :<|> eulers

  where evalMonadic :: String -> EitherT ServantErr IO String
        evalMonadic expr = return $ show $ evaluator' expr

        evalNonMonadic :: String -> EitherT ServantErr IO String
        evalNonMonadic expr = return $ show $ evaluator expr

        test a b = return $ show $ isprime a

        eulers eulerNumber = return $ show $ if eulerNumber < length eulerList then eulerList !! eulerNumber else 0


api :: Proxy API
api = Proxy


app :: Application
app = serve api server


main :: IO ()
main = run 8081 app
