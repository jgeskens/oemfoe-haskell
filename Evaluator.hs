-- | Main entry point to the application.
module Evaluator where

import           Control.Applicative
import           Control.Monad.Writer
import           Data.List
import           Text.Read
import           Data.Maybe
import           Debug.Trace

data Token = TEquals
           | TPlus
           | TMinus
           | TMultiply
           | TDivision
           | TInteger Integer
           | TDouble Double
           | TVar String
           | TLParen
           | TRParen
           deriving (Show, Eq)


makeToken :: String -> Token
makeToken "+" = TPlus
makeToken "-" = TMinus
makeToken "*" = TMultiply
makeToken "/" = TDivision
makeToken "=" = TEquals
makeToken "(" = TLParen
makeToken ")" = TRParen
makeToken s = case readMaybe s :: Maybe Integer of
    Just i -> TInteger i
    Nothing -> case readMaybe s :: Maybe Double of
        Just d -> TDouble d
        Nothing -> TVar s


tokenize :: String -> String -> [String]
tokenize "" "" = []
tokenize "" token = [token]
tokenize (x:xs) token
    | x `elem` "+-*/=()" = split
    | x == ' ' = ignore
    | otherwise = accumulate
    where split = (if token /= "" then [token, [x]] else [[x]]) ++ tokenize xs ""
          ignore = tokenize xs token
          accumulate = tokenize xs $ token ++ [x]


operators = [
    [TLParen],
    [TEquals],
    [TPlus, TMinus],
    [TMultiply, TDivision]
    ]


infixOperators = [TEquals, TPlus, TMinus, TMultiply, TDivision]


isInfix :: Token -> Bool
isInfix = (`elem` infixOperators)


type Assignment = (String, Token)


-- | Untrace will fake a trace call but just let the value pass without logging.
-- | Useful for temporarily disabling debug tracing.
untrace :: String -> a -> a
untrace _ = id

-- | scanUntilMatchingParen :: toscan -> (betweennParens, remainder)
scanUntilMatchingParen :: [Token] -> ([Token], [Token])
scanUntilMatchingParen toscan = untilMatchingParen toscan [] 1
    where untilMatchingParen [] tokens level = ([], tokens)
          untilMatchingParen (TRParen:xs) tokens level = if level == 1
            then (tokens, xs)
            else untilMatchingParen xs (tokens ++ [TRParen]) (level - 1)
          untilMatchingParen (TLParen:xs) tokens level = untilMatchingParen xs (tokens ++ [TLParen]) (level + 1)
          untilMatchingParen (x:xs) tokens level = untilMatchingParen xs (tokens ++ [x]) level


opTokenToFn :: Num a => Token -> a -> a -> a
opTokenToFn TPlus = (+)
opTokenToFn TMinus = (-)
opTokenToFn TMultiply = (*)


-- | Monadic version of 'ev'
ev' :: [Token] -> Writer [Assignment] Token
ev' [token] = return token
ev' [TDouble left, TDivision, TDouble right] = return $ TDouble $ left / right
ev' [TInteger left, opToken, TInteger right] = return $ TInteger $ opTokenToFn opToken left right
ev' [TDouble left, opToken, TDouble right] = return $ TDouble $ opTokenToFn opToken left right
ev' [TVar v, TEquals, token] = writer (token, [(v, token)])
ev' (TLParen:tokens) = do
  let (betweenParens, remainder) = scanUntilMatchingParen tokens
  betweenToken <- ev' betweenParens
  ev' $ betweenToken:remainder
ev' tokens = fromMaybe (return $ TInteger 0) $ evsplitss operators tokens
  where splitInfix i search tokens = do
          left <- ev' $ take i tokens
          right <- ev' $ drop (i+1) tokens
          ev' $ left:search:[right]
        splitParen i search tokens = do
          let (betweenParens, remainder) = scanUntilMatchingParen $ drop (i+1) tokens
          betweenToken <- ev' betweenParens
          ev' $ take i tokens ++ betweenToken:remainder
        evsplit search tokens
            | isInfix search = case elemIndex search tokens of
                Just i -> Just $ splitInfix i search tokens
                Nothing -> Nothing
            | not $ isInfix search = case elemIndex search tokens of
                Just i -> Just $ splitParen i search tokens
                Nothing -> Nothing
        evsplits [] tokens = Nothing
        evsplits (s:ss) tokens = case evsplit s tokens of
            Just t -> Just t
            Nothing -> evsplits ss tokens
        evsplitss [] tokens = Nothing
        evsplitss (ss:sss) tokens = case evsplits ss tokens of
            Just t -> Just t
            Nothing -> evsplitss sss tokens


ev :: [Assignment] -> [Token] -> (Token, [Assignment])
ev vars [token] = (token, vars)
ev vars [TDouble left, TDivision, TDouble right] = (TDouble $ left / right, vars)
ev vars [TInteger left, opToken, TInteger right] = (TInteger $ opTokenToFn opToken left right, vars)
ev vars [TDouble left, opToken, TDouble right] = (TDouble $ opTokenToFn opToken left right, vars)
ev vars [TVar v, TEquals, token] = (token, (v, token):vars)
ev vars (TLParen:tokens) = ev (betweenVars ++ vars) $ betweenToken:remainder
  where (betweenParens, remainder) = scanUntilMatchingParen tokens
        (betweenToken, betweenVars) = ev vars betweenParens
ev vars tokens = fromMaybe (TInteger 0, vars) $ evsplitss operators tokens
    where evv x = untrace (show x) $ ev vars x
          splitInfix i search tokens = ev (leftVars ++ rightVars ++ vars) $ leftToken:search:[rightToken]
            where (leftToken, leftVars) = evv $ take i tokens
                  (rightToken, rightVars) = evv $ drop (i+1) tokens
          splitParen i search tokens = ev (betweenVars ++ vars) $ take i tokens ++ betweenToken:remainder
            where (betweenParens, remainder) = scanUntilMatchingParen $ drop (i+1) tokens
                  (betweenToken, betweenVars) = ev vars betweenParens
          evsplit search tokens
              | isInfix search = case elemIndex search tokens of
                  Just i -> Just $ splitInfix i search tokens
                  Nothing -> Nothing
              | not $ isInfix search = case elemIndex search tokens of
                  Just i -> Just $ splitParen i search tokens
                  Nothing -> Nothing
          evsplits [] tokens = Nothing
          evsplits (s:ss) tokens = case evsplit s tokens of
              Just t -> Just t
              Nothing -> evsplits ss tokens
          evsplitss [] tokens = Nothing
          evsplitss (ss:sss) tokens = case evsplits ss tokens of
              Just t -> Just t
              Nothing -> evsplitss sss tokens


evaluator :: String -> (Token, [Assignment])
evaluator "" = (TInteger 0, [])
evaluator ex = ev [] $ map makeToken $ tokenize ex ""


evaluator' :: String -> (Token, [Assignment])
evaluator' "" = (TInteger 0, [])
evaluator' ex = runWriter $ ev' $ map makeToken $ tokenize ex ""
