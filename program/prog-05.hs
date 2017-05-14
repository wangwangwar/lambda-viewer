{-# LANGUAGE FlexibleContexts #-}
-- Program that goes with "Learn You a Lambda, a Haskell Tutorial", Chapter 5.

import           Data.Char
import           Data.Ratio
import           Prelude          hiding (fst, snd)
import           Text.Regex.Posix

-- Datatype definition for lambda expression.
data Term = Var Var | Lam Var Term | App Term Term
data Var  = V String

-- Show instances for Var and Term.
instance Show Var where
  show (V s) = s

instance Show Term where
  show (Var v)   = show v
  show (Lam x e) = "(λ" ++ show x ++ "." ++ show e ++ ")"
  show (App f e) = "(" ++ show f ++ " " ++ show e ++ ")"

-- Read instance for Var.
instance Read Var where
  readsPrec _ = variable

-- Parser for variables that start with an alphabet, followed by a number.
variable = mapP f (alpha &&& digits)
  where f (c, d) = V (c : d)
        alpha = char isAlpha
        digits = many1 (char isDigit)

-- Parser for single character variables.
variable1 = mapP (\c -> V [c]) (char isAlpha)

-- Basic parser combinators.
nil :: ReadS [a]
nil s = [([], s)]

char :: (Char -> Bool) -> ReadS Char
char f (c:s) | f c = [(c, s)]
char f _           = []

mapP :: (a -> b) -> ReadS a -> ReadS b
mapP f g = map (\ (c, s) -> (f c, s)) . g

(&&&) :: ReadS a -> ReadS b -> ReadS (a, b)
f &&& g = \s -> [ ((x, y), s2)
                | (x, s1) <- f s,
                  (y, s2) <- g s1 ]

-- Left biased choice (which is actually not appropriate)
(|||) :: ReadS a -> ReadS b -> ReadS (Either a b)
f ||| g = \s -> case f s of
                  [] -> map right (g s)
                  xs -> map left xs
  where left  (x, s) = (Left  x, s)
        right (y, s) = (Right y, s)

(<|>) :: ReadS a -> ReadS a -> ReadS a
f <|> g = mapP select (f ||| g)
  where select (Left  x) = x
        select (Right y) = y

many, many1 :: ReadS a -> ReadS [a]
many r  = many1 r <|> nil
many1 r = mapP cons (r &&& many r)
  where cons (x, xs) = x : xs

nothing :: ReadS (Maybe a)
nothing s = [(Nothing, s)]

mayb :: ReadS a -> ReadS (Maybe a)
mayb r = mapP Just r <|> nothing

-- 一、用组合的方式设计一个识别器 num :: ReadS Int，从字串中识别十进制的自然数，
-- 正则表达式为 [0-9]+。换句话说，是要实现一个和 Prelude 里定义的 reads :: ReadS Int 等价的函数。
-- 不可以用 read，但是可以用 fromEnum 来把 Char 转为 Int。
num :: ReadS Integer
num = mapP ((foldr (\current sum -> 10 * sum + current) 0) . reverse) ints
  where
    num' :: ReadS [Char]
    num' = many $ char (\x -> [x] =~ "[0-9]+")
    ints :: ReadS [Integer]
    ints = mapP (map ((\x -> x - fromIntegral (fromEnum '0')) . (fromIntegral . fromEnum))) num'

-- 二、用组合的方式设计一个识别器 readR :: ReadS Rational，识别正则表达式
-- [+-][0-9]+(%[0-9]+)? 表示的有理数。换句话说，是要实现一个和 Prelude 里定义的
-- reads :: ReadS Rational 等价的函数。注意 Haskell 里有理数的分子分母之间是用 % 符号隔开。
readR :: ReadS Rational
readR = mapP f readRationalStr
  where
    op ((a, _), _) = a
    numerator ((_, a), _) = a
    denominator (_, Just (_, a)) = Just a
    denominator (_, Nothing) = Nothing
    f :: ((Char, Integer), Maybe (Char, Integer)) -> Rational
    f result = case (denominator result) of
      Just deno -> positiveF (op result) ((numerator result) % deno) :: Rational
      Nothing -> positiveF (op result) (fromInteger (numerator result)) :: Rational
    positiveF :: Char -> Rational -> Rational
    positiveF op rationalNum
      | op == '-' = negate rationalNum
      | otherwise = rationalNum

readRationalStr :: ReadS ((Char, Integer), Maybe (Char, Integer))
readRationalStr = char (\x -> [x] =~ "[+-]") &&& num &&& mayb (char (== '%') &&& num)

-- 三、上一节讲到 ReadS a 的类型可以表达多个识别值，但以上定义的 <|> 和 ||| 的组合方式只能
-- 返回其中一个分支的可能性。应该如何定义 <|> 或者 ||| 让它们能够返回两个分支的所有可能性呢？

main = do
  print $ char (== 'a') "a"
  print $ variable "ab"
  print $ variable "a1"
  print $ many (char (=='a')) "aaaa"
  print $ many (char (=='b')) "aaaa"
  print $ num "39"
  print $ readR "-123"
  print $ readR "+123a"
  print $ readR "123a"
  print $ readR "-123%123"
  print $ readR "-123%77"
